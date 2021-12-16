;;; mastodon-tl.el --- HTTP request/response functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.9.1
;; Homepage: https://github.com/jdenen/mastodon.el
;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;; This file is part of mastodon.el.

;; mastodon.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mastodon.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mastodon.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; mastodon-tl.el provides timeline functions.

;;; Code:

(require 'shr)
(require 'thingatpt) ;; for word-at-point
(require 'time-date)

(autoload 'mastodon-auth--get-account-name "mastodon-auth")
(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-media--get-avatar-rendering "mastodon-media")
(autoload 'mastodon-media--get-media-link-rendering "mastodon-media")
(autoload 'mastodon-media--inline-images "mastodon-media")
(autoload 'mastodon-mode "mastodon")
(autoload 'mastodon-profile--account-from-id "mastodon-profile")
(autoload 'mastodon-profile--make-author-buffer "mastodon-profile")
(autoload 'mastodon-profile--search-account-by-handle "mastodon-profile")
;; mousebot adds
(autoload 'mastodon-profile--toot-json "mastodon-profile")
(autoload 'mastodon-profile--account-field "mastodon-profile")
(autoload 'mastodon-profile--extract-users-handles "mastodon-profile")
(autoload 'mastodon-profile--my-profile "mastodon-profile")
(autoload 'mastodon-toot--delete-toot "mastodon-toot")
(autoload 'mastodon-http--post "mastodon-http")
(autoload 'mastodon-http--triage "mastodon-http")
(autoload 'mastodon-http--get-json-async "mastodon-http")
(autoload 'mastodon-profile--lookup-account-in-status "mastodon-profile")
(autoload 'mastodon-profile-mode "mastodon-profile")
(autoload 'mastodon-notifications--get "mastodon-notifications")
(defvar mastodon-instance-url)
(defvar mastodon-toot-timestamp-format)
(defvar shr-use-fonts)  ;; declare it since Emacs24 didn't have this

(declare-function mapcar* "cl")

(defgroup mastodon-tl nil
  "Timelines in Mastodon."
  :prefix "mastodon-tl-"
  :group 'mastodon)

(defcustom mastodon-tl--enable-relative-timestamps t
  "Whether to show relative (to the current time) timestamps.

This will require periodic updates of a timeline buffer to
keep the timestamps current as time progresses."
  :group 'mastodon-tl
  :type '(boolean :tag "Enable relative timestamps and background updater task"))

(defcustom mastodon-tl--enable-proportional-fonts nil
  "Nonnil to enable using proportional fonts when rendering HTML.

By default fixed width fonts are used."
  :group 'mastodon-tl
  :type '(boolean :tag "Enable using proportional rather than fixed \
width fonts when rendering HTML text"))

(defvar-local mastodon-tl--buffer-spec nil
  "A unique identifier and functions for each Mastodon buffer.")

(defcustom mastodon-tl--show-avatars nil
  "Whether to enable display of user avatars in timelines."
  :group 'mastodon-tl
  :type '(boolean :tag "Whether to display user avatars in timelines"))

;; (defvar mastodon-tl--show-avatars nil
;; (if (version< emacs-version "27.1")
;; (image-type-available-p 'imagemagick)
;; (image-transforms-p))
;; "A boolean value stating whether to show avatars in timelines.")

(defvar-local mastodon-tl--update-point nil
  "When updating a mastodon buffer this is where new toots will be inserted.

If nil `(point-min)' is used instead.")

(defvar mastodon-tl--display-media-p t
  "A boolean value stating whether to show media in timelines.")

(defvar-local mastodon-tl--timestamp-next-update nil
  "The timestamp when the buffer should next be scanned to update the timestamps.")

(defvar-local mastodon-tl--timestamp-update-timer nil
  "The timer that, when set will scan the buffer to update the timestamps.")

(defvar mastodon-tl--link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'mastodon-tl--do-link-action-at-point)
    (define-key map [mouse-2] 'mastodon-tl--do-link-action)
    (define-key map [follow-link] 'mouse-face)
    (keymap-canonicalize map))
  "The keymap for link-like things in buffer (except for shr.el generate links).

This will make the region of text act like like a link with mouse
highlighting, mouse click action tabbing to next/previous link
etc.")

(defvar mastodon-tl--shr-map-replacement
  (let ((map (copy-keymap shr-map)))
    ;; Replace the move to next/previous link bindings with our
    ;; version that knows about more types of links.
    (define-key map [remap shr-next-link] 'mastodon-tl--next-tab-item)
    (define-key map [remap shr-previous-link] 'mastodon-tl--previous-tab-item)
    (keymap-canonicalize map))
  "The keymap to be set for shr.el generated links that are not images.

We need to override the keymap so tabbing will navigate to all
types of mastodon links and not just shr.el-generated ones.")

(defvar mastodon-tl--shr-image-map-replacement
  (let ((map (copy-keymap (if (boundp 'shr-image-map)
                              shr-image-map
                            shr-map))))
    ;; Replace the move to next/previous link bindings with our
    ;; version that knows about more types of links.
    (define-key map [remap shr-next-link] 'mastodon-tl--next-tab-item)
    (define-key map [remap shr-previous-link] 'mastodon-tl--previous-tab-item)
    ;; browse-url loads the preview only, we want browse-image
    ;; on RET to browse full sized image URL
    (define-key map [remap shr-browse-url] 'shr-browse-image)
    ;; remove shr's u binding, as it the maybe-probe-and-copy-url
    ;; is already bound to w also
    (define-key map (kbd "u") 'mastodon-tl--update)
    (keymap-canonicalize map))
  "The keymap to be set for shr.el generated image links.

We need to override the keymap so tabbing will navigate to all
types of mastodon links and not just shr.el-generated ones.")

(defun mastodon-tl--next-tab-item ()
  "Move to the next interesting item.

This could be the next toot, link, or image; whichever comes first.
Don't move if nothing else to move to is found, i.e. near the end of the buffer.
This also skips tab items in invisible text, i.e. hidden spoiler text."
  (interactive)
  (let (next-range
        (search-pos (point)))
    (while (and (setq next-range (mastodon-tl--find-next-or-previous-property-range
                                  'mastodon-tab-stop search-pos nil))

                (get-text-property (car next-range) 'invisible)
                (setq search-pos (1+ (cdr next-range))))
      ;; do nothing, all the action in in the while condition
      )
    (if (null next-range)
        (message "Nothing else here.")
      (goto-char (car next-range))
      (message "%s" (get-text-property (point) 'help-echo)))))

(defun mastodon-tl--previous-tab-item ()
  "Move to the previous interesting item.

This could be the previous toot, link, or image; whichever comes
first. Don't move if nothing else to move to is found, i.e. near
the start of the buffer. This also skips tab items in invisible
text, i.e. hidden spoiler text."
  (interactive)
  (let (next-range
        (search-pos (point)))
    (while (and (setq next-range (mastodon-tl--find-next-or-previous-property-range
                                  'mastodon-tab-stop search-pos t))
                (get-text-property (car next-range) 'invisible)
                (setq search-pos (1- (car next-range))))
      ;; do nothing, all the action in in the while condition
      )
    (if (null next-range)
        (message "Nothing else before this.")
      (goto-char (car next-range))
      (message "%s" (get-text-property (point) 'help-echo)))))

(defun mastodon-tl--get-federated-timeline ()
  "Opens federated timeline."
  (interactive)
  (message "Loading federated timeline...")
  (mastodon-tl--init
   "federated" "timelines/public" 'mastodon-tl--timeline))

(defun mastodon-tl--get-home-timeline ()
  "Opens home timeline."
  (interactive)
  (message "Loading home timeline...")
  (mastodon-tl--init
   "home" "timelines/home" 'mastodon-tl--timeline))

(defun mastodon-tl--get-local-timeline ()
  "Opens local timeline."
  (interactive)
  (message "Loading local timeline...")
  (mastodon-tl--init
   "local" "timelines/public?local=true" 'mastodon-tl--timeline))

(defun mastodon-tl--get-tag-timeline ()
  "Prompts for tag and opens its timeline."
  (interactive)
  (let* ((word (or (word-at-point) ""))
         (input (read-string (format "Load timeline for tag(%s): " word)))
         (tag (if (equal input "") word input)))
    (message "Loading timeline for #%s..." tag)
    (mastodon-tl--show-tag-timeline tag)))

(defun mastodon-tl--show-tag-timeline (tag)
  "Opens a new buffer showing the timeline of posts with hastag TAG."
  (mastodon-tl--init
   (concat "tag-" tag) (concat "timelines/tag/" tag) 'mastodon-tl--timeline))

(defun mastodon-tl--goto-toot-pos (find-pos refresh &optional pos)
  "Search for toot with FIND-POS.
If search returns nil, execute REFRESH function.

Optionally start from POS."
  (let* ((npos (funcall find-pos
                        (or pos (point))
                        'byline
                        (current-buffer))))
    (if npos
        (if (not (get-text-property npos 'toot-id))
            (mastodon-tl--goto-toot-pos find-pos refresh npos)
          (goto-char npos))
      (funcall refresh))))

(defun mastodon-tl--goto-next-toot ()
  "Jump to next toot header."
  (interactive)
  (mastodon-tl--goto-toot-pos 'next-single-property-change
                              'mastodon-tl--more))

(defun mastodon-tl--goto-prev-toot ()
  "Jump to last toot header."
  (interactive)
  (mastodon-tl--goto-toot-pos 'previous-single-property-change
                              'mastodon-tl--update))

(defun mastodon-tl--remove-html (toot)
  "Remove unrendered tags from TOOT."
  (let* ((t1 (replace-regexp-in-string "<\/p>" "\n\n" toot))
         (t2 (replace-regexp-in-string "<\/?span>" "" t1)))
    (replace-regexp-in-string "<span class=\"h-card\">" "" t2)))

(defun mastodon-tl--byline-author (toot)
  "Propertize author of TOOT."
  (let* ((account (alist-get 'account toot))
         (handle (alist-get 'acct account))
         (name (if (not (string= "" (alist-get 'display_name account)))
                   (alist-get 'display_name account)
                 (alist-get 'username account)))
         (profile-url (alist-get 'url account))
         (avatar-url (alist-get 'avatar account)))
    ;; TODO: Once we have a view for a user (e.g. their posts
    ;; timeline) make this a tab-stop and attach an action
    (concat
     (when (and mastodon-tl--show-avatars
                mastodon-tl--display-media-p
                (if (version< emacs-version "27.1")
                    (image-type-available-p 'imagemagick)
                  (image-transforms-p)))
       (mastodon-media--get-avatar-rendering avatar-url))
     (propertize name 'face 'mastodon-display-name-face)
     " ("
     (propertize (concat "@" handle)
                 'face 'mastodon-handle-face
                 'mouse-face 'highlight
		         ;; TODO: Replace url browsing with native profile viewing
		         'mastodon-tab-stop 'user-handle
                 'account account
		         'shr-url profile-url
		         'keymap mastodon-tl--link-keymap
                 'mastodon-handle (concat "@" handle)
		         'help-echo (concat "Browse user profile of @" handle))
     ")")))

(defun mastodon-tl--byline-boosted (toot)
  "Add byline for boosted data from TOOT."
  (let ((reblog (alist-get 'reblog toot)))
    (when reblog
      (concat
       "\n "
       (propertize "Boosted" 'face 'mastodon-boosted-face)
       " "
       (mastodon-tl--byline-author reblog)))))

(defun mastodon-tl--field (field toot)
  "Return FIELD from TOOT.

Return value from boosted content if available."
  (or (alist-get field (alist-get 'reblog toot))
      (alist-get field toot)))

(defun mastodon-tl--relative-time-details (timestamp &optional current-time)
  "Return cons of (descriptive string . next change) for the TIMESTAMP.

Use the optional CURRENT-TIME as the current time (only used for
reliable testing).

The descriptive string is a human readable version relative to
the current time while the next change timestamp give the first
time that this description will change in the future.

TIMESTAMP is assumed to be in the past."
  (let* ((now (or current-time (current-time)))
         (time-difference (time-subtract now timestamp))
         (seconds-difference (float-time time-difference))
         (regular-response
          (lambda (seconds-difference multiplier unit-name)
            (let ((n (floor (+ 0.5 (/ seconds-difference multiplier)))))
              (cons (format "%d %ss ago" n unit-name)
                    (* (+ 0.5 n) multiplier)))))
         (relative-result
          (cond
           ((< seconds-difference 60)
            (cons "less than a minute ago"
                  60))
           ((< seconds-difference (* 1.5 60))
            (cons "one minute ago"
                  90)) ;; at 90 secs
           ((< seconds-difference (* 60 59.5))
            (funcall regular-response seconds-difference 60 "minute"))
           ((< seconds-difference (* 1.5 60 60))
            (cons "one hour ago"
                  (* 60 90))) ;; at 90 minutes
           ((< seconds-difference (* 60 60 23.5))
            (funcall regular-response seconds-difference (* 60 60) "hour"))
           ((< seconds-difference (* 1.5 60 60 24))
            (cons "one day ago"
                  (* 1.5 60 60 24))) ;; at a day and a half
           ((< seconds-difference (* 60 60 24 6.5))
            (funcall regular-response seconds-difference (* 60 60 24) "day"))
           ((< seconds-difference (* 1.5 60 60 24 7))
            (cons "one week ago"
                  (* 1.5 60 60 24 7))) ;; a week and a half
           ((< seconds-difference (* 60 60 24 7 52))
            (if (= 52 (floor (+ 0.5 (/ seconds-difference 60 60 24 7))))
                (cons "52 weeks ago"
                      (* 60 60 24 7 52))
              (funcall regular-response seconds-difference (* 60 60 24 7) "week")))
           ((< seconds-difference (* 1.5 60 60 24 365))
            (cons "one year ago"
                  (* 60 60 24 365 1.5))) ;; a year and a half
           (t
            (funcall regular-response seconds-difference (* 60 60 24 365.25) "year")))))
    (cons (car relative-result)
          (time-add timestamp (seconds-to-time (cdr relative-result))))))

(defun mastodon-tl--relative-time-description (timestamp &optional current-time)
  "Return a string with a human readable TIMESTAMP relative to the current time.

Use the optional CURRENT-TIME as the current time (only used for
reliable testing).

E.g. this could return something like \"1 min ago\", \"yesterday\", etc.
TIME-STAMP is assumed to be in the past."
  (car (mastodon-tl--relative-time-details timestamp current-time)))

(defun mastodon-tl--byline (toot author-byline action-byline)
  "Generate byline for TOOT.

AUTHOR-BYLINE is function for adding the author portion of
the byline that takes one variable.
ACTION-BYLINE is a function for adding an action, such as boosting
favouriting and following to the byline. It also takes a single function.
By default it is `mastodon-tl--byline-boosted'"
  (let ((parsed-time (date-to-time (mastodon-tl--field 'created_at toot)))
        (faved (equal 't (mastodon-tl--field 'favourited toot)))
        (boosted (equal 't (mastodon-tl--field 'reblogged toot)))
        (visibility (mastodon-tl--field 'visibility toot)))
    (concat
     ;; (propertize "\n | " 'face 'default)
     (propertize
      (concat (when boosted
                (format "(%s) "
                        (propertize "B" 'face 'mastodon-boost-fave-face)))
              (when faved
                (format "(%s) "
                        (propertize "F" 'face 'mastodon-boost-fave-face)))
              (funcall author-byline toot)
              (cond ((equal visibility "direct")
                     (if (fontp (char-displayable-p #10r128274))
                         " ✉"
                       " [direct]"))
                    ((equal visibility "private")
                     (if (fontp (char-displayable-p #10r9993))
                         " 🔒"
                       " [followers]")))
              (funcall action-byline toot)
              " "
              ;; TODO: Once we have a view for toot (responses etc.) make
              ;; this a tab stop and attach an action.
              (propertize
               (format-time-string mastodon-toot-timestamp-format parsed-time)
               'timestamp parsed-time
               'display (if mastodon-tl--enable-relative-timestamps
                            (mastodon-tl--relative-time-description parsed-time)
                          parsed-time))
              (propertize "\n  ------------\n" 'face 'default))
      'favourited-p faved
      'boosted-p    boosted
      'byline       t))))

(defun mastodon-tl--render-text (string toot)
  "Return a propertized text rendering the given HTML string STRING.

The contents comes from the given TOOT which is used in parsing
links in the text. If TOOT is nil no parsing occurs."
  (with-temp-buffer
    (insert string)
    (let ((shr-use-fonts mastodon-tl--enable-proportional-fonts)
          (shr-width (when mastodon-tl--enable-proportional-fonts
                       (- (window-width) 1))))
      (shr-render-region (point-min) (point-max)))
    ;; Make all links a tab stop recognized by our own logic, make things point
    ;; to our own logic (e.g. hashtags), and update keymaps where needed:
    (when toot
      (let (region)
        (while (setq region (mastodon-tl--find-property-range
                             'shr-url (or (cdr region) (point-min))))
          (mastodon-tl--process-link toot
                                     (car region) (cdr region)
                                     (get-text-property (car region) 'shr-url)))))
    (buffer-string)))

(defun mastodon-tl--process-link (toot start end url)
  "Process link URL in TOOT as hashtag, userhandle, or normal link.
START and END are the boundaries of the link in the toot."
  (let* (mastodon-tab-stop-type
         keymap
         (help-echo (get-text-property start 'help-echo))
         extra-properties
         (toot-url (mastodon-tl--field 'url toot))
         (toot-url (when toot-url (url-generic-parse-url toot-url)))
         (toot-instance-url (if toot-url
                                (concat (url-type toot-url) "://"
                                        (url-host toot-url))
                              mastodon-instance-url))
         (maybe-hashtag (mastodon-tl--extract-hashtag-from-url
                         url toot-instance-url))
         (url-instance (concat "https://"
                               (url-host (url-generic-parse-url url))))
         (maybe-userhandle (if (string= mastodon-instance-url url-instance)
                                        ; if handle is local, then no instance suffix:
                               (buffer-substring-no-properties start end)
                             (mastodon-tl--extract-userhandle-from-url
                              url (buffer-substring-no-properties start end)))))
    (cond (;; Hashtags:
           maybe-hashtag
           (setq mastodon-tab-stop-type 'hashtag
                 keymap mastodon-tl--link-keymap
                 help-echo (concat "Browse tag #" maybe-hashtag)
                 extra-properties (list 'mastodon-tag maybe-hashtag)))
          (;; User handles:
           maybe-userhandle
           ;; this fails on mentions in profile notes:
           (let ((maybe-userid (mastodon-tl--extract-userid-toot
                                toot maybe-userhandle)))
             (setq mastodon-tab-stop-type 'user-handle
                   keymap mastodon-tl--link-keymap
                   help-echo (concat "Browse user profile of " maybe-userhandle)
                   extra-properties (append
                                     (list 'mastodon-handle maybe-userhandle)
                                     (when maybe-userid
                                       (list 'account-id maybe-userid))))))
          ;; Anything else:
          (t
           ;; Leave it as a url handled by shr.el.
           ;; (We still have to replace the keymap so that tabbing works.)
           (setq keymap (if (eq shr-map (get-text-property start 'keymap))
                            mastodon-tl--shr-map-replacement
                          mastodon-tl--shr-image-map-replacement)
                 mastodon-tab-stop-type 'shr-url)))
    (add-text-properties start end
                         (append
                          (list 'mastodon-tab-stop mastodon-tab-stop-type
                                'keymap keymap
                                'help-echo help-echo)
                          extra-properties))))

(defun mastodon-tl--extract-userid-toot (toot acct)
  "Extract a user id for an ACCT from mentions in a TOOT."
  (let* ((mentions (append (alist-get 'mentions toot) nil))
         (mention (pop mentions))
         (short-acct (substring acct 1 (length acct)))
         return)
    (while mention
      (when (string= (alist-get 'acct mention)
                     short-acct)
        (setq return (alist-get 'id  mention)))
      (setq mention (pop mentions)))
    return))

(defun mastodon-tl--extract-userhandle-from-url (url buffer-text)
  "Return the user hande the URL points to or nil if it is not a profile link.

BUFFER-TEXT is the text covered by the link with URL, for a user profile
this should be of the form <at-sign><user id>, e.g. \"@Gargon\"."
  (let ((parsed-url (url-generic-parse-url url)))
    (when (and (string= "@" (substring buffer-text 0 1))
               (string= (downcase buffer-text)
                        (downcase (substring (url-filename parsed-url) 1))))
      (concat buffer-text "@" (url-host parsed-url)))))

(defun mastodon-tl--extract-hashtag-from-url (url instance-url)
  "Return the hashtag that URL points to or nil if URL is not a tag link.

INSTANCE-URL is the url of the instance for the toot that the link
came from (tag links always point to a page on the instance publishing
the toot)."
  (cond
   ;; Mastodon type tag link:
   ((string-prefix-p (concat instance-url "/tags/") url)
    (substring url (length (concat instance-url "/tags/"))))
   ;; Link from some other ostatus site we've encountered:
   ((string-prefix-p (concat instance-url "/tag/") url)
    (substring url (length (concat instance-url "/tag/"))))
   ;; If nothing matches we assume it is not a hashtag link:
   (t nil)))

(defun mastodon-tl--set-face (string face)
  "Return the propertized STRING with the face property set to FACE."
  (propertize string 'face face))

(defun mastodon-tl--toggle-spoiler-text (position)
  "Toggle the visibility of the spoiler text at/after POSITION."
  (let ((inhibit-read-only t)
        (spoiler-text-region (mastodon-tl--find-property-range
                              'mastodon-content-warning-body position nil)))
    (if (not spoiler-text-region)
        (message "No spoiler text here")
      (add-text-properties (car spoiler-text-region) (cdr spoiler-text-region)
                           (list 'invisible
                                 (not (get-text-property (car spoiler-text-region)
                                                         'invisible)))))))

(defun mastodon-tl--toggle-spoiler-text-in-toot ()
  "Toggle the visibility of the spoiler text in the current toot."
  (interactive)
  (let* ((toot-range (or (mastodon-tl--find-property-range
                          'toot-json (point))
                         (mastodon-tl--find-property-range
                          'toot-json (point) t)))
         (spoiler-range (when toot-range
                          (mastodon-tl--find-property-range
                           'mastodon-content-warning-body
                           (car toot-range)))))
    (cond ((null toot-range)
           (message "No toot here"))
          ((or (null spoiler-range)
               (> (car spoiler-range) (cdr toot-range)))
           (message "No content warning text here"))
          (t
           (mastodon-tl--toggle-spoiler-text (car spoiler-range))))))

(defun mastodon-tl--make-link (string link-type)
  "Return a propertized version of STRING that will act like link.

LINK-TYPE is the type of link to produce."
  (let ((help-text (cond
                    ((eq link-type 'content-warning)
                     "Toggle hidden text")
                    (t
                     (error "Unknown link type %s" link-type)))))
    (propertize
     string
     'mastodon-tab-stop link-type
     'mouse-face 'highlight
     'keymap mastodon-tl--link-keymap
     'help-echo help-text)))

(defun mastodon-tl--do-link-action-at-point (position)
  "Do the action of the link at POSITION.
Used for hitting <return> on a given link."
  (interactive "d")
  (let ((link-type (get-text-property position 'mastodon-tab-stop)))
    (cond ((eq link-type 'content-warning)
           (mastodon-tl--toggle-spoiler-text position))
          ((eq link-type 'hashtag)
           (mastodon-tl--show-tag-timeline (get-text-property position 'mastodon-tag)))
          ;; FIXME: 'account / 'account-id is not set for mentions
          ;; only works for bylines, not mentions
          ((eq link-type 'user-handle)
           (let ((account-json (get-text-property position 'account))
                 (account-id (get-text-property position 'account-id)))
             (cond
              (account-json
               (mastodon-profile--make-author-buffer
                account-json))
              (account-id
               (mastodon-profile--make-author-buffer
                (mastodon-profile--account-from-id account-id)))
              (t
               (mastodon-profile--make-author-buffer
                (mastodon-profile--search-account-by-handle
                 (get-text-property position 'mastodon-handle)))))))
          (t
           (error "Unknown link type %s" link-type)))))

(defun mastodon-tl--do-link-action (event)
  "Do the action of the link at.
Used for a mouse-click EVENT on a link."
  (interactive "e")
  (mastodon-tl--do-link-action-at-point (posn-point (event-end event))))

(defun mastodon-tl--has-spoiler (toot)
  "Check if the given TOOT has a spoiler text.

Spoiler text should initially be shown only while the main
content should be hidden."
  (let ((spoiler (mastodon-tl--field 'spoiler_text toot)))
    (and spoiler (> (length spoiler) 0))))

(defun mastodon-tl--clean-tabs-and-nl (string)
  "Remove tabs and newlines from STRING."
  (replace-regexp-in-string
   "[\t\n ]*\\'" "" string))

(defun mastodon-tl--spoiler (toot)
  "Render TOOT with spoiler message.

This assumes TOOT is a toot with a spoiler message.
The main body gets hidden and only the spoiler text and the
content warning message are displayed. The content warning
message is a link which unhides/hides the main body."
  (let* ((spoiler (mastodon-tl--field 'spoiler_text toot))
         (string (mastodon-tl--set-face
                  ;; remove trailing whitespace
                  (mastodon-tl--clean-tabs-and-nl
                   (mastodon-tl--render-text spoiler toot))
                  'default))
         (message (concat ;"\n"
                   " ---------------\n"
                   " " (mastodon-tl--make-link
                        (concat "CW: " string)
                        'content-warning)
                   "\n"
                   " ---------------\n"))
         (cw (mastodon-tl--set-face message 'mastodon-cw-face)))
    (concat
     cw
     (propertize (mastodon-tl--content toot)
                 'invisible t
                 'mastodon-content-warning-body t))))

(defun mastodon-tl--media (toot)
  "Retrieve a media attachment link for TOOT if one exists."
  (let* ((media-attachements (mastodon-tl--field 'media_attachments toot))
         (media-string (mapconcat
                        (lambda (media-attachement)
                          (let ((preview-url
                                 (alist-get 'preview_url media-attachement))
                                (remote-url
                                 (if (alist-get 'remote_url media-attachement)
                                     (alist-get 'remote_url media-attachement)
                                   ;; fallback b/c notifications don't have remote_url
                                   (alist-get 'url media-attachement)))
                                (type (alist-get 'type media-attachement)))
                            (if mastodon-tl--display-media-p
                                (mastodon-media--get-media-link-rendering
                                 preview-url remote-url type) ; 2nd arg for shr-browse-url
                              (concat "Media::" preview-url "\n"))))
                        media-attachements "")))
    (if (not (and mastodon-tl--display-media-p
                  (equal media-string "")))
        (concat "\n" media-string)
      "")))

(defun mastodon-tl--content (toot)
  "Retrieve text content from TOOT."
  (let* ((content (mastodon-tl--field 'content toot))
         (reblog (alist-get 'reblog toot))
         (poll-p (if reblog
                     (alist-get 'poll reblog)
                   (alist-get 'poll toot))))
    (concat
     (mastodon-tl--render-text content toot)
     (when poll-p
       (mastodon-tl--get-poll toot))
     (mastodon-tl--media toot))))

(defun mastodon-tl--insert-status (toot body author-byline action-byline)
  "Display the content and byline of timeline element TOOT.

BODY will form the section of the toot above the byline.
AUTHOR-BYLINE is an optional function for adding the author
portion of the byline that takes one variable. By default it is
`mastodon-tl--byline-author'
ACTION-BYLINE is also an optional function for adding an action,
such as boosting favouriting and following to the byline. It also
takes a single function. By default it is
`mastodon-tl--byline-boosted'"
  (let ((start-pos (point)))
    (insert
     (propertize
      (concat "\n"
              body
              " \n"
              (mastodon-tl--byline toot author-byline action-byline))
      'toot-id      (alist-get 'id toot)
      'base-toot-id (mastodon-tl--toot-id toot)
      'toot-json    toot)
     "\n")
    (when mastodon-tl--display-media-p
      (mastodon-media--inline-images start-pos (point)))))

(defun mastodon-tl--get-poll (toot)
  "If post TOOT is a poll, return a formatted string of poll."
  (let* ((poll (mastodon-tl--field 'poll toot))
         (options (mastodon-tl--field 'options poll))
         (option-titles (mapcar (lambda (x)
                                  (alist-get 'title x))
                                options))
         (longest-option (car (sort option-titles
                                    (lambda (x y)
                                      (> (length x)
                                         (length y))))))
         (option-counter 0))
    (concat "\nPoll: \n\n"
            (mapconcat (lambda (option)
                         (progn
                           (format "Option %s: %s%s [%s votes].\n"
                                   (setq option-counter (1+ option-counter))
                                   (alist-get 'title option)
                                   (make-string
                                    (1+
                                     (- (length longest-option)
                                        (length (alist-get 'title
                                                           option))))
                                    ?\ )
                                   (alist-get 'votes_count option))))
                       options
                       "\n")
            "\n")))

(defun mastodon-tl--poll-vote (option)
  "If there is a poll at point, prompt user for OPTION to vote on it."
  (interactive
   (list
    (let* ((toot (mastodon-tl--property 'toot-json))
           (reblog (alist-get 'reblog toot))
           (poll (or (alist-get 'poll reblog)
                     (mastodon-tl--field 'poll toot)))
           (options (mastodon-tl--field 'options poll))
           (options-titles (mapcar (lambda (x)
                                     (alist-get 'title x))
                                   options))
           (options-number-seq (number-sequence 1 (length options)))
           (options-numbers (mapcar (lambda(x)
                                      (number-to-string x))
                                    options-number-seq))
           (options-alist (mapcar* 'cons options-numbers options-titles))
           ;; we display both option number and the option title
           ;; but also store both as cons cell as cdr, as we need it below
           (candidates (mapcar (lambda (cell)
                                 (cons (format "%s | %s" (car cell) (cdr cell))
                                       cell))
                               options-alist)))
      (if (null (mastodon-tl--field 'poll (mastodon-tl--property 'toot-json)))
          (message "No poll here.")
        ;; var "option" = just the cdr, a cons of option number and desc
        (cdr (assoc
              (completing-read "Poll option to vote for: "
                               candidates
                               nil ; (predicate)
                               t) ; require match
              candidates))))))
  (if (null (mastodon-tl--field 'poll (mastodon-tl--property 'toot-json)))
      (message "No poll here.")
    (let* ((toot (mastodon-tl--property 'toot-json))
           (poll (mastodon-tl--field 'poll toot))
           (poll-id (alist-get 'id poll))
           (url (mastodon-http--api (format "polls/%s/votes" poll-id)))
           ;; need to zero-index our option:
           (option-as-arg (number-to-string (1- (string-to-number (car option)))))
           ;; (option-indexed
           (arg `(("choices[]" . ,option-as-arg)))
           (response (mastodon-http--post url arg nil)))
      (mastodon-http--triage response
                             (lambda ()
                               (message "You voted for option %s: %s!"
                                        (car option) (cdr option)))))))

(defun mastodon-tl--toot (toot)
  "Formats TOOT and insertes it into the buffer."
  (mastodon-tl--insert-status
   toot
   (mastodon-tl--clean-tabs-and-nl
    (if (mastodon-tl--has-spoiler toot)
        (mastodon-tl--spoiler toot)
      (mastodon-tl--content toot)))
   'mastodon-tl--byline-author
   'mastodon-tl--byline-boosted))

(defun mastodon-tl--timeline (toots)
  "Display each toot in TOOTS."
  (mapc 'mastodon-tl--toot toots)
  (goto-char (point-min)))

(defun mastodon-tl--get-update-function (&optional buffer)
  "Get the UPDATE-FUNCTION stored in `mastodon-tl--buffer-spec'.
Optionally get it for BUFFER."
  (mastodon-tl--get-buffer-property 'update-function buffer))

(defun mastodon-tl--get-endpoint (&optional buffer)
  "Get the ENDPOINT stored in `mastodon-tl--buffer-spec'.
Optionally set it for BUFFER."
  (mastodon-tl--get-buffer-property 'endpoint buffer))

(defun mastodon-tl--buffer-name (&optional buffer)
  "Get the BUFFER-NAME stored in `mastodon-tl--buffer-spec'.
Optionally get it for BUFFER."
  (mastodon-tl--get-buffer-property 'buffer-name buffer ))

(defun mastodon-tl--get-buffer-property (property &optional buffer)
  "Get PROPERTY from `mastodon-tl--buffer-spec' in BUFFER or `current-buffer'."
  (with-current-buffer  (or buffer (current-buffer))
    (if (plist-get mastodon-tl--buffer-spec property)
        (plist-get mastodon-tl--buffer-spec property)
      (error "Mastodon-tl--buffer-spec is not defined for buffer %s"
             (or buffer (current-buffer))))))

(defun mastodon-tl--more-json (endpoint id)
  "Return JSON for timeline ENDPOINT before ID."
  (let* ((url (mastodon-http--api (concat
                                   endpoint
                                   (if (string-match-p "?" endpoint)
                                       "&"
                                     "?")
                                   "max_id="
                                   (mastodon-tl--as-string id)))))
    (mastodon-http--get-json url)))

(defun mastodon-tl--more-json-async (endpoint id callback &rest cbargs)
  "Return JSON for timeline ENDPOINT before ID.
Then run CALLBACK with arguments CBARGS."
  (let* ((url (mastodon-http--api (concat
                                   endpoint
                                   (if (string-match-p "?" endpoint)
                                       "&"
                                     "?")
                                   "max_id="
                                   (mastodon-tl--as-string id)))))
    (apply 'mastodon-http--get-json-async url callback cbargs)))

;; TODO
;; Look into the JSON returned here by Local
(defun mastodon-tl--updated-json (endpoint id)
  "Return JSON for timeline ENDPOINT since ID."
  (let ((url (mastodon-http--api (concat
                                  endpoint
                                  (if (string-match-p "?" endpoint)
                                      "&"
                                    "?")
                                  "since_id="
                                  (mastodon-tl--as-string id)))))
    (mastodon-http--get-json url)))

(defun mastodon-tl--property (prop &optional backward)
  "Get property PROP for toot at point.

Move forward (down) the timeline unless BACKWARD is non-nil."
  (or (get-text-property (point) prop)
      (save-excursion
        (if backward
            (mastodon-tl--goto-prev-toot)
          (mastodon-tl--goto-next-toot))
        (get-text-property (point) prop))))

(defun mastodon-tl--newest-id ()
  "Return toot-id from the top of the buffer."
  (save-excursion
    (goto-char (point-min))
    (mastodon-tl--property 'toot-id)))

(defun mastodon-tl--oldest-id ()
  "Return toot-id from the bottom of the buffer."
  (save-excursion
    (goto-char (point-max))
    (mastodon-tl--property 'toot-id t)))

(defun mastodon-tl--as-string (numeric)
  "Convert NUMERIC to string."
  (cond ((numberp numeric)
         (number-to-string numeric))
        ((stringp numeric) numeric)
        (t (error
            "Numeric:%s must be either a string or a number"
            numeric))))

(defun mastodon-tl--toot-id (json)
  "Find approproiate toot id in JSON.

If the toot has been boosted use the id found in the
reblog portion of the toot.  Otherwise, use the body of
the toot.  This is the same behaviour as the mastodon.social
webapp"
  (let ((id (alist-get 'id json))
        (reblog (alist-get 'reblog json)))
    (if reblog (alist-get 'id reblog) id)))


(defun mastodon-tl--thread ()
  "Open thread buffer for toot under `point'."
  (interactive)
  (let* ((id (mastodon-tl--as-string (mastodon-tl--toot-id
                                      (mastodon-tl--property 'toot-json))))
         (url (mastodon-http--api (format "statuses/%s/context" id)))
         (buffer (format "*mastodon-thread-%s*" id))
         (toot (mastodon-tl--property 'toot-json))
         (context (mastodon-http--get-json url)))
    (when (member (alist-get 'type toot) '("reblog" "favourite"))
      (setq toot (alist-get 'status toot)))
    (if (> (+ (length (alist-get 'ancestors context))
              (length (alist-get 'descendants context)))
           0)
        (with-output-to-temp-buffer buffer
          (switch-to-buffer buffer)
          (mastodon-mode)
          (setq mastodon-tl--buffer-spec
                `(buffer-name ,buffer
                              endpoint ,(format "statuses/%s/context" id)
                              update-function
                              (lambda(toot) (message "END of thread."))))
          (let ((inhibit-read-only t))
            (mastodon-tl--timeline (vconcat
                                    (alist-get 'ancestors context)
                                    `(,toot)
                                    (alist-get 'descendants context)))))
      (message "No Thread!"))))

(defun mastodon-tl--follow-user (user-handle &optional notify)
  "Query for USER-HANDLE from current status and follow that user.
If NOTIFY is \"true\", enable notifications when that user posts.
If NOTIFY is \"false\", disable notifications when that user posts.
This can be called to toggle NOTIFY on users already being followed."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "follow")))
  (mastodon-tl--do-user-action-and-response user-handle "follow" nil notify))

(defun mastodon-tl--enable-notify-user-posts (user-handle)
  "Query for USER-HANDLE and enable notifications when they post."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "enable")))
  (mastodon-tl--follow-user user-handle "true"))

(defun mastodon-tl--disable-notify-user-posts (user-handle)
  "Query for USER-HANDLE and disable notifications when they post."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "disable")))
  (mastodon-tl--follow-user user-handle "false"))

(defun mastodon-tl--unfollow-user (user-handle)
  "Query for USER-HANDLE from current status and unfollow that user."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "unfollow")))
  (mastodon-tl--do-user-action-and-response user-handle "unfollow" t))

(defun mastodon-tl--block-user (user-handle)
  "Query for USER-HANDLE from current status and block that user."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "block")))
  (mastodon-tl--do-user-action-and-response user-handle "block"))

(defun mastodon-tl--unblock-user (user-handle)
  "Query for USER-HANDLE from list of blocked users and unblock that user."
  (interactive
   (list
    (mastodon-tl--interactive-blocks-or-mutes-list-get "unblock")))
  (if (not user-handle)
      (message "Looks like you have no blocks to unblock!")
    (mastodon-tl--do-user-action-and-response user-handle "unblock" t)))

(defun mastodon-tl--mute-user (user-handle)
  "Query for USER-HANDLE from current status and mute that user."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "mute")))
  (mastodon-tl--do-user-action-and-response user-handle "mute"))

(defun mastodon-tl--unmute-user (user-handle)
  "Query for USER-HANDLE from list of muted users and unmute that user."
  (interactive
   (list
    (mastodon-tl--interactive-blocks-or-mutes-list-get "unmute")))
  (if (not user-handle)
      (message "Looks like you have no mutes to unmute!")
    (mastodon-tl--do-user-action-and-response user-handle "unmute" t)))

(defun mastodon-tl--interactive-user-handles-get (action)
  "Get the list of user-handles for ACTION from the current toot."
  (let ((user-handles (mastodon-profile--extract-users-handles
                       (mastodon-profile--toot-json))))
    (completing-read (if (or (equal action "disable")
                             (equal action "enable"))
                         (format "%s notifications when user posts: " action)
                       (format "Handle of user to %s: " action))
                     user-handles
                     nil ; predicate
                     'confirm)))

(defun mastodon-tl--interactive-blocks-or-mutes-list-get (action)
  "Fetch the list of accounts for ACTION from the server.
Action must be either \"unblock\" or \"mute\"."
  (let* ((endpoint (cond ((equal action "unblock")
                          "blocks")
                         ((equal action "unmute")
                          "mutes")))
         (url (mastodon-http--api endpoint))
         (json (mastodon-http--get-json url))
         (accts (mapcar (lambda (user)
                          (alist-get 'acct user))
                        json)))
    (when accts
      (completing-read (format "Handle of user to %s: " action)
                       accts
                       nil ; predicate
                       t))))

(defun mastodon-tl--do-user-action-and-response (user-handle action &optional negp notify)
  "Do ACTION on user NAME/USER-HANDLE.
NEGP is whether the action involves un-doing something.
If NOTIFY is \"true\", enable notifications when that user posts.
If NOTIFY is \"false\", disable notifications when that user posts.
NOTIFY is only non-nil when called by `mastodon-tl--follow-user'."
  (let* ((account (if negp
                      ;; TODO check if both are actually needed
                      (mastodon-profile--search-account-by-handle
                       user-handle)
                    (mastodon-profile--lookup-account-in-status
                     user-handle (mastodon-profile--toot-json))))
         (user-id (mastodon-profile--account-field account 'id))
         (name (mastodon-profile--account-field account 'display_name))
         (url (mastodon-http--api
               (if notify
                   (format "accounts/%s/%s?notify=%s" user-id action notify)
                 (format "accounts/%s/%s" user-id action)))))
    (if account
        (if (equal action "follow") ; y-or-n for all but follow
            (mastodon-tl--do-user-action-function url name user-handle action notify)
          (when (y-or-n-p (format "%s user %s? " action name))
            (mastodon-tl--do-user-action-function url name user-handle action)))
      (message "Cannot find a user with handle %S" user-handle))))

(defun mastodon-tl--do-user-action-function (url name user-handle action &optional notify)
  "Post ACTION on user NAME/USER-HANDLE to URL."
  (let ((response (mastodon-http--post url nil nil)))
    (mastodon-http--triage response
                           (lambda ()
                             (cond ((string-equal notify "true")
                                    (message "Receiving notifications for user %s (@%s)!"
                                             name user-handle))
                                   ((string-equal notify "false")
                                    (message "Not receiving notifications for user %s (@%s)!"
                                             name user-handle))
                                   ((eq notify nil)
                                    (message "User %s (@%s) %sed!" name user-handle action)))))))

;; TODO: add this to new posts in some cases, e.g. in thread view.
(defun mastodon-tl--reload-timeline-or-profile ()
  "Reload the current timeline or profile page.
For use after e.g. deleting a toot."
  (cond ((equal (mastodon-tl--get-endpoint) "timelines/home")
         (mastodon-tl--get-home-timeline))
        ((equal (mastodon-tl--get-endpoint) "timelines/public")
         (mastodon-tl--get-federated-timeline))
        ((equal (mastodon-tl--get-endpoint) "timelines/public?local=true")
         (mastodon-tl--get-local-timeline))
        ((equal (mastodon-tl--get-endpoint) "notifications")
         (mastodon-notifications--get))
        ((equal (mastodon-tl--buffer-name)
                (concat "*mastodon-" (mastodon-auth--get-account-name) "-statuses*"))
         (mastodon-profile--my-profile))))

(defun mastodon-tl--more ()
  "Append older toots to timeline, asynchronously."
  (interactive)
  (mastodon-tl--more-json-async (mastodon-tl--get-endpoint) (mastodon-tl--oldest-id)
                                'mastodon-tl--more* (current-buffer) (point)))

(defun mastodon-tl--more* (json buffer point-before)
  "Append older toots to timeline, asynchronously.
Runs the timeline's update function on JSON, in BUFFER.
When done, places point at POINT-BEFORE."
  (with-current-buffer buffer
    (when json
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (funcall (mastodon-tl--get-update-function) json)
        (goto-char point-before)))))

(defun mastodon-tl--find-property-range (property start-point &optional search-backwards)
  "Return `nil` if no such range is found.

If PROPERTY is set at START-POINT returns a range around
START-POINT otherwise before/after START-POINT.

SEARCH-BACKWARDS determines whether we pick point
before (non-nil) or after (nil)"
  (if (get-text-property start-point property)
      ;; We are within a range, so look backwards for the start:
      (cons (previous-single-property-change
             (if (equal start-point (point-max)) start-point (1+ start-point))
             property nil (point-min))
            (next-single-property-change start-point property nil (point-max)))
    (if search-backwards
        (let* ((end (or (previous-single-property-change
                         (if (equal start-point (point-max))
                             start-point (1+ start-point))
                         property)
                        ;; we may either be just before the range or there
                        ;; is nothing at all
                        (and (not (equal start-point (point-min)))
                             (get-text-property (1- start-point) property)
                             start-point)))
               (start (and
                       end
                       (previous-single-property-change end property nil (point-min)))))
          (when end
            (cons start end)))
      (let* ((start (next-single-property-change start-point property))
             (end (and start
                       (next-single-property-change start property nil (point-max)))))
        (when start
          (cons start end))))))

(defun mastodon-tl--find-next-or-previous-property-range
    (property start-point search-backwards)
  "Find (start . end) property range after/before START-POINT.

Does so while PROPERTY is set to a consistent value (different
from the value at START-POINT if that is set).

Return nil if no such range exists.

If SEARCH-BACKWARDS is non-nil it find a region before
START-POINT otherwise after START-POINT."
  (if (get-text-property start-point property)
      ;; We are within a range, we need to start the search from
      ;; before/after this range:
      (let ((current-range (mastodon-tl--find-property-range property start-point)))
        (if search-backwards
            (unless (equal (car current-range) (point-min))
              (mastodon-tl--find-property-range
               property (1- (car current-range)) search-backwards))
          (unless (equal (cdr current-range) (point-max))
            (mastodon-tl--find-property-range
             property (1+ (cdr current-range)) search-backwards))))
    ;; If we are not within a range, we can just defer to
    ;; mastodon-tl--find-property-range directly.
    (mastodon-tl--find-property-range property start-point search-backwards)))

(defun mastodon-tl--consider-timestamp-for-updates (timestamp)
  "Take note that TIMESTAMP is used in buffer and ajust timers as needed.

This calculates the next time the text for TIMESTAMP will change
and may adjust existing or future timer runs should that time
before current plans to run the update function.

The adjustment is only made if it is significantly (a few
seconds) before the currently scheduled time. This helps reduce
the number of occasions where we schedule an update only to
schedule the next one on completion to be within a few seconds.

If relative timestamps are
disabled (`mastodon-tl--enable-relative-timestamps` is nil) this
is a no-op."
  (when mastodon-tl--enable-relative-timestamps
    (let ((this-update (cdr (mastodon-tl--relative-time-details timestamp))))
      (when (time-less-p this-update
                         (time-subtract mastodon-tl--timestamp-next-update
                                        (seconds-to-time 10)))
        (setq mastodon-tl--timestamp-next-update this-update)
        (when mastodon-tl--timestamp-update-timer
          ;; We need to re-schedule for an earlier time
          (cancel-timer mastodon-tl--timestamp-update-timer)
          (setq mastodon-tl--timestamp-update-timer
                (run-at-time this-update
                             nil ;; don't repeat
                             #'mastodon-tl--update-timestamps-callback
                             (current-buffer) nil)))))))

(defun mastodon-tl--update-timestamps-callback (buffer previous-marker)
  "Update the next few timestamp displays in BUFFER.

Start searching for more timestamps from PREVIOUS-MARKER or
from the start if it is nil."
  ;; only do things if the buffer hasn't been killed in the meantime
  (when (and mastodon-tl--enable-relative-timestamps ;; should be true but just in case...
             (buffer-live-p buffer))
    (save-excursion
      (with-current-buffer buffer
        (let ((previous-timestamp (if previous-marker
                                      (marker-position previous-marker)
                                    (point-min)))
              (iteration 0)
              next-timestamp-range)
          (if previous-marker
              ;; This is a follow-up call to process the next batch of
              ;; timestamps.
              ;; Release the marker to not slow things down.
              (set-marker previous-marker nil)
            ;; Otherwise this is a rew run, so let's initialize the next-run time.
            (setq mastodon-tl--timestamp-next-update (time-add (current-time)
                                                               (seconds-to-time 300))
                  mastodon-tl--timestamp-update-timer nil))
          (while (and (< iteration 5)
                      (setq next-timestamp-range
                            (mastodon-tl--find-property-range 'timestamp
                                                              previous-timestamp)))
            (let* ((start (car next-timestamp-range))
                   (end (cdr next-timestamp-range))
                   (timestamp (get-text-property start 'timestamp))
                   (current-display (get-text-property start 'display))
                   (new-display (mastodon-tl--relative-time-description timestamp)))
              (unless (string= current-display new-display)
                (let ((inhibit-read-only t))
                  (add-text-properties
                   start end (list 'display
                                   (mastodon-tl--relative-time-description timestamp)))))
              (mastodon-tl--consider-timestamp-for-updates timestamp)
              (setq iteration (1+ iteration)
                    previous-timestamp (1+ (cdr next-timestamp-range)))))
          (if next-timestamp-range
              ;; schedule the next batch from the previous location to
              ;; start very soon in the future:
              (run-at-time 0.1 nil #'mastodon-tl--update-timestamps-callback buffer
                           (copy-marker previous-timestamp))
            ;; otherwise we are done for now; schedule a new run for when needed
            (setq mastodon-tl--timestamp-update-timer
                  (run-at-time mastodon-tl--timestamp-next-update
                               nil ;; don't repeat
                               #'mastodon-tl--update-timestamps-callback
                               buffer nil))))))))

(defun mastodon-tl--update ()
  "Update timeline with new toots."
  (interactive)
  (let* ((endpoint (mastodon-tl--get-endpoint))
         (update-function (mastodon-tl--get-update-function))
         (id (mastodon-tl--newest-id))
         (json (mastodon-tl--updated-json endpoint id)))
    (when json
      (let ((inhibit-read-only t))
        (goto-char (or mastodon-tl--update-point (point-min)))
        (funcall update-function json)))))

(defun mastodon-tl--init (buffer-name endpoint update-function)
  "Initialize BUFFER-NAME with timeline targeted by ENDPOINT asynchronously.

UPDATE-FUNCTION is used to recieve more toots."
  (let ((url (mastodon-http--api endpoint))
        (buffer (concat "*mastodon-" buffer-name "*")))
    (mastodon-http--get-json-async
     url 'mastodon-tl--init* buffer endpoint update-function)))

(defun mastodon-tl--init* (json buffer endpoint update-function)
  "Initialize BUFFER with timeline targeted by ENDPOINT.

UPDATE-FUNCTION is used to recieve more toots.
JSON is the data returned from the server."
  (with-output-to-temp-buffer buffer
    (switch-to-buffer buffer)
    (setq
     ;; Initialize with a minimal interval; we re-scan at least once
     ;; every 5 minutes to catch any timestamps we may have missed
     mastodon-tl--timestamp-next-update (time-add (current-time)
                                                  (seconds-to-time 300)))
    (funcall update-function json))
  (mastodon-mode)
  (when (equal endpoint "follow_requests")
    (mastodon-profile-mode))
  (with-current-buffer buffer
    (setq mastodon-tl--buffer-spec
          `(buffer-name ,buffer
                        endpoint ,endpoint
                        update-function ,update-function)
          mastodon-tl--timestamp-update-timer
          (when mastodon-tl--enable-relative-timestamps
            (run-at-time mastodon-tl--timestamp-next-update
                         nil ;; don't repeat
                         #'mastodon-tl--update-timestamps-callback
                         (current-buffer)
                         nil)))))

(defun mastodon-tl--init-sync (buffer-name endpoint update-function)
  "Initialize BUFFER-NAME with timeline targeted by ENDPOINT.

UPDATE-FUNCTION is used to receive more toots.
Runs synchronously."
  (let* ((url (mastodon-http--api endpoint))
         (buffer (concat "*mastodon-" buffer-name "*"))
         (json (mastodon-http--get-json url)))
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      (setq
       ;; Initialize with a minimal interval; we re-scan at least once
       ;; every 5 minutes to catch any timestamps we may have missed
       mastodon-tl--timestamp-next-update (time-add (current-time)
                                                    (seconds-to-time 300)))
      (funcall update-function json))
    (mastodon-mode)
    (with-current-buffer buffer
      (setq mastodon-tl--buffer-spec
            `(buffer-name ,buffer-name
                          endpoint ,endpoint update-function
                          ,update-function)
            mastodon-tl--timestamp-update-timer
            (when mastodon-tl--enable-relative-timestamps
              (run-at-time mastodon-tl--timestamp-next-update
                           nil ;; don't repeat
                           #'mastodon-tl--update-timestamps-callback
                           (current-buffer)
                           nil))))
    buffer))

(provide 'mastodon-tl)
;;; mastodon-tl.el ends here
