;;; mastodon-toot.el --- Minor mode for sending Mastodon toots  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Maintainer: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 0.10.0
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://git.blast.noho.st/mouse/mastodon.el

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

;; mastodon-toot.el supports POSTing status data to Mastodon.

;;; Code:


(when (require 'emojify nil :noerror)
  (declare-function emojify-insert-emoji "emojify")
  (declare-function emojify-set-emoji-data "emojify")
  (defvar emojify-emojis-dir)
  (defvar emojify-user-emojis))

(require 'cl-lib)

(when (require 'company nil :noerror)
  (declare-function company-mode-on "company")
  (declare-function company-begin-backend "company")
  (declare-function company-grab-symbol "company")
  (defvar company-backends))

(defvar mastodon-instance-url)
(defvar mastodon-tl--buffer-spec)
(autoload 'mastodon-auth--user-acct "mastodon-auth")
(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--delete "mastodon-http")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-http--get-json-async "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")
(autoload 'mastodon-http--post-media-attachment "mastodon-http")
(autoload 'mastodon-http--process-json "mastodon-http")
(autoload 'mastodon-http--read-file-as-string "mastodon-http")
(autoload 'mastodon-http--triage "mastodon-http")
(autoload 'mastodon-search--search-accounts-query "mastodon-search")
(autoload 'mastodon-tl--as-string "mastodon-tl")
(autoload 'mastodon-tl--clean-tabs-and-nl "mastodon-tl")
(autoload 'mastodon-tl--field "mastodon-tl")
(autoload 'mastodon-tl--find-property-range "mastodon-tl")
(autoload 'mastodon-tl--find-property-range "mastodon-tl")
(autoload 'mastodon-tl--goto-next-toot "mastodon-tl")
(autoload 'mastodon-tl--property "mastodon-tl")
(autoload 'mastodon-tl--reload-timeline-or-profile "mastodon-tl")
(autoload 'mastodon-tl--toot-id "mastodon-tl")
(autoload 'mastodon-toot "mastodon")

(defgroup mastodon-toot nil
  "Tooting in Mastodon."
  :prefix "mastodon-toot-"
  :group 'mastodon)

(defcustom mastodon-toot--default-visibility "public"
  "The default visibility for new toots.

Must be one of \"public\", \"unlisted\", \"private\" (for
followers-only), or \"direct\"."
  :group 'mastodon-toot
  :type '(choice
          (const :tag "public" "public")
          (const :tag "unlisted" "unlisted")
          (const :tag "followers only" "private")
          (const :tag "direct" "direct")))

(defcustom mastodon-toot--default-media-directory "~/"
  "The default directory when prompting for a media file to upload."
  :group 'mastodon-toot
  :type 'string)

(defcustom mastodon-toot--attachment-height 80
  "Height of the attached images preview in the toot draft buffer."
  :group 'mastodon-toot
  :type 'integer)

(defcustom mastodon-toot--enable-completion-for-mentions
  (if (require 'company nil :noerror) "following" "off")
  "Whether to enable company completion for mentions.

Used for completion in toot compose buffer.

This is only used if company mode is installed."
  :group 'mastodon-toot
  :type '(choice
          (const :tag "off" nil)
          (const :tag "following only" "following")
          (const :tag "all users" "all")))

(defcustom mastodon-toot--enable-custom-instance-emoji nil
  "Whether to enable your instance's custom emoji by default."
  :group 'mastodon-toot
  :type 'boolean)

(defvar-local mastodon-toot--content-warning nil
  "A flag whether the toot should be marked with a content warning.")

(defvar-local mastodon-toot--content-warning-from-reply-or-redraft nil
  "The content warning of the toot being replied to.")

(defvar-local mastodon-toot--content-nsfw nil
  "A flag indicating whether the toot should be marked as NSFW.")

(defvar-local mastodon-toot--visibility "public"
  "A string indicating the visibility of the toot being composed.

Valid values are \"direct\", \"private\" (followers-only),
\"unlisted\", and \"public\".")

(defvar-local mastodon-toot--media-attachments nil
  "A list of the media attachments of the toot being composed.")

(defvar-local mastodon-toot--media-attachment-ids nil
  "A list of any media attachment ids of the toot being composed.")

(defvar-local mastodon-toot--reply-to-id nil
  "Buffer-local variable to hold the id of the toot being replied to.")

(defvar mastodon-toot--max-toot-chars nil
  "The maximum allowed characters count for a single toot.")

(defvar mastodon-toot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mastodon-toot--send)
    (define-key map (kbd "C-c C-k") #'mastodon-toot--cancel)
    (define-key map (kbd "C-c C-w") #'mastodon-toot--toggle-warning)
    (define-key map (kbd "C-c C-n") #'mastodon-toot--toggle-nsfw)
    (define-key map (kbd "C-c C-v") #'mastodon-toot--change-visibility)
    (when (require 'emojify nil :noerror)
      (define-key map (kbd "C-c C-e") #'mastodon-toot--insert-emoji))
    (define-key map (kbd "C-c C-a") #'mastodon-toot--attach-media)
    (define-key map (kbd "C-c !") #'mastodon-toot--clear-all-attachments)
    map)
  "Keymap for `mastodon-toot'.")

(defun mastodon-toot--get-max-toot-chars ()
  "Fetch max_toot_chars from `mastodon-instance-url' asynchronously."
  (mastodon-http--get-json-async
   (mastodon-http--api "instance") 'mastodon-toot--get-max-toot-chars-callback))

(defun mastodon-toot--get-max-toot-chars-callback (json-response)
  "Set max_toot_chars returned in JSON-RESPONSE and display in new toot buffer."
  (let ((max-chars
         (or
          (alist-get 'max_toot_chars json-response)
          ;; some servers have this instead:
          (alist-get 'max_characters
                     (alist-get 'statuses
                                (alist-get 'configuration
                                           json-response))))))
  (setq mastodon-toot--max-toot-chars
        (number-to-string max-chars))
  (with-current-buffer "*new toot*"
    (mastodon-toot--update-status-fields))))

(defun mastodon-toot--action-success (marker byline-region remove)
  "Insert/remove the text MARKER with 'success face in byline.

BYLINE-REGION is a cons of start and end pos of the byline to be
modified.
Remove MARKER if REMOVE is non-nil, otherwise add it."
  (let ((inhibit-read-only t)
        (bol (car byline-region))
        (eol (cdr byline-region)))
    (save-excursion
      (when remove
        (goto-char bol)
        (beginning-of-line) ;; The marker is not part of the byline
        (if (search-forward (format "(%s) " marker) eol t)
            (replace-match "")
          (message "Oops: could not find marker '(%s)'" marker)))
      (unless remove
        (goto-char bol)
        (insert (format "(%s) "
                        (propertize marker 'face 'success)))))))

(defun mastodon-toot--action (action callback)
  "Take ACTION on toot at point, then execute CALLBACK.
Makes a POST request to the server."
  (let* ((id (mastodon-tl--property 'base-toot-id))
         (url (mastodon-http--api (concat "statuses/"
                                          (mastodon-tl--as-string id)
                                          "/"
                                          action))))
    (let ((response (mastodon-http--post url nil nil)))
      (mastodon-http--triage response callback))))

(defun mastodon-toot--toggle-boost ()
  "Boost/unboost toot at `point'."
  (interactive)
  (let* ((has-id (mastodon-tl--property 'base-toot-id))
         (byline-region (when has-id
                          (mastodon-tl--find-property-range 'byline (point))))
         (id (when byline-region
               (mastodon-tl--as-string (mastodon-tl--property 'base-toot-id))))
         (boosted (when byline-region
                    (get-text-property (car byline-region) 'boosted-p)))
         (action (if boosted "unreblog" "reblog"))
         (msg (if boosted "unboosted" "boosted"))
         (remove (when boosted t)))
    (if byline-region
        (mastodon-toot--action action
                               (lambda ()
                                 (let ((inhibit-read-only t))
                                   (add-text-properties (car byline-region)
                                                        (cdr byline-region)
                                                        (list 'boosted-p
                                                              (not boosted)))
                                   (mastodon-toot--action-success
                                    "B" byline-region remove))
                                 (message (format "%s #%s" msg id))))
      (message "Nothing to boost here?!?"))))

(defun mastodon-toot--toggle-favourite ()
  "Favourite/unfavourite toot at `point'."
  (interactive)
  (let* ((has-id (mastodon-tl--property 'base-toot-id))
         (byline-region (when has-id
                          (mastodon-tl--find-property-range 'byline (point))))
         (id (when byline-region
               (mastodon-tl--as-string (mastodon-tl--property 'base-toot-id))))
         (faved (when byline-region
                  (get-text-property (car byline-region) 'favourited-p)))
         (action (if faved "unfavourite" "favourite"))
         (remove (when faved t)))
    (if byline-region
        (mastodon-toot--action action
                               (lambda ()
                                 (let ((inhibit-read-only t))
                                   (add-text-properties (car byline-region)
                                                        (cdr byline-region)
                                                        (list 'favourited-p
                                                              (not faved)))
                                   (mastodon-toot--action-success
                                    "F" byline-region remove))
                                 (message (format "%s #%s" action id))))
      (message "Nothing to favorite here?!?"))))

(defun mastodon-toot--copy-toot-url ()
  "Copy URL of toot at point."
  (interactive)
  (let* ((toot (mastodon-tl--property 'toot-json))
         (url (if (mastodon-tl--field 'reblog toot)
                  (alist-get 'url (alist-get 'reblog toot))
                (alist-get 'url toot))))
    (kill-new url)
    (message "Toot URL copied to the clipboard.")))

(defun mastodon-toot--own-toot-p (toot)
  "Check if TOOT is user's own, e.g. for deleting it."
  (and (not (alist-get 'reblog toot))
       (equal (alist-get 'acct (alist-get 'account toot))
              (mastodon-auth--user-acct))))

(defun mastodon-toot--pin-toot-toggle ()
  "Pin or unpin user's toot at point."
  (interactive)
  (let* ((toot (mastodon-tl--property 'toot-json))
         (pinnable-p (mastodon-toot--own-toot-p toot))
         (pinned-p (equal (alist-get 'pinned toot) t))
         (action (if pinned-p "unpin" "pin"))
         (msg (if pinned-p "unpinned" "pinned"))
         (msg-y-or-n (if pinned-p "Unpin" "Pin")))
    (if (not pinnable-p)
        (message "You can only pin your own toots.")
      (if (y-or-n-p (format "%s this toot? " msg-y-or-n))
          (mastodon-toot--action action
                                 (lambda ()
                                   (when mastodon-tl--buffer-spec
                                     (mastodon-tl--reload-timeline-or-profile))
                                   (message "Toot %s!" msg)))))))

(defun mastodon-toot--delete-toot ()
  "Delete user's toot at point synchronously."
  (interactive)
  (mastodon-toot--delete-and-redraft-toot t))

;; TODO: handle media/poll for redrafting toots
(defun mastodon-toot--delete-and-redraft-toot (&optional no-redraft)
  "Delete and redraft user's toot at point synchronously.
NO-REDRAFT means delete toot only."
  (interactive)
  (let* ((toot (mastodon-tl--property 'toot-json))
         (id (mastodon-tl--as-string (mastodon-tl--toot-id toot)))
         (url (mastodon-http--api (format "statuses/%s" id)))
         (toot-cw (alist-get 'spoiler_text toot))
         (toot-visibility (alist-get 'visibility toot))
         (reply-id (alist-get 'in_reply_to_id toot)))
    (if (not (mastodon-toot--own-toot-p toot))
        (message "You can only delete (and redraft) your own toots.")
      (if (y-or-n-p (if no-redraft
                        (format "Delete this toot? ")
                      (format "Delete and redraft this toot? ")))
          (let* ((response (mastodon-http--delete url)))
            (mastodon-http--triage
             response
             (lambda ()
               (if no-redraft
                   (progn
                     (when mastodon-tl--buffer-spec
                            (mastodon-tl--reload-timeline-or-profile))
                     (message "Toot deleted!"))
                 (mastodon-toot--redraft response
                                         reply-id
                                         toot-visibility
                                         toot-cw)))))))))

(defun mastodon-toot--redraft (response &optional reply-id toot-visibility toot-cw)
  "Opens a new toot compose buffer using values from RESPONSE buffer.
REPLY-ID, TOOT-VISIBILITY, and TOOT-CW of deleted toot are preseved."
  (with-current-buffer response
    (let* ((json-response (mastodon-http--process-json))
           (content (alist-get 'text json-response)))
      (mastodon-toot--compose-buffer nil nil)
      (goto-char (point-max))
      (insert content)
      ;; adopt reply-to-id, visibility and CW from deleted toot:
      (when reply-id
        (setq mastodon-toot--reply-to-id reply-id))
      (setq mastodon-toot--visibility toot-visibility)
      (when (not (equal toot-cw ""))
        (setq mastodon-toot--content-warning t)
        (setq mastodon-toot--content-warning-from-reply-or-redraft toot-cw))
      (mastodon-toot--update-status-fields))))

(defun mastodon-toot--bookmark-toot-toggle ()
  "Bookmark or unbookmark toot at point synchronously."
  (interactive)
  (let* ((toot (mastodon-tl--property 'toot-json))
         (id (mastodon-tl--as-string (mastodon-tl--toot-id toot)))
         (bookmarked (alist-get 'bookmarked toot))
         (url (mastodon-http--api (if (equal bookmarked t)
                                      (format "statuses/%s/unbookmark" id)
                                    (format "statuses/%s/bookmark" id))))
         (prompt (if (equal bookmarked t)
                     (format "Toot already bookmarked. Remove? ")
                   (format "Bookmark this toot? ")))
         (message (if (equal bookmarked t)
                      "Bookmark removed!"
                    "Toot bookmarked!")))
    (when (y-or-n-p prompt)
      (let ((response (mastodon-http--post url nil nil)))
        (mastodon-http--triage response
                               (lambda ()
                                 (message message)))))))

(defun mastodon-toot--kill ()
  "Kill `mastodon-toot-mode' buffer and window."
  (kill-buffer-and-window))

(defun mastodon-toot--cancel ()
  "Kill new-toot buffer/window. Does not POST content to Mastodon."
  (interactive)
  (let* ((toot (mastodon-toot--remove-docs))
         (empty-toot-p (and (not mastodon-toot--media-attachments)
                            (string= "" (mastodon-tl--clean-tabs-and-nl toot)))))
    (if empty-toot-p
        (mastodon-toot--kill)
      (when (y-or-n-p "Discard draft toot? ")
        (mastodon-toot--kill)))))

(defalias 'mastodon-toot--insert-emoji
  'emojify-insert-emoji
  "Prompt to insert an emoji.")

(defun mastodon-toot--download-custom-emoji ()
  "Download `mastodon-instance-url's custom emoji.
Emoji images are stored in a subdir of `emojify-emojis-dir'.
To use the downloaded emoji, run `mastodon-toot--enable-custom-emoji'."
  (interactive)
  (let ((custom-emoji (mastodon-http--get-json
                       (mastodon-http--api "custom_emojis")))
        (mastodon-custom-emoji-dir (file-name-as-directory
                                    (concat (file-name-as-directory
                                             (expand-file-name
                                              emojify-emojis-dir))
                                            "mastodon-custom-emojis"))))
    (if (not (file-directory-p emojify-emojis-dir))
        (message "Looks like you need to set up emojify first.")
      (unless (file-directory-p mastodon-custom-emoji-dir)
        (make-directory mastodon-custom-emoji-dir nil)) ; no add parent
      (mapc (lambda (x)
              (url-copy-file (alist-get 'url x)
                             (concat
                              mastodon-custom-emoji-dir
                              (alist-get 'shortcode x)
                              "."
                              (file-name-extension (alist-get 'url x)))
                             t))
            custom-emoji)
      (message "Custom emoji for %s downloaded to %s"
               mastodon-instance-url
               mastodon-custom-emoji-dir))))

(defun mastodon-toot--collect-custom-emoji ()
  "Return a list of `mastodon-instance-url's custom emoji.
The list is formatted for `emojify-user-emojis', which see."
  (let* ((mastodon-custom-emojis-dir (concat (expand-file-name
                                              emojify-emojis-dir)
                                             "/mastodon-custom-emojis/"))
         (custom-emoji-files (directory-files mastodon-custom-emojis-dir
                                              nil ; not full path
                                              "^[^.]")) ; no dot files
         (mastodon-emojify-user-emojis))
    (mapc (lambda (x)
            (push
             `(,(concat ":"
                        (file-name-base x)
                        ":") . (("name" . ,(file-name-base x))
                        ("image" . ,(concat mastodon-custom-emojis-dir x))
                        ("style" . "github")))
             mastodon-emojify-user-emojis))
          custom-emoji-files)
    (reverse mastodon-emojify-user-emojis)))

(defun mastodon-toot--enable-custom-emoji ()
  "Add `mastodon-instance-url's custom emoji to `emojify'.
Custom emoji must first be downloaded with
`mastodon-toot--download-custom-emoji'. Custom emoji are appended
to `emojify-user-emojis', and the emoji data is updated."
  (interactive)
  (unless (file-exists-p (concat (expand-file-name
                                  emojify-emojis-dir)
                                 "/mastodon-custom-emojis/"))
    (when (y-or-n-p "Looks like you haven't downloaded your instance's custom emoji yet. Download now? ")
      (mastodon-toot--download-custom-emoji)))
  (setq emojify-user-emojis
        (append (mastodon-toot--collect-custom-emoji)
                emojify-user-emojis))
  ;; if already loaded, reload
  (when (featurep 'emojify)
    (emojify-set-emoji-data)))


(defun mastodon-toot--remove-docs ()
  "Get the body of a toot from the current compose buffer."
  (let ((header-region (mastodon-tl--find-property-range 'toot-post-header
                                                         (point-min))))
    (buffer-substring (cdr header-region) (point-max))))

(defun mastodon-toot--set-visibility (visibility)
  "Set the visiblity of the next toot to VISIBILITY."
  (interactive
   (list (completing-read "Visiblity: " '("public"
                                          "unlisted"
                                          "private"
                                          "direct"))))
  (setq mastodon-toot--visibility visibility)
  (message "Visibility set to %s" visibility))

(defun mastodon-toot--send ()
  "POST contents of new-toot buffer to Mastodon instance and kill buffer.
If media items have been attached and uploaded with
`mastodon-toot--attach-media', they are attached to the toot."
  (interactive)
  (let* ((toot (mastodon-toot--remove-docs))
         (empty-toot-p (and (not mastodon-toot--media-attachments)
                            (string= "" (mastodon-tl--clean-tabs-and-nl toot))))
         (endpoint (mastodon-http--api "statuses"))
         (spoiler (when (and (not empty-toot-p)
                             mastodon-toot--content-warning)
                    (read-string "Warning: " mastodon-toot--content-warning-from-reply-or-redraft)))
         (args-no-media `(("status" . ,toot)
                          ("in_reply_to_id" . ,mastodon-toot--reply-to-id)
                          ("visibility" . ,mastodon-toot--visibility)
                          ("sensitive" . ,(when mastodon-toot--content-nsfw
                                            (symbol-name t)))
                          ("spoiler_text" . ,spoiler)))
         (args-media (when mastodon-toot--media-attachments
                           (mapcar (lambda (id)
                                     (cons "media_ids[]" id))
                                   mastodon-toot--media-attachment-ids)))
         (args (append args-media args-no-media)))
    (cond ((and mastodon-toot--media-attachments
                ;; make sure we have media args
                ;; and the same num of ids as attachments
                (or (not args-media)
                    (not (= (length mastodon-toot--media-attachments)
                            (length mastodon-toot--media-attachment-ids)))))
           (message "Something is wrong with your uploads. Wait for them to complete or try again."))
          ((and mastodon-toot--max-toot-chars
                (> (length toot) (string-to-number mastodon-toot--max-toot-chars)))
           (message "Looks like your toot is longer than that maximum allowed length."))
          (empty-toot-p
           (message "Empty toot. Cowardly refusing to post this."))
          (t
           (let ((response (mastodon-http--post endpoint args nil)))
             (mastodon-http--triage response
                                    (lambda ()
                                      (mastodon-toot--kill)
                                      (message "Toot toot!"))))))))

(defun mastodon-toot--process-local (acct)
  "Add domain to local ACCT and replace the curent user name with \"\".

Mastodon requires the full user@domain, even in the case of local accts.
eg. \"user\" -> \"user@local.social \" (when local.social is the domain of the
mastodon-instance-url).
eg. \"yourusername\" -> \"\"
eg. \"feduser@fed.social\" -> \"feduser@fed.social\"."
  (cond ((string-match-p "@" acct) (concat "@" acct " ")) ; federated acct
        ((string= (mastodon-auth--user-acct) acct) "") ; your acct
        (t (concat "@" acct "@" ; local acct
                   (cadr (split-string mastodon-instance-url "/" t)) " "))))

(defun mastodon-toot--mentions (status)
  "Extract mentions from STATUS and process them into a string."
  (interactive)
  (let* ((boosted (mastodon-tl--field 'reblog status))
         (mentions
          (if boosted
              (alist-get 'mentions (alist-get 'reblog status))
            (alist-get 'mentions status))))
    (mapconcat (lambda(x) (mastodon-toot--process-local
                           (alist-get 'acct x)))
               ;; reverse does not work on vectors in 24.5
               (reverse (append mentions nil))
               "")))

(defun mastodon-toot--mentions-company-meta (candidate)
  "Format company completion CANDIDATE's meta field."
  (format " %s"
          (get-text-property 0 'meta candidate)))

(defun mastodon-toot--mentions-company-annotation (candidate)
  "Format company completion CANDIDATE's annotation."
  (format " %s" (get-text-property 0 'annot candidate)))

(defun mastodon-toot--mentions-company-candidates (prefix)
  "Given a company PREFIX query, build a list of candidates.
The prefix can match against both user handles and display names."
  (let ((prefix (substring prefix 1)) ;remove @ for search
        (res))
    (dolist (item (mastodon-search--search-accounts-query prefix))
      (when (or (string-prefix-p prefix (substring (cadr item) 1) t)
                (string-prefix-p prefix (car item) t))
        (push (mastodon-toot--mentions-company-make-candidate item) res)))
    res))

(defun mastodon-toot--mentions-company-make-candidate (candidate)
  "Construct a company completion CANDIDATE for display."
  (let ((display-name (car candidate))
        (handle (cadr candidate))
        (url (caddr candidate)))
    (propertize handle 'annot display-name 'meta url)))

(defun mastodon-toot-mentions (command &optional arg &rest ignored)
  "A company completion backend for toot mentions.
COMMAND is either prefix, to fetch a prefix query, candidates, to
build a list of candidates with query ARG, annotation, to format
an annotation for candidate ARG, or meta, to format meta info for
candidate ARG. IGNORED remains a mystery."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'mastodon-toot-mentions))
    (prefix (when (and (bound-and-true-p mastodon-toot-mode) ; if masto toot minor mode
                       (save-excursion
                         (forward-whitespace -1)
                         (forward-whitespace 1)
                         (looking-at "@")))
              ;; @ + thing before point
              (concat "@" (company-grab-symbol))))
    (candidates (mastodon-toot--mentions-company-candidates arg))
    (annotation (mastodon-toot--mentions-company-annotation arg))
    (meta (mastodon-toot--mentions-company-meta arg))))

(defun mastodon-toot--reply ()
  "Reply to toot at `point'."
  (interactive)
  (let* ((toot (mastodon-tl--property 'toot-json))
         (id (mastodon-tl--as-string (mastodon-tl--field 'id toot)))
         (account (mastodon-tl--field 'account toot))
         (user (alist-get 'acct account))
         (mentions (mastodon-toot--mentions toot))
         (boosted (mastodon-tl--field 'reblog toot))
         (booster (when boosted
                    (alist-get 'acct
                               (alist-get 'account toot)))))
    (mastodon-toot (when user
                     (if booster
                         (if (and
                              (not (equal user booster))
                              (not (string-match booster mentions)))
                             (concat (mastodon-toot--process-local user)
                                     ;; "@" booster " "
                                     (mastodon-toot--process-local booster) mentions)
                           (concat (mastodon-toot--process-local user)
                                   mentions))
                       (concat (mastodon-toot--process-local user)
                               mentions)))
                   id toot)))

(defun mastodon-toot--toggle-warning ()
  "Toggle `mastodon-toot--content-warning'."
  (interactive)
  (setq mastodon-toot--content-warning
        (not mastodon-toot--content-warning))
  (mastodon-toot--update-status-fields))

(defun mastodon-toot--toggle-nsfw ()
  "Toggle `mastodon-toot--content-nsfw'."
  (interactive)
  (setq mastodon-toot--content-nsfw
        (not mastodon-toot--content-nsfw))
  (message "NSFW flag is now %s" (if mastodon-toot--content-nsfw "on" "off"))
  (mastodon-toot--update-status-fields))

(defun mastodon-toot--change-visibility ()
  "Change the current visibility to the next valid value."
  (interactive)
  (setq mastodon-toot--visibility
        (cond ((string= mastodon-toot--visibility "public")
               "unlisted")
              ((string= mastodon-toot--visibility "unlisted")
               "private")
              ((string= mastodon-toot--visibility "private")
               "direct")
              (t
               "public")))
  (mastodon-toot--update-status-fields))

(defun mastodon-toot--clear-all-attachments ()
  "Remove all attachments from a toot draft."
  (interactive)
  (setq mastodon-toot--media-attachments nil)
  (setq mastodon-toot--media-attachment-ids nil)
  (mastodon-toot--refresh-attachments-display)
  (mastodon-toot--update-status-fields))

(defun mastodon-toot--attach-media (file content-type description)
  "Prompt for an attachment FILE of CONTENT-TYPE with DESCRIPTION.
A preview is displayed in the new toot buffer, and the file
is uploaded asynchronously using `mastodon-toot--upload-attached-media'.
File is actually attached to the toot upon posting."
  (interactive "fFilename: \nsContent type: \nsDescription: ")
  (when (>= (length mastodon-toot--media-attachments) 4)
    ;; Only a max. of 4 attachments are allowed, so pop the oldest one.
    (pop mastodon-toot--media-attachments))
  (if (file-directory-p file)
      (message "Looks like you chose a directory not a file.")
    (setq mastodon-toot--media-attachments
          (nconc mastodon-toot--media-attachments
                 `(((:contents . ,(mastodon-http--read-file-as-string file))
                    (:content-type . ,content-type)
                    (:description . ,description)
                    (:filename . ,file)))))
    (mastodon-toot--refresh-attachments-display)
    ;; upload only most recent attachment:
    (mastodon-toot--upload-attached-media (car (last mastodon-toot--media-attachments)))))

(defun mastodon-toot--upload-attached-media (attachment)
  "Upload a single ATTACHMENT using `mastodon-http--post-media-attachment'.
The item's id is added to `mastodon-toot--media-attachment-ids',
which is used to attach it to a toot when posting."
  (let* ((filename (expand-file-name
                    (alist-get :filename attachment)))
         (caption (alist-get :description attachment))
         (url (concat mastodon-instance-url "/api/v2/media")))
    (message "Uploading %s..." (file-name-nondirectory filename))
    (mastodon-http--post-media-attachment url filename caption)))

(defun mastodon-toot--refresh-attachments-display ()
  "Update the display attachment previews in toot draft buffer."
  (let ((inhibit-read-only t)
        (attachments-region (mastodon-tl--find-property-range
                             'toot-attachments (point-min)))
        (display-specs (mastodon-toot--format-attachments)))
    (dotimes (i (- (cdr attachments-region) (car attachments-region)))
      (add-text-properties (+ (car attachments-region) i)
                           (+ (car attachments-region) i 1)
                           (list 'display (or (nth i display-specs) ""))))))

(defun mastodon-toot--format-attachments ()
  "Format the attachment previews for display in toot draft buffer."
  (or (let ((counter 0)
            (image-options (when (or (image-type-available-p 'imagemagick)
                                     (image-transforms-p))
                             `(:height ,mastodon-toot--attachment-height))))
        (mapcan (lambda (attachment)
                  (let* ((data (alist-get :contents attachment))
                         (image (apply #'create-image data
                                       (if (version< emacs-version "27.1")
                                           (when image-options 'imagemagick)
                                         nil) ; inbuilt scaling in 27.1
                                       t image-options))
                         (type (alist-get :content-type attachment))
                         (description (alist-get :description attachment)))
                    (setq counter (1+ counter))
                    (list (format "\n    %d: " counter)
                          image
                          (format " \"%s\" (%s)" description type))))
                mastodon-toot--media-attachments))
      (list "None")))

;; we'll need to revisit this if the binds get
;; more diverse than two-chord bindings
(defun mastodon-toot--get-mode-kbinds ()
  "Get a list of the keybindings in the mastodon-toot-mode."
  (let* ((binds (copy-tree mastodon-toot-mode-map))
         (prefix (car (cadr binds)))
         (bindings (remove nil (mapcar (lambda (i) (if (listp i) i))
                                       (cadr binds)))))
    (mapcar (lambda (b)
              (setf (car b) (vector prefix (car b)))
              b)
            bindings)))

(defun mastodon-toot--format-kbind-command (cmd)
  "Format CMD to be more readable.
e.g. mastodon-toot--send -> Send."
  (let* ((str (symbol-name cmd))
         (re "--\\(.*\\)$")
         (str2 (save-match-data
                 (string-match re str)
                 (match-string 1 str))))
    (capitalize (replace-regexp-in-string "-" " " str2))))

(defun mastodon-toot--format-kbind (kbind)
  "Format a single keybinding, KBIND, for display in documentation."
  (let ((key (help-key-description (car kbind) nil))
        (command (mastodon-toot--format-kbind-command (cdr kbind))))
    (format "    %s - %s" key command)))

(defun mastodon-toot--format-kbinds (kbinds)
  "Format a list of keybindings, KBINDS, for display in documentation."
  (mapcar #'mastodon-toot--format-kbind kbinds))

(defvar-local mastodon-toot--kbinds-pairs nil
  "Contains a list of paired toot compose buffer keybindings for inserting.")

(defun mastodon-toot--formatted-kbinds-pairs (kbinds-list longest)
  "Return a list of strings each containing two formatted kbinds.
KBINDS-LIST is the list of formatted bindings to pair.
LONGEST is the length of the longest binding."
  (when kbinds-list
    (push (concat "\n"
                  (car kbinds-list)
                  (make-string (- (1+ longest) (length (car kbinds-list)))
                               ?\ )
                  (cadr kbinds-list))
          mastodon-toot--kbinds-pairs)
    (mastodon-toot--formatted-kbinds-pairs (cddr kbinds-list) longest))
  (reverse mastodon-toot--kbinds-pairs))

(defun mastodon-toot--formatted-kbinds-longest (kbinds-list)
  "Return the length of the longest item in KBINDS-LIST."
  (let ((lengths (mapcar (lambda (x)
                           (length x))
                         kbinds-list)))
    (car (sort lengths #'>))))

(defun mastodon-toot--make-mode-docs ()
  "Create formatted documentation text for the mastodon-toot-mode."
  (let* ((kbinds (mastodon-toot--get-mode-kbinds))
         (longest-kbind
          (mastodon-toot--formatted-kbinds-longest
           (mastodon-toot--format-kbinds kbinds))))
    (concat
     " Compose a new toot here. The following keybindings are available:"
     ;; (mastodon-toot--format-kbinds kbinds))))
     (mapconcat 'identity
                (mastodon-toot--formatted-kbinds-pairs
                 (mastodon-toot--format-kbinds kbinds)
                 longest-kbind)
                nil))))

(defun mastodon-toot--display-docs-and-status-fields ()
  "Insert propertized text with documentation about `mastodon-toot-mode'.
Also includes and the status fields which will get updated based
on the status of NSFW, content warning flags, media attachments, etc."
  (let ((divider
         "|=================================================================|"))
    (insert
     (propertize
      (concat
       divider "\n"
       (mastodon-toot--make-mode-docs) "\n"
       ;; divider "\n"
       ;; "\n"
       divider "\n"
       " "
       (propertize "Count"
                   'toot-post-counter t)
       " ⋅ "
       (propertize "Visibility"
                   'toot-post-visibility t)
       " ⋅ "
       (propertize "CW"
                   'toot-post-cw-flag t)
       " "
       (propertize "NSFW"
                   'toot-post-nsfw-flag t)
       "\n"
       " Attachments: "
       (propertize "None                  " 'toot-attachments t)
       "\n"
       divider
       (propertize "\n"
                   'rear-nonsticky t))
      'face 'font-lock-comment-face
      'read-only "Edit your message below."
      'toot-post-header t))))

(defun mastodon-toot--setup-as-reply (reply-to-user reply-to-id reply-json)
  "If REPLY-TO-USER is provided, inject their handle into the message.
If REPLY-TO-ID is provided, set `mastodon-toot--reply-to-id'.
REPLY-JSON is the full JSON of the toot being replied to."
  (let ((reply-visibility (alist-get 'visibility reply-json))
        (reply-cw (alist-get 'spoiler_text reply-json)))
    (when reply-to-user
      (insert (format "%s " reply-to-user))
      (setq mastodon-toot--reply-to-id reply-to-id)
      (if (not (equal mastodon-toot--visibility
                      reply-visibility))
          (setq mastodon-toot--visibility reply-visibility))
      (when (not (equal reply-cw ""))
        (setq mastodon-toot--content-warning t)
        (setq mastodon-toot--content-warning-from-reply-or-redraft reply-cw)))))

(defun mastodon-toot--update-status-fields (&rest _args)
  "Update the status fields in the header based on the current state."
  (ignore-errors  ;; called from after-change-functions so let's not leak errors
    (let ((inhibit-read-only t)
          (header-region (mastodon-tl--find-property-range 'toot-post-header
                                                           (point-min)))
          (count-region (mastodon-tl--find-property-range 'toot-post-counter
                                                          (point-min)))
          (visibility-region (mastodon-tl--find-property-range
                              'toot-post-visibility (point-min)))
          (nsfw-region (mastodon-tl--find-property-range 'toot-post-nsfw-flag
                                                         (point-min)))
          (cw-region (mastodon-tl--find-property-range 'toot-post-cw-flag
                                                       (point-min))))
      (add-text-properties (car count-region) (cdr count-region)
                           (list 'display
                                 (format "%s/%s characters"
                                         (- (point-max) (cdr header-region))
                                         mastodon-toot--max-toot-chars)))
      (add-text-properties (car visibility-region) (cdr visibility-region)
                           (list 'display
                                 (format "Visibility: %s"
                                         (if (equal
                                              mastodon-toot--visibility
                                              "private")
                                             "followers-only"
                                           mastodon-toot--visibility))))
      (add-text-properties (car nsfw-region) (cdr nsfw-region)
                           (list 'display (if mastodon-toot--content-nsfw
                                              (if mastodon-toot--media-attachments
                                                  "NSFW" "NSFW (no effect until attachments added)")
                                            "")
                                 'face 'mastodon-cw-face))
      (add-text-properties (car cw-region) (cdr cw-region)
                           (list 'invisible (not mastodon-toot--content-warning)
                                 'face 'mastodon-cw-face)))))

(defun mastodon-toot--compose-buffer (reply-to-user reply-to-id &optional reply-json)
  "Create a new buffer to capture text for a new toot.
If REPLY-TO-USER is provided, inject their handle into the message.
If REPLY-TO-ID is provided, set the `mastodon-toot--reply-to-id' var.
REPLY-JSON is the full JSON of the toot being replied to."
  (let* ((buffer-exists (get-buffer "*new toot*"))
         (buffer (or buffer-exists (get-buffer-create "*new toot*")))
         (inhibit-read-only t))
    (switch-to-buffer-other-window buffer)
    (text-mode)
    (mastodon-toot-mode t)
    (when (not buffer-exists)
      (mastodon-toot--display-docs-and-status-fields)
      (mastodon-toot--setup-as-reply reply-to-user reply-to-id reply-json))
    (unless mastodon-toot--max-toot-chars
      (mastodon-toot--get-max-toot-chars))
    (when (require 'company nil :noerror)
      (when mastodon-toot--enable-completion-for-mentions
        (set (make-local-variable 'company-backends)
             (add-to-list 'company-backends 'mastodon-toot-mentions))
        (company-mode-on)))
    (make-local-variable 'after-change-functions)
    (push #'mastodon-toot--update-status-fields after-change-functions)
    (mastodon-toot--refresh-attachments-display)
    (mastodon-toot--update-status-fields)))

(define-minor-mode mastodon-toot-mode
  "Minor mode to capture Mastodon toots."
  :group 'mastodon-toot
  :keymap mastodon-toot-mode-map
  :global nil)

(provide 'mastodon-toot)
;;; mastodon-toot.el ends here
