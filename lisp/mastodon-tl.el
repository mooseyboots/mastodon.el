;;; mastodon-tl.el --- HTTP request/response functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.7.1
;; Homepage: https://github.com/jdenen/mastodon.el
;; Package-Requires: ((emacs "24.4"))

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

(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-media--get-avatar-rendering "mastodon-media")
(autoload 'mastodon-media--get-media-link-rendering "mastodon-media")
(autoload 'mastodon-media--inline-images "mastodon-media")
(autoload 'mastodon-mode "mastodon")
(defvar mastodon-toot-timestamp-format)

(defgroup mastodon-tl nil
  "Timelines in Mastodon."
  :prefix "mastodon-tl-"
  :group 'mastodon)

(defcustom mastodon-tl--enable-relative-timestamps t
  "Nonnil to enable showing relative (to the current time) timestamps.

This will require periodic updates of a timeline buffer to 
keep the timestamps current as time progresses."
  :group 'mastodon-tl
  :type '(boolean :tag "Enable relative timestamps and background updater task"))

(defvar mastodon-tl--buffer-spec nil
  "A unique identifier and functions for each Mastodon buffer.")
(make-variable-buffer-local 'mastodon-tl--buffer-spec)

(defvar mastodon-tl--show-avatars-p
  (image-type-available-p 'imagemagick)
  "A boolean value stating whether to show avatars in timelines.")

(defvar mastodon-tl--timestamp-next-update nil
  "The timestamp when the buffer should next be scanned to update the timestamps.")
(make-variable-buffer-local 'mastodon-tl--timestamp-next-update)

(defvar mastodon-tl--timestamp-update-timer nil
  "The timer that, when set will scan the buffer to update the timestamps.")
(make-variable-buffer-local 'mastodon-tl--timestamp-update-timer)


(defun mastodon-tl--get-federated-timeline ()
  "Opens federated timeline."
  (interactive)
  (mastodon-tl--init
   "federated" "timelines/public" 'mastodon-tl--timeline))

(defun mastodon-tl--get-home-timeline ()
  "Opens home timeline."
  (interactive)
  (mastodon-tl--init
   "home" "timelines/home" 'mastodon-tl--timeline))

(defun mastodon-tl--get-local-timeline ()
  "Opens local timeline."
  (interactive)
  (mastodon-tl--init
   "local" "timelines/public?local=true" 'mastodon-tl--timeline))

(defun mastodon-tl--get-tag-timeline ()
  "Prompts for tag and opens its timeline."
  (interactive)
  (let* ((word (or (word-at-point) ""))
         (input (read-string (format "Tag(%s): " word)))
         (tag (if (equal input "") word input)))
    (mastodon-tl--init
     (concat "tag-" tag) (concat "timelines/tag/" tag) 'mastodon-tl--timeline)))

(defun mastodon-tl--goto-toot-pos (find-pos refresh &optional pos)
  "Search for toot with FIND-POS.
If search returns nil, execute REFRESH function.

Optionally start from POS."
  (let* ((npos (funcall find-pos
                        (or pos (point))
                        'toot-id
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
  (let* ((account (cdr (assoc 'account toot)))
         (handle (cdr (assoc 'acct account)))
         (name (cdr (assoc 'display_name account)))
         (avatar-url (cdr (assoc 'avatar account))))
    (concat
     (when mastodon-tl--show-avatars-p
       (mastodon-media--get-avatar-rendering avatar-url))
     (propertize name 'face 'mastodon-display-name-face)
     (propertize (concat " (@"
                         handle
                         ")")
                 'face 'mastodon-handle-face))))

(defun mastodon-tl--byline-boosted (toot)
  "Add byline for boosted data from TOOT."
  (let ((reblog (cdr (assoc 'reblog toot))))
    (when reblog
      (concat
       " "
       (propertize "Boosted" 'face 'mastodon-boosted-face)
       " "
       (mastodon-tl--byline-author reblog)))))

(defun mastodon-tl--field (field toot)
  "Return FIELD from TOOT.

Return value from boosted content if available."
  (or (cdr (assoc field (cdr (assoc 'reblog toot))))
      (cdr (assoc field toot))))

(defun mastodon-tl--relative-time-details (timestamp &optional current-time)
  "Returns cons of (descriptive string . next change) for the TIMESTAMP.

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
  "Returns a string with a human readable description of TIMESTMAP relative to the current time.

Use the optional CURRENT-TIME as the current time (only used for
reliable testing).

E.g. this could return something like \"1 min ago\", \"yesterday\", etc.
TIME-STAMP is assumed to be in the past."
  (car (mastodon-tl--relative-time-details timestamp current-time)))

(defun mastodon-tl--byline (toot)
  "Generate byline for TOOT."
  (let ((id (cdr (assoc 'id toot)))
        (parsed-time (date-to-time (mastodon-tl--field 'created_at toot)))
        (faved (mastodon-tl--field 'favourited toot))
        (boosted (mastodon-tl--field 'reblogged toot)))
    (propertize
     (concat (propertize "\n | " 'face 'default)
             (when boosted
               (format "(%s) "
                       (propertize "B" 'face 'mastodon-boost-fave-face)))
             (when faved
               (format "(%s) "
                       (propertize "F" 'face 'mastodon-boost-fave-face)))
             (mastodon-tl--byline-author toot)
             (mastodon-tl--byline-boosted toot)
             " "
             (propertize
              (format-time-string mastodon-toot-timestamp-format parsed-time)
              'timestamp parsed-time
              'display (if mastodon-tl--enable-relative-timestamps
                           (mastodon-tl--relative-time-description parsed-time)
                         parsed-time))
             (propertize "\n  ------------" 'face 'default))
     'favourited-p faved
     'boosted-p    boosted
     'toot-id      id
     'toot-json    toot)))

(defun mastodon-tl--set-face (string face render)
  "Set the face of a string. If `render' is not nil
also render the html"
  (propertize
   (with-temp-buffer
     (insert string)
     (when render
       (let ((shr-use-fonts nil))
         (shr-render-region (point-min) (point-max))))
     (buffer-string))
   'face face))

(defun mastodon-tl--spoiler (toot)
  "Retrieve spoiler message from TOOT."
  (let* ((spoiler (mastodon-tl--field 'spoiler_text toot))
         (string (mastodon-tl--set-face spoiler 'default t))
         (message (concat "\n ---------------"
                          "\n Content Warning"
                          "\n ---------------\n"))
         (cw (mastodon-tl--set-face message 'mastodon-cw-face nil)))
    (if (> (length string) 0)
        (replace-regexp-in-string "\n\n\n ---------------"
                                  "\n ---------------" (concat string cw))
      "")))

(defun mastodon-tl--media (toot)
  "Retrieve a media attachment link for TOOT if one exists."
  (let* ((media-attachements (mastodon-tl--field 'media_attachments toot))
         (media-string (mapconcat
                        (lambda (media-attachement)
                          (let ((preview-url
                                 (cdr (assoc 'preview_url media-attachement))))
                            (mastodon-media--get-media-link-rendering
                             preview-url)))
                        media-attachements "")))
    (if (not (equal media-string ""))
        (concat "\n" media-string ) "")))


(defun mastodon-tl--content (toot)
  "Retrieve text content from TOOT."
  (let ((content (mastodon-tl--field 'content toot))
        (shr-use-fonts nil))
    (propertize (with-temp-buffer
                  (insert content)
                  (shr-render-region (point-min) (point-max))
                  (buffer-string))
                'face 'default)))

(defun mastodon-tl--toot (toot)
  "Display TOOT content and byline."
  (insert
   (concat
    (mastodon-tl--spoiler toot)
    ;; remove two trailing newlines
    (substring (mastodon-tl--content toot) 0 -2)
    (mastodon-tl--media toot)
    "\n\n"
    (mastodon-tl--byline toot)
    "\n\n")))

(defun mastodon-tl--timeline (toots)
  "Display each toot in TOOTS."
  (mapc 'mastodon-tl--toot toots)
  (goto-char (point-min))
  (while (search-forward "\n\n\n | " nil t)
    (replace-match "\n | "))
  (mastodon-media--inline-images))

(defun mastodon-tl--get-update-function (&optional buffer)
  "Get the UPDATE-FUNCTION stored in `mastodon-tl--buffer-spec'"
  (mastodon-tl--get-buffer-property 'update-function buffer))

(defun mastodon-tl--get-endpoint (&optional buffer)
  "Get the ENDPOINT stored in `mastodon-tl--buffer-spec'"
  (mastodon-tl--get-buffer-property 'endpoint buffer))

(defun mastodon-tl--buffer-name (&optional buffer)
  "Get the BUFFER-NAME stored in `mastodon-tl--buffer-spec'"
  (mastodon-tl--get-buffer-property 'buffer-name buffer ))

(defun mastodon-tl--get-buffer-property (property &optional buffer)
  "Get `MASTODON-TL--BUFFER-SPEC' in BUFFER or `CURRENT-BUFFER'"
  (with-current-buffer  (or buffer (current-buffer))
    (if (plist-get mastodon-tl--buffer-spec property)
        (plist-get mastodon-tl--buffer-spec property)
      (error "mastodon-tl--buffer-spec is not defined for buffer %s"
             (or buffer (current-buffer))))))

(defun mastodon-tl--more-json (endpoint id)
  "Return JSON for timeline ENDPOINT before ID."
  (let* ((url (mastodon-http--api (concat
                                   endpoint
                                   (if (string-match-p "?" endpoint)
                                       "&"
                                     "?")
                                   "max_id="
                                   (if (numberp id )
                                       (number-to-string id)
                                     id)))))

    (mastodon-http--get-json url)))

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
                                  (if (numberp id)
                                      (number-to-string id)
                                    id)))))
    (mastodon-http--get-json url)))

(defun mastodon-tl--property (prop &optional backward)
  "Get property PROP for toot at point.

Move forward (down) the timeline unless BACKWARD is non-nil."
  (or (get-text-property (point) prop)
      (progn
        (if backward
            (mastodon-tl--goto-prev-toot)
          (mastodon-tl--goto-next-toot))
        (get-text-property (point) prop))))

(defun mastodon-tl--newest-id ()
  "Return toot-id from the top of the buffer."
  (goto-char (point-min))
  (mastodon-tl--property 'toot-id))

(defun mastodon-tl--oldest-id ()
  "Return toot-id from the bottom of the buffer."
  (goto-char (point-max))
  (mastodon-tl--property 'toot-id t))

(defun mastodon-tl--thread ()
  "Open thread buffer for toot under `point'."
  (interactive)
  (let* ((id (number-to-string (mastodon-tl--property 'toot-id)))
         (url (mastodon-http--api (format "statuses/%s/context" id)))
         (buffer (format "*mastodon-thread-%s*" id))
         (toot (mastodon-tl--property 'toot-json))
         (context (mastodon-http--get-json url)))
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      (mastodon-tl--timeline (vconcat
                              (cdr (assoc 'ancestors context))
                              `(,toot)
                              (cdr (assoc 'descendants context)))))
    (mastodon-mode)))

(defun mastodon-tl--more ()
  "Append older toots to timeline."
  (interactive)
  (let* ((point-before (point))
         (endpoint (mastodon-tl--get-endpoint))
         (update-function (mastodon-tl--get-update-function))
         (id (mastodon-tl--oldest-id))
         (json (mastodon-tl--more-json endpoint id)))
    (when json
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (funcall update-function json)
        (goto-char point-before)))))

(defun mastodon-tl--find-property-range (property start-point)
  "Finds (start . end) range around or after START-POINT where PROPERTY is set to a consistent value.

If PROPERTY is set at START-POINT returns a range aroung
START-POINT otherwise after START-POINT."
  (if (get-text-property start-point property)
      ;; We are within a range, so look backwards for the start:
      (cons (or (previous-single-property-change start-point property)
                (point-min))
            (or (next-single-property-change start-point property)
                (point-max)))
    (let* ((start (next-single-property-change start-point property))
           (end (and start
                     (or (next-single-property-change start property)
                         (point-max)))))
      (when start
        (cons start end)))))

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
                      (setq next-timestamp-range (mastodon-tl--find-property-range 'timestamp
                                                                                   previous-timestamp)))
            (let* ((start (car next-timestamp-range))
                   (end (cdr next-timestamp-range))
                   (timestamp (get-text-property start 'timestamp))
                   (current-display (get-text-property start 'display))
                   (new-display (mastodon-tl--relative-time-description timestamp)))
              (unless (string= current-display new-display)
                (let ((inhibit-read-only t))
                  (add-text-properties start end
                                       (list 'display (mastodon-tl--relative-time-description timestamp)))))
              (mastodon-tl--consider-timestamp-for-updates timestamp)
              (setq iteration (1+ iteration)
                    previous-timestamp (1+ (cdr next-timestamp-range)))))
          (if next-timestamp-range
              ;; schedule the next batch from the previous location to
              ;; start very soon in the future:
              (run-at-time 0.1 nil #'mastodon-tl--update-timestamps-callback buffer (copy-marker previous-timestamp))
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
        (goto-char (point-min))
        (funcall update-function json)))))

(defun mastodon-tl--init (buffer-name endpoint update-function)
  "Initialize BUFFER-NAME with timeline targeted by ENDPOINT.

UPDATE-FUNCTION is used to recieve more toots."
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
            mastodon-tl--timestamp-update-timer (when mastodon-tl--enable-relative-timestamps
                                                  (run-at-time mastodon-tl--timestamp-next-update
                                                               nil ;; don't repeat
                                                               #'mastodon-tl--update-timestamps-callback
                                                               (current-buffer)
                                                               nil))))
    buffer))

(provide 'mastodon-tl)
;;; mastodon-tl.el ends here
