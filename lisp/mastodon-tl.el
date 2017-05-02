;;; mastodon-tl.el --- HTTP request/response functions for mastodon.el

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.6.2
;; Homepage: https://github.com/jdenen/mastodon.el

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

(require 'mastodon-http)
(require 'mastodon-toot)
(require 'mastodon-media)
(require 'time-date)

(defgroup mastodon-tl nil
  "Timelines in Mastodon."
  :prefix "mastodon-tl-"
  :group 'mastodon)

(defun mastodon-tl--get-federated-timeline ()
  "Opens federated timeline."
  (interactive)
  (mastodon-tl--get "public"))

(defun mastodon-tl--get-home-timeline ()
  "Opens home timeline."
  (interactive)
  (mastodon-tl--get "home"))

(defun mastodon-tl--get-local-timeline ()
  "Opens local timeline."
  (interactive)
  (mastodon-tl--get "public?local=true"))

(defun mastodon-tl--get-tag-timeline ()
  "Prompts for tag and opens its timeline."
  (interactive)
  (let* ((word (or (word-at-point) ""))
         (input (read-string (format "Tag(%s): " word)))
         (tag (if (equal input "") word input)))
    (print tag)
    (mastodon-tl--get (concat "tag/" tag))))

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

(defun mastodon-tl--timeline-name ()
  "Determine timeline from `buffer-name'."
  (replace-regexp-in-string "\*" ""
                            (replace-regexp-in-string "mastodon-" "" (buffer-name))))

(defun mastodon-tl--remove-html (toot)
  "Remove unrendered tags from TOOT."
  (let* ((t1 (replace-regexp-in-string "<\/p>" "\n\n" toot))
         (t2 (replace-regexp-in-string "<\/?span>" "" t1)))
    (replace-regexp-in-string "<span class=\"h-card\">" "" t2)))

(defun mastodon-tl--byline-author (toot)
  "Propertize author of TOOT."
  (let* ((account (cdr (assoc 'account toot)))
         (handle (cdr (assoc 'acct account)))
         (name (cdr (assoc 'display_name account))))
    (concat
     (propertize name 'face 'warning)
     " (@"
     handle
     ")")))

(defun mastodon-tl--byline-boosted (toot)
  "Add byline for boosted data from TOOT."
  (let ((reblog (cdr (assoc 'reblog toot))))
    (when reblog
      (concat
       " "
       (propertize "Boosted" 'face 'highlight)
       " "
       (mastodon-tl--byline-author reblog)))))

(defun mastodon-tl--field (field toot)
  "Return FIELD from TOOT.

Return value from boosted content if available."
  (or (cdr (assoc field (cdr (assoc 'reblog toot))))
      (cdr (assoc field toot))))

(defun mastodon-tl--byline (toot)
  "Generate byline for TOOT."
  (let ((id (cdr (assoc 'id toot)))
        (timestamp (mastodon-tl--field 'created_at toot))
        (faved (mastodon-tl--field 'favourited toot))
        (boosted (mastodon-tl--field 'reblogged toot)))
    (propertize
     (concat (propertize "\n | " 'face 'default)
             (when boosted
               (format "(%s) " (propertize "B" 'face 'success)))
             (when faved
               (format "(%s) " (propertize "F" 'face 'success)))
             (mastodon-tl--byline-author toot)
             (mastodon-tl--byline-boosted toot)
             " "
             (format-time-string mastodon-toot-timestamp-format (date-to-time timestamp))
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
         (cw (mastodon-tl--set-face message 'success nil)))
    (if (> (length string) 0)
        (replace-regexp-in-string "\n\n\n ---------------"
                                  "\n ---------------" (concat string cw))
      "")))

(defun mastodon-tl--media (toot)
  "Retrieve a media attachment link for TOOT if one exists."
  (let ((media (mastodon-tl--field 'media_attachments toot)))
        (mapconcat
         (lambda (media-preview)
           (concat "Media_Link:: "
                   (mastodon-tl--set-face
                    (cdr (assoc 'preview_url media-preview))
                    'mouse-face nil)))
         media "\n")))

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
    (mastodon-tl--content toot)
    (mastodon-tl--media toot)
    (mastodon-tl--byline toot)
    "\n\n")))

(defun mastodon-tl--timeline (toots)
  "Display each toot in TOOTS."
  (mapcar 'mastodon-tl--toot toots)
  (replace-regexp "\n\n\n | " "\n | " nil (point-min) (point-max))
  (mastodon-media--inline-images))

(defun mastodon-tl--more-json (timeline id)
  "Return JSON for TIMELINE before ID."
  (let ((url (mastodon-http--api (concat "timelines/"
                                         timeline
                                         "?max_id="
                                         (number-to-string id)))))
    (mastodon-http--get-json url)))

;; TODO
;; Look into the JSON returned here by Local
(defun mastodon-tl--updated-json (timeline id)
  "Return JSON for TIMELINE since ID."
  (let ((url (mastodon-http--api (concat "timelines/"
                                         timeline
                                         "?since_id="
                                         (number-to-string id)))))
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
  (progn
    (goto-char (point-max))
    (mastodon-tl--property 'toot-id t)))

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
         (tl (mastodon-tl--timeline-name))
         (id (mastodon-tl--oldest-id))
         (json (mastodon-tl--more-json tl id)))
    (when json
      (with-current-buffer (current-buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (mastodon-tl--timeline json)
          (goto-char point-before)
          (mastodon-tl--goto-next-toot))))))

(defun mastodon-tl--update ()
  "Update timeline with new toots."
  (interactive)
  (let* ((tl (mastodon-tl--timeline-name))
         (id (mastodon-tl--newest-id))
         (json (mastodon-tl--updated-json tl id)))
    (when json
      (with-current-buffer (current-buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (mastodon-tl--timeline json))))))

(defun mastodon-tl--get (timeline)
  "Display TIMELINE in buffer."
  (let* ((url (mastodon-http--api (concat "timelines/" timeline)))
         (buffer (concat "*mastodon-" timeline "*"))
         (json (mastodon-http--get-json url)))
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      (mastodon-tl--timeline json))
    (mastodon-mode)))

(provide 'mastodon-tl)
;;; mastodon-tl.el ends here
