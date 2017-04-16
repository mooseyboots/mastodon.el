;;; mastodon-tl.el --- HTTP request/response functions for mastodon.el

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
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

(defgroup mastodon-tl nil
  "Timelines in Mastodon."
  :prefix "mastodon-tl-"
  :group 'mastodon)

(defface mastodon-tl-toot-text-face
  '((t (:foreground "LightGray")))
  "Toot content face.")

(defface mastodon-tl-toot-display-name-face
  '((t (:foreground "cyan")))
  "Mastodon user handle face.")

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
  (let ((tag (read-string "Tag: ")))
    (mastodon-tl--get (concat "tag/" tag))))

(defun mastodon-tl--from-toot (key toot)
  "Return value for KEY in TOOT."
  (cdr (assoc key toot)))

(defun mastodon-tl--goto-toot-pos (find-pos &optional pos)
  "Search for toot with FIND-POS. Optionally stat from POS."
  (let* ((npos (funcall find-pos
                        (or pos (point))
                        'toot-id
                        (current-buffer))))
    (when npos (if (not (get-text-property npos 'toot-id))
                   (mastodon-tl--goto-toot-pos find-pos npos)
                 (when npos (goto-char npos))))))

(defun mastodon-tl--goto-next-toot ()
  "Jump to next toot header."
  (interactive)
  (mastodon-tl--goto-toot-pos 'next-single-property-change))

(defun mastodon-tl--goto-prev-toot ()
  "Jump to last toot header."
  (interactive)
  (mastodon-tl--goto-toot-pos 'previous-single-property-change))

(defun mastodon-tl--header (handle name id)
  "Create header string with toot properties.

HANDLE is the username.
NAME is the display name.
ID is the toot id."
  (propertize
   (concat "=== "
           (propertize name 'face 'mastodon-tl-toot-display-name-face)
           " @"
           handle
           "\n")
   'toot-id id))

(defun mastodon-tl--remove-html (toot)
  "Remove HTML from TOOT."
  (let* ((t1 (replace-regexp-in-string "<\/p>" "\n\n" toot))
         (t2 (replace-regexp-in-string "<\/?span>" "" t1))
         (t3 (replace-regexp-in-string "<span class=\"h-card\">" "" t2)))
    t3))

(defun mastodon-tl--content (toot)
  "Distill content to display from TOOT."
  (let* ((content (mastodon-tl--remove-html toot)))
     (propertize content
                 'face 'mastodon-tl-toot-text-face)))

(defun mastodon-tl--render-toot (toot)
  "Display TOOT with header and content sections."
  (let ((id (number-to-string (mastodon-tl--from-toot 'id toot)))
        (handle (mastodon-tl--from-toot 'acct (mastodon-tl--from-toot 'account toot)))
        (name (mastodon-tl--from-toot 'display_name (mastodon-tl--from-toot 'account toot)))
        (text (mastodon-tl--from-toot 'content toot))
        (url (mastodon-tl--from-toot 'url toot)))
    (insert
     (propertize
      (concat
       (mastodon-tl--header handle name id)
       (mastodon-tl--content text)
       "\n")
      'toot-url url))))

(defun mastodon-tl--render-timeline (buffer json)
  "Display toots in BUFFER. Data taken from JSON."
  (switch-to-buffer buffer)
  (mapcar 'mastodon-tl--render-toot json)
  (html2text))

(defun mastodon-tl--timeline-name ()
  "Determine timeline from `buffer-name'."
  (replace-regexp-in-string "\*" ""
                            (replace-regexp-in-string "mastodon-" "" (buffer-name))))

(defun mastodon-tl--update ()
  "Update timeline with new toots."
  (interactive)
  (let* ((tl (mastodon-tl--timeline-name))
         (id (get-text-property (point-min) 'toot-id))
         (url (mastodon--api-for (concat "timelines/" tl "?since_id=" id))))
    (with-current-buffer (current-buffer)
      (let ((inhibit-read-only t)
            (json (mastodon-http--get-json url)))
        (goto-char (point-min))
        (mastodon-tl--render-timeline (current-buffer) json)))))

(defun mastodon-tl--get (timeline)
  "Display toots for TIMELINE in new buffer."
  (let* ((url (mastodon--api-for (concat "timelines/" timeline)))
         (tl-buff (concat "*mastodon-" timeline "*"))
         (tl-json (mastodon-http--get-json url)))
    (with-output-to-temp-buffer tl-buff
      (mastodon-tl--render-timeline tl-buff tl-json))
    (mastodon-mode)))

(provide 'mastodon-tl)
;;; mastodon-tl.el ends here
