;;; mastodon-toot.el --- Minor mode for sending Mastodon toots

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.5.5
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

;; mastodon-toot.el supports POSTing status data to Mastodon.

;;; Code:

(require 'mastodon-auth nil t)

(defgroup mastodon-toot nil
  "Capture Mastodon toots."
  :prefix "mastodon-toot-"
  :group 'mastodon)

(defvar mastodon-toot--reply-to-id nil)

(defun mastodon-toot--action-success (marker)
  "Insert MARKER with 'success face in byline."
  (let ((inhibit-read-only t))
    (move-beginning-of-line '())
    (mastodon-tl--goto-next-toot)
    (insert (format "(%s) "
                    (propertize marker 'face 'success)))))

(defun mastodon-toot--action-success-undo (marker)
  "Remove MARKER from byline."
  (let ((inhibit-read-only t)
	(start (progn (move-beginning-of-line '())
		      (point)))
	(end (progn (move-end-of-line '())
		    (point))))
    (replace-regexp (format "(%s) " marker) "" '() start end)
    (move-beginning-of-line '())
    (mastodon-tl--goto-next-toot)))

(defun mastodon-toot--action (action callback)
  "Take ACTION on toot at point, then execute CALLBACK."
  (let* ((id (mastodon-tl--property 'toot-id))
         (url (mastodon-http--api (concat "statuses/"
                                         (number-to-string id)
                                         "/"
                                         action))))
    (let ((response (mastodon-http--post url nil nil)))
      (mastodon-http--triage response callback))))

(defun mastodon-toot--cancel ()
  "Kill new-toot buffer/window. Does not POST content to Mastodon."
  (interactive)
  (setq mastodon-toot--reply-to-id nil)
  (kill-buffer-and-window))

(defun mastodon-toot--send ()
  "Kill new-toot buffer/window and POST contents to the Mastodon instance."
  (interactive)
  (let* ((toot (buffer-string))
         (endpoint (mastodon-http--api "statuses"))
         (args `(("status" . ,toot)
                 ("in_reply_to_id" . ,mastodon-toot--reply-to-id))))
    (progn
      (kill-buffer-and-window)
      (setq mastodon-toot--reply-to-id nil)
      (let ((response (mastodon-http--post endpoint args nil)))
        (mastodon-http--triage response
                               (lambda () (message "Toot toot!")))))))

(defun mastodon-toot--boost ()
  "Boost toot at `point'."
  (interactive)
  (let ((callback (lambda () (mastodon-toot--action-success "B")))
        (id (mastodon-tl--property 'toot-id)))
    (mastodon-toot--action "reblog" callback)
    (message (format "Boosted #%s" id))))

(defun mastodon-toot--favourite ()
  "Favourite toot at `point'."
  (interactive)
  (let ((callback (lambda () (mastodon-toot--action-success "F")))
        (id (mastodon-tl--property 'toot-id)))
    (mastodon-toot--action "favourite" callback)
    (message (format "Favourited #%s" id))))

(defun mastodon-toot--unfavourite ()
  "Favourite toot at `point'."
  (interactive)
  (let ((callback (lambda () ( mastodon-toot--action-success-undo "F")))
        (id (mastodon-tl--property 'toot-id)))
    (mastodon-toot--action "unfavourite" callback)
    (message (format "unfavourited #%s" id))))

(defun mastodon-toot--reply ()
  "Reply to toot at `point'."
  (interactive)
  (let* ((toot (mastodon-tl--property 'toot-json))
         (id (number-to-string (mastodon-tl--field 'id toot)))
         (account (mastodon-tl--field 'account toot))
         (user (cdr (assoc 'username account))))
    (mastodon-toot user id)))

(defvar mastodon-toot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mastodon-toot--send)
    (define-key map (kbd "C-c C-k") #'mastodon-toot--cancel)
      map)
  "Keymap for `mastodon-toot'.")

(define-minor-mode mastodon-toot-mode
  "Minor mode to capture Mastodon toots."
  :group 'mastodon-toot
  :keymap mastodon-toot-mode-map
  :global nil)

(provide 'mastodon-toot)
;;; mastodon-toot.el ends here
