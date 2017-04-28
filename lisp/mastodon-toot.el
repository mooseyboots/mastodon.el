;;; mastodon-toot.el --- Minor mode for sending Mastodon toots

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.6.0
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
(defvar mastodon-toot--content-warning nil)

(defun mastodon-toot--action-success (marker)
  "Insert MARKER with 'success face in byline."
  (let ((inhibit-read-only t))
    (insert (format "(%s) "
                    (propertize marker 'face 'success)))
    (mastodon-tl--goto-prev-toot)))

(defun mastodon-toot--action (action callback)
  "Take ACTION on toot at point, then execute CALLBACK."
  (let* ((id (mastodon-tl--property 'toot-id))
         (url (mastodon-http--api (concat "statuses/"
                                         (number-to-string id)
                                         "/"
                                         action))))
    (let ((response (mastodon-http--post url nil nil)))
      (mastodon-http--triage response callback))))

(defun mastodon-toot--kill ()
  "Kill `mastodon-toot-mode' buffer and window.

Set `mastodon-toot--reply-to-id' to nil.
Set `mastodon-toot--content-warning' to nil."
  (kill-buffer-and-window)
  (setq mastodon-toot--reply-to-id     nil
        mastodon-toot--content-warning nil))

(defun mastodon-toot--cancel ()
  "Kill new-toot buffer/window. Does not POST content to Mastodon."
  (interactive)
  (mastodon-toot--kill))

(defun mastodon-toot--remove-docs ()
  "Get the body of a toot from the current compose buffer."
  (let ((re "^|=+=|$"))
    (save-excursion
      (goto-char 0)
      (re-search-forward re)
      (re-search-forward re) ; end of the docs
      (buffer-substring (+ 2 (point)) (length (buffer-string))))))

(defun mastodon-toot--send ()
  "Kill new-toot buffer/window and POST contents to the Mastodon instance."
  (interactive)
  (let* ((toot (mastodon-toot--remove-docs))
         (endpoint (mastodon-http--api "statuses"))
         (spoiler (when mastodon-toot--content-warning
                    (read-string "Warning: ")))
         (args `(("status" . ,toot)
                 ("in_reply_to_id" . ,mastodon-toot--reply-to-id)
                 ("sensitive" . ,(when mastodon-toot--content-warning
                                   (symbol-name t)))
                 ("spoiler_text" . ,spoiler))))
    (progn
      (mastodon-toot--kill)
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

(defun mastodon-toot--reply ()
  "Reply to toot at `point'."
  (interactive)
  (let* ((toot (mastodon-tl--property 'toot-json))
         (id (number-to-string (mastodon-tl--field 'id toot)))
         (account (mastodon-tl--field 'account toot))
         (user (cdr (assoc 'username account))))
    (mastodon-toot user id)))

(defun mastodon-toot--toggle-warning ()
  "Toggle `mastodon-toot--content-warning'."
  (interactive)
  (setq mastodon-toot--content-warning
        (not mastodon-toot--content-warning)))

(defun mastodon-toot--get-mode-kbinds ()
  "Get a list of the keybindings in the mastodon-toot-mode."
  (remove-if-not (lambda (x) (listp x))
		 (cadr mastodon-toot-mode-map)))

(defun mastodon-toot--format-kbind (kbind)
  "Format a single keybinding, KBIND, for display in documentation."
  (let ((key (help-key-description (vector (car kbind)) nil))
	(command (cdr kbind)))
    (format "\t%s - %s" key command)))

(defun mastodon-toot--format-kbinds (kbinds)
  "Format a list keybindings, KBINDS, for display in documentation."
  (string-join (cons "" (mapcar #'mastodon-toot--format-kbind kbinds))
	       "\n"))

(defun mastodon-toot--make-mode-docs ()
  "Create formatted documentation text for the mastodon-toot-mode."
  (let ((kbinds (mastodon-toot--get-mode-kbinds)))
    (concat
     "|=================================================================|\n"
     " Compose a new toot here. The following keybindings are available:"
     (mastodon-toot--format-kbinds kbinds)
     "\n|=================================================================|\n\n")))

(defun mastodon-toot--display-docs ()
  "Display documentation about mastodon-toot mode."
  (insert
   (propertize
    (mastodon-toot--make-mode-docs)
    'face 'comment)))

(defun mastodon-toot--setup-as-reply (reply-to-user reply-to-id)
  "If REPLY-TO-USER is provided, inject their handle into the message.
If REPLY-TO-ID is provided, set the MASTODON-TOOT--REPLY-TO-ID var."
  (when reply-to-user
    (insert (format "@%s " reply-to-user))
    (setq mastodon-toot--reply-to-id reply-to-id)))

(defun mastodon-toot--compose-buffer (reply-to-user reply-to-id)
  "Create a new buffer to capture text for a new toot.
If REPLY-TO-USER is provided, inject their handle into the message.
If REPLY-TO-ID is provided, set the MASTODON-TOOT--REPLY-TO-ID var."
  (let ((buffer (get-buffer-create "*new toot*")))
    (switch-to-buffer-other-window buffer)
    (mastodon-toot--display-docs)
    (mastodon-toot--setup-as-reply reply-to-user reply-to-id)
    (mastodon-toot-mode t)))

(defvar mastodon-toot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mastodon-toot--send)
    (define-key map (kbd "C-c C-k") #'mastodon-toot--cancel)
    (define-key map (kbd "C-c C-w") #'mastodon-toot--toggle-warning)
      map)
  "Keymap for `mastodon-toot'.")

(define-minor-mode mastodon-toot-mode
  "Minor mode to capture Mastodon toots."
  :group 'mastodon-toot
  :keymap mastodon-toot-mode-map
  :global nil)

(provide 'mastodon-toot)
;;; mastodon-toot.el ends here
