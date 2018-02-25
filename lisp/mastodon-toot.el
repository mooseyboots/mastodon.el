;;; mastodon-toot.el --- Minor mode for sending Mastodon toots  -*- lexical-binding: t -*-

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

;; mastodon-toot.el supports POSTing status data to Mastodon.

;;; Code:

(defvar mastodon-toot--reply-to-id nil)
(defvar mastodon-toot--content-warning nil)

(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")
(autoload 'mastodon-http--triage "mastodon-http")
(autoload 'mastodon-tl--field "mastodon-tl")
(autoload 'mastodon-tl--goto-next-toot "mastodon-tl")
(autoload 'mastodon-tl--property "mastodon-tl")
(autoload 'mastodon-toot "mastodon")

(defvar mastodon-toot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mastodon-toot--send)
    (define-key map (kbd "C-c C-k") #'mastodon-toot--cancel)
    (define-key map (kbd "C-c C-w") #'mastodon-toot--toggle-warning)
      map)
  "Keymap for `mastodon-toot'.")


(defun mastodon-toot--action-success (marker &optional rm)
  "Insert MARKER with 'success face in byline.

Remove MARKER if RM is non-nil."
  (let ((inhibit-read-only t)
        (bol (progn (move-beginning-of-line nil) (point)))
        (eol (progn (move-end-of-line nil) (point))))
    (when rm
      (goto-char bol)
      (if (search-forward (format "(%s) " marker) eol t)
          (replace-match "")))
    (move-beginning-of-line nil)
    (mastodon-tl--goto-next-toot)
    (unless rm
      (insert (format "(%s) "
                      (propertize marker 'face 'success))))))

(defun mastodon-toot--action (action callback)
  "Take ACTION on toot at point, then execute CALLBACK."
  (let* ((id (mastodon-tl--property 'toot-id))
         (url (mastodon-http--api (concat "statuses/"
                                         (number-to-string id)
                                         "/"
                                         action))))
    (let ((response (mastodon-http--post url nil nil)))
      (mastodon-http--triage response callback))))

(defun mastodon-toot--toggle-boost ()
  "Boost/unboost toot at `point'."
  (interactive)
  (let* ((id (mastodon-tl--property 'toot-id))
         (boosted (get-text-property (point) 'boosted-p))
         (action (if boosted "unreblog" "reblog"))
         (msg (if boosted "unboosted" "boosted"))
         (remove (when boosted t)))
    (mastodon-toot--action action
                           (lambda ()
                             (mastodon-toot--action-success "B" remove)
                             (message (format "%s #%d" msg id))))))

(defun mastodon-toot--toggle-favourite ()
  "Favourite/unfavourite toot at `point'."
  (interactive)
  (let* ((id (mastodon-tl--property 'toot-id))
         (faved (get-text-property (point) 'favourited-p))
         (action (if faved "unfavourite" "favourite"))
         (remove (when faved t)))
    (mastodon-toot--action action
                           (lambda ()
                             (mastodon-toot--action-success "F" remove)
                             (message (format "%sd #%d" action id))))))

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
      (re-search-forward re nil nil 2)
      (buffer-substring (+ 2 (point)) (+ 1 (length (buffer-string)))))))

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
    (mastodon-toot--kill)
    (let ((response (mastodon-http--post endpoint args nil)))
      (mastodon-http--triage response
                             (lambda () (message "Toot toot!"))))))

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
    (format "\t%s - %s" key command)))

(defun mastodon-toot--format-kbinds (kbinds)
  "Format a list keybindings, KBINDS, for display in documentation."
  (mapconcat 'identity (cons "" (mapcar #'mastodon-toot--format-kbind kbinds))
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
  (let* ((buffer-exists (get-buffer "*new toot*"))
         (buffer (or buffer-exists (get-buffer-create "*new toot*"))))
    (switch-to-buffer-other-window buffer)
    (when (not buffer-exists)
      (mastodon-toot--display-docs)
      (mastodon-toot--setup-as-reply reply-to-user reply-to-id))
    (mastodon-toot-mode t)))

(define-minor-mode mastodon-toot-mode
  "Minor mode to capture Mastodon toots."
  :group 'mastodon-toot
  :keymap mastodon-toot-mode-map
  :global nil)

(provide 'mastodon-toot)
;;; mastodon-toot.el ends here
