;;; mastodon-toot.el --- Minor mode for sending Mastodon toots

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

;; mastodon-toot.el supports POSTing status data to Mastodon.

;;; Code:

(require 'mastodon-auth nil t)

(defgroup mastodon-toot nil
  "Capture Mastodon toots."
  :prefix "mastodon-toot-"
  :group 'mastodon)

(defcustom mastodon-toot-character-limit 500
  "Maximum number of characters allowed in a toot."
  :type 'number)

(defun mastodon-toot--edit-info (&optional docs)
  "Take an optional DOCS string and create an alist representing data about the edit buffer."
  (let* ((d (if (stringp docs) docs ""))
	 (s (+ 1 (length d)))
	 (e s)
	 (c 0))
    `((docs . ,d)
      (start . ,s)
      (end . ,e)
      (count . ,c))))

(defvar-local mastodon-toot--reply-to-id nil)
(defvar-local mastodon-toot--content-warning nil)

(defvar-local mastodon-toot--edit-data nil
  "Buffer-local variable to store data about the toot being composed.")

(defun mastodon-toot--action-success (marker &optional rm)
  "Insert MARKER with 'success face in byline.

Remove MARKER if RM is non-nil."
  (let ((inhibit-read-only t)
        (bol (progn (move-beginning-of-line '()) (point)))
        (eol (progn (move-end-of-line '()) (point))))
    (when rm (replace-match (format "(%s) " marker) "" '() bol eol))
    (move-beginning-of-line '())
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
  (kill-all-local-variables)
  (kill-buffer-and-window))

(defun mastodon-toot--cancel ()
  "Kill new-toot buffer/window.  Does not POST content to Mastodon."
  (interactive)
  (mastodon-toot--kill))

(defun mastodon-toot--get-from-buffer ()
  "Use the data stored in MASTODON-TOOT--EDIT-DATA and the compose buffer to extract the toot text."
  (let ((toot-start (alist-get 'start mastodon-toot--edit-data))
	(toot-end (alist-get 'end mastodon-toot--edit-data)))
    (buffer-substring toot-start toot-end)))

(defun mastodon-toot--send ()
  "Kill new-toot buffer/window and POST contents to the Mastodon instance."
  (interactive)
  (let* ((toot (mastodon-toot--get-from-buffer))
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
  (setq-local mastodon-toot--content-warning
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
    (propertize
     (concat
      "|=================================================================|\n"
      " Compose a new toot here. The following keybindings are available:"
      (mastodon-toot--format-kbinds kbinds)
      "\n|=================================================================|\n")
     'read-only t
     'face 'font-lock-comment-face)))

(defun mastodon-toot--format-char-count (current max)
  "Format the [CURRENT/MAX] character count pattern."
  (propertize
   (format "\n[%5d / %5d]" current max)
   'read-only t
   'face
   (if (> current max)
       'warning
     'font-lock-comment-face)))

(defun mastodon-toot--setup-as-reply (reply-to-user reply-to-id)
  "If REPLY-TO-USER is provided, inject their handle into the message.
If REPLY-TO-ID is provided, set the MASTODON-TOOT--REPLY-TO-ID var."
  (when reply-to-user
    (insert (format "@%s " reply-to-user))
    (setq-local mastodon-toot--reply-to-id reply-to-id)))

(defun mastodon-toot--init-data ()
  "Initialize the buffer-local toot edit data."
  (let* ((docs (mastodon-toot--make-mode-docs)))
    (setq-local mastodon-toot--edit-data
		(mastodon-toot--edit-info docs))))

(defun mastodon-toot--draw-ui (edit-data)
  "Render a compose ui in buffer using EDIT-DATA."
  (let ((start (alist-get 'start edit-data))
	(end (alist-get 'end edit-data)))
    (progn
      (insert
       (propertize
	"\n"
	'read-only nil))
      (goto-char (point-min))
      (insert (alist-get 'docs edit-data))
      (goto-char (+ 1 start))
      (insert (mastodon-toot--format-char-count
      	       (alist-get 'count edit-data)
      	       mastodon-toot-character-limit))
      (goto-char (+ 1 start))
      )))

(defun mastodon-toot--compose-buffer (reply-to-user reply-to-id)
  "Create a new buffer to capture text for a new toot.
If REPLY-TO-USER is provided, inject their handle into the message.
If REPLY-TO-ID is provided, set the MASTODON-TOOT--REPLY-TO-ID var."
  (let* ((buffer-name (concat "*new " (mastodon-instance-name) " toot*"))
	 (buffer-exists (get-buffer buffer-name))
	 (buffer (or buffer-exists (get-buffer-create buffer-name))))
    (switch-to-buffer-other-window buffer)
    (when (not mastodon-toot--edit-data)
      (mastodon-toot--init-data)
      (mastodon-toot--draw-ui mastodon-toot--edit-data)
      (mastodon-toot--setup-as-reply reply-to-user reply-to-id))
    (mastodon-toot-mode t)))

;;; hooks to make editing sensible
(defun mastodon-toot--stay-in-bounds (change-start-pos change-end-pos)
  "Use CHANGE-START-POS and CHANGE-END-POS to keep edits between the docs and the char count."
  (let ((start-lim  (alist-get 'start mastodon-toot--edit-data))
	(end-lim  (+ 1 (alist-get 'end mastodon-toot--edit-data))))
    (message (format "%s:%s %s:%s"
		     change-start-pos start-lim
		     change-end-pos end-lim))
    (cond
     ((< change-start-pos start-lim)
      (goto-char start-lim))
     ((< end-lim change-start-pos)
      (goto-char end-lim)))))

(defun mastodon-toot--update-char-count (&rest _)
  "Update the character count in MASTODON-TOOT--EDIT-DATA."
  (let* ((raw-len (length (buffer-string)))
	 (char-count-len (length (mastodon-toot--format-char-count
				  (alist-get 'count mastodon-toot--edit-data)
				  mastodon-toot-character-limit)))
	 (doc-string-len (length (alist-get 'docs mastodon-toot--edit-data)))
	 (final-len (- raw-len char-count-len doc-string-len))
	 (start (alist-get 'start mastodon-toot--edit-data)))
    (message (format "l:%s s:%s e:%s c:%s"
		     final-len
		     start
		     (alist-get 'end mastodon-toot--edit-data)
		     (alist-get 'count mastodon-toot--edit-data)))
    (setf (alist-get 'end mastodon-toot--edit-data)
	  (+ final-len start)

	  (alist-get 'count mastodon-toot--edit-data)
	  final-len)))

(defun mastodon-toot--render-char-count (&rest _)
  "Clear and render the character counter from MASTODON-TOOT--EDIT-DATA."
  (let* ((count (alist-get 'count mastodon-toot--edit-data))
	 (count-str (mastodon-toot--format-char-count
		     count mastodon-toot-character-limit))
	 (count-str-len (length count-str))
	 (count-str-end (point-max))
	 (count-str-start (- count-str-end count-str-len)))
    (save-mark-and-excursion
     (setq-local inhibit-read-only t)
     (remove-text-properties count-str-start count-str-end '(read-only))
     (delete-region count-str-start count-str-end)
     (goto-char (point-max))
     (setq-local inhibit-read-only nil)
     (insert count-str))))

(defun mastodon-toot--after-change (&rest _)
  "Dah."
  (progn
    (mastodon-toot--update-char-count nil)
    (mastodon-toot--render-char-count nil)))

(defun mastodon-toot--mode-hooks ()
  "Set the hooks for mastodon-toot-mode."
  (progn
    (add-hook 'before-change-functions
    	      #'mastodon-toot--stay-in-bounds nil t)
    (add-hook 'after-change-functions
	      #'mastodon-toot--after-change nil t)))

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
  :global nil
  :after-hook
  (progn
    (mastodon-toot--mode-hooks)))

(provide 'mastodon-toot)
;;; mastodon-toot.el ends here
