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

(defun mastodon-tl--from-toot (key toot)
  "Return value for KEY in TOOT."
  (cdr (assoc key toot)))

(defun mastodon-tl--render-display-name (name)
  (insert
   (propertize (concat name " ")
               'face 'mastodon-tl-toot-display-name-face)))

(defun mastodon-tl--render-header (handle name id url)
  (insert "=== ")
  (mastodon-tl--render-display-name name)
  (insert (concat "@" handle "\n")))

(defun mastodon-tl--remove-html (toot)
  (let* ((t1 (replace-regexp-in-string "<\/p>" "\n\n" toot))
         (t2 (replace-regexp-in-string "<\/?span>" "" t1))
         (t3 (replace-regexp-in-string "<span class=\"h-card\">" "" t2)))
    t3))

(defun mastodon-tl--render-content (toot)
  (let* ((content (mastodon-tl--remove-html toot)))
    (insert
     (propertize content
                 'face 'mastodon-tl-toot-text-face))
    (insert "\n")))

(defun mastodon-tl--render-toot (toot)
  (let ((id (number-to-string (mastodon-tl--from-toot 'id toot)))
        (handle (mastodon-tl--from-toot 'acct (mastodon-tl--from-toot 'account toot)))
        (name (mastodon-tl--from-toot 'display_name (mastodon-tl--from-toot 'account toot)))
        (text (mastodon-tl--from-toot 'content toot))
        (url (mastodon-tl--from-toot 'url toot)))
    (mastodon-tl--render-header handle name id url)
    (mastodon-tl--render-content text)))

(defun mastodon-tl--render-timeline (buffer json)
  (with-output-to-temp-buffer buffer
    (switch-to-buffer buffer)
    (mapcar 'mastodon-tl--render-toot json))
  (html2text))

(defun mastodon-tl--get (timeline)
  (let* ((url (mastodon--api-for (concat "timelines/" timeline)))
         (tl-buff (concat "*mastodon-" timeline "*"))
         (tl-json (mastodon-http--get-json url)))
    (mastodon-tl--render-timeline tl-buff tl-json)
    (mastodon-mode)))

(provide 'mastodon-tl)
;;; mastodon-tl.el ends here
