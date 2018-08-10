;;; mastodon-notifications.el --- Notification functions for mastodon.el -*- lexical-binding: t -*-

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.7.2
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

;; mastodon-notification.el provides notification functions for Mastodon.

;;; Code:

(autoload 'mastodon-media--inline-images "mastodon-media.el")
(autoload 'mastodon-tl--byline-author "mastodon-tl.el")
(autoload 'mastodon-tl--clean-tabs-and-nl "mastodon-tl.el")
(autoload 'mastodon-tl--content "mastodon-tl.el")
(autoload 'mastodon-tl--field "mastodon-tl.el")
(autoload 'mastodon-tl--has-spoiler "mastodon-tl.el")
(autoload 'mastodon-tl--init "mastodon-tl.el")
(autoload 'mastodon-tl--insert-status "mastodon-tl.el")
(autoload 'mastodon-tl--spoiler "mastodon-tl.el")
(defvar mastodon-tl--display-media-p)


(defvar mastodon-notifications--types-alist
  '(("mention" . mastodon-notifications--mention)
    ("follow" . mastodon-notifications--follow)
    ("favourite" . mastodon-notifications--favourite)
    ("reblog" . mastodon-notifications--reblog))
  "Alist of notification types and their corresponding function.")

(defvar mastodon-notifications--response-alist
  '(("Mentioned" . "you")
    ("Followed" . "you")
    ("Favourited" . "your status")
    ("Boosted" . "your status"))
  "Alist of subjects for notification types.")

(defun mastodon-notifications--byline-concat (message)
  "Add byline for TOOT with MESSAGE."
  (concat
   " "
   (propertize message 'face 'highlight)
   " "
   (cdr (assoc message mastodon-notifications--response-alist))))

(defun mastodon-notifications--mention (note)
  "Format for a `mention' NOTE."
  (let ((status (mastodon-tl--field 'status note)))
    (mastodon-tl--insert-status
     status
     (mastodon-tl--clean-tabs-and-nl
      (if (mastodon-tl--has-spoiler status)
          (mastodon-tl--spoiler status)
        (mastodon-tl--content status)))
     'mastodon-tl--byline-author
     (lambda (_status)
       (mastodon-notifications--byline-concat
        "Mentioned")))))

(defun mastodon-notifications--follow (note)
  "Format for a `follow' NOTE."
  (mastodon-tl--insert-status
   ;; Using reblog with an empty id will mark this as something
   ;; non-boostable/non-favable.
   (cons '(reblog (id . nil)) note)
   (propertize "Congratulations, you have a new follower!"
               'face 'default)
   'mastodon-tl--byline-author
   (lambda (_status)
     (mastodon-notifications--byline-concat
      "Followed"))))

(defun mastodon-notifications--favourite (note)
  "Format for a `favourite' NOTE."
  (let ((status (mastodon-tl--field 'status note)))
    (mastodon-tl--insert-status
     status
     (mastodon-tl--clean-tabs-and-nl
      (if (mastodon-tl--has-spoiler status)
          (mastodon-tl--spoiler status)
        (mastodon-tl--content status)))
     (lambda (_status)
       (mastodon-tl--byline-author
        note))
     (lambda (_status)
       (mastodon-notifications--byline-concat
        "Favourited")))))

(defun mastodon-notifications--reblog (note)
  "Format for a `boost' NOTE."
  (let ((status (mastodon-tl--field 'status note)))
    (mastodon-tl--insert-status
     status
     (mastodon-tl--clean-tabs-and-nl
      (if (mastodon-tl--has-spoiler status)
          (mastodon-tl--spoiler status)
        (mastodon-tl--content status)))
     (lambda (_status)
       (mastodon-tl--byline-author
        note))
     (lambda (_status)
       (mastodon-notifications--byline-concat
        "Boosted")))))

(defun mastodon-notifications--by-type (note)
  "Filters NOTE for those listed in `mastodon-notifications--types-alist'."
  (let* ((type (mastodon-tl--field 'type note))
         (fun (cdr (assoc type mastodon-notifications--types-alist)))
         (start-pos (point)))
    (when fun
      (funcall fun note)
      (when mastodon-tl--display-media-p
        (mastodon-media--inline-images start-pos (point))))))

(defun mastodon-notifications--timeline (json)
  "Format JSON in Emacs buffer."
  (mapc #'mastodon-notifications--by-type json)
  (goto-char (point-min)))

(defun mastodon-notifications--get ()
  "Display NOTIFICATIONS in buffer."
  (interactive)
  (mastodon-tl--init
   "*mastodon-notifications*"
   "notifications"
   'mastodon-notifications--timeline))

(provide 'mastodon-notifications)
;;; mastodon-notifications.el ends here
