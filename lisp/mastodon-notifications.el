;;; mastodon-notifications.el --- Notification functions for mastodon.el -*- lexical-binding: t -*-

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

;; mastodon-notification.el provides notification functions for Mastodon.

;;; Code:

(autoload 'mastodon-http--api "mastodon-http.el")
(autoload 'mastodon-http--post "mastodon-http.el")
(autoload 'mastodon-http--triage "mastodon-http.el")
(autoload 'mastodon-media--inline-images "mastodon-media.el")
(autoload 'mastodon-tl--byline "mastodon-tl.el")
(autoload 'mastodon-tl--byline-author "mastodon-tl.el")
(autoload 'mastodon-tl--clean-tabs-and-nl "mastodon-tl.el")
(autoload 'mastodon-tl--content "mastodon-tl.el")
(autoload 'mastodon-tl--field "mastodon-tl.el")
(autoload 'mastodon-tl--find-property-range "mastodon-tl.el")
(autoload 'mastodon-tl--has-spoiler "mastodon-tl.el")
(autoload 'mastodon-tl--init "mastodon-tl.el")
(autoload 'mastodon-tl--init-sync "mastodon-tl.el")
(autoload 'mastodon-tl--insert-status "mastodon-tl.el")
(autoload 'mastodon-tl--property "mastodon-tl.el")
(autoload 'mastodon-tl--spoiler "mastodon-tl.el")
(autoload 'mastodon-tl--toot-id "mastodon-tl.el")
(defvar mastodon-tl--display-media-p)

(defvar mastodon-notifications--types-alist
  '(("mention" . mastodon-notifications--mention)
    ("follow" . mastodon-notifications--follow)
    ("favourite" . mastodon-notifications--favourite)
    ("reblog" . mastodon-notifications--reblog)
    ("follow_request" . mastodon-notifications--follow-request)
    ("status" . mastodon-notifications--status))
  "Alist of notification types and their corresponding function.")

(defvar mastodon-notifications--response-alist
  '(("Mentioned" . "you")
    ("Followed" . "you")
    ("Favourited" . "your status from")
    ("Boosted" . "your status from")
    ("Requested to follow" . "you")
    ("Posted" . "a post"))
  "Alist of subjects for notification types.")

(defun mastodon-notifications--byline-concat (message)
  "Add byline for TOOT with MESSAGE."
  (concat
   " "
   (propertize message 'face 'highlight)
   " "
   (cdr (assoc message mastodon-notifications--response-alist))))

(defun mastodon-notifications--follow-request-accept-notifs ()
  "Accept the follow request of user at point, in notifications view."
  (interactive)
  (when (mastodon-tl--find-property-range 'toot-json (point))
    (let* ((toot-json (mastodon-tl--property 'toot-json))
           (f-req-p (string= "follow_request" (alist-get 'type toot-json))))
      (if f-req-p
          (let* ((account (alist-get 'account toot-json))
                 (id (alist-get 'id account))
                 (handle (alist-get 'acct account))
                 (name (alist-get 'username account)))
            (if id
                (let ((response
                       (mastodon-http--post
                        (concat
                         (mastodon-http--api "follow_requests")
                         (format "/%s/authorize" id))
                        nil nil)))
                  (mastodon-http--triage response
                                         (lambda ()
                                           (mastodon-notifications--get)
                                           (message "Follow request of %s (@%s) accepted!"
                                                    name handle))))
              (message "No account result at point?")))
        (message "No follow request at point?")))))

(defun mastodon-notifications--follow-request-reject-notifs ()
  "Reject the follow request of user at point, in notifications view."
  (interactive)
  (when (mastodon-tl--find-property-range 'toot-json (point))
    (let* ((toot-json (mastodon-tl--property 'toot-json))
           (f-req-p (string= "follow_request" (alist-get 'type toot-json))))
      (if f-req-p
          (let* ((account (alist-get 'account toot-json))
                 (id (alist-get 'id account))
                 (handle (alist-get 'acct account))
                 (name (alist-get 'username account)))
            (if id
                (let ((response
                       (mastodon-http--post
                        (concat
                         (mastodon-http--api "follow_requests")
                         (format "/%s/reject" id))
                        nil nil)))
                  (mastodon-http--triage response
                                         (lambda ()
                                           (mastodon-notifications--get)
                                           (message "Follow request of %s (@%s) rejected!"
                                                    name handle))))
              (message "No account result at point?")))
        (message "No follow request at point?")))))

(defun mastodon-notifications--mention (note)
  "Format for a `mention' NOTE."
  (let ((id (alist-get 'id note))
        (status (mastodon-tl--field 'status note)))
    (mastodon-notifications--insert-status
     status
     (mastodon-tl--clean-tabs-and-nl
      (if (mastodon-tl--has-spoiler status)
          (mastodon-tl--spoiler status)
        (mastodon-tl--content status)))
     'mastodon-tl--byline-author
     (lambda (_status)
       (mastodon-notifications--byline-concat
        "Mentioned"))
     id)))

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

(defun mastodon-notifications--follow-request (note)
  "Format for a `follow-request' NOTE."
  (let ((id (alist-get 'id note))
        (follower (alist-get 'username (alist-get 'account note))))
    (mastodon-notifications--insert-status
     (cons '(reblog (id . nil)) note)
     (propertize (format "You have a follow request from... %s" follower)
                 'face 'default)
     'mastodon-tl--byline-author
     (lambda (_status)
       (mastodon-notifications--byline-concat
        "Requested to follow"))
     id)))

(defun mastodon-notifications--favourite (note)
  "Format for a `favourite' NOTE."
  (let ((id (alist-get 'id note))
        (status (mastodon-tl--field 'status note)))
    (mastodon-notifications--insert-status
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
        "Favourited"))
     id)))

(defun mastodon-notifications--reblog (note)
  "Format for a `boost' NOTE."
  (let ((id (alist-get 'id note))
        (status (mastodon-tl--field 'status note)))
    (mastodon-notifications--insert-status
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
        "Boosted"))
     id)))

(defun mastodon-notifications--status (note)
  "Format for a `status' NOTE.
Status notifications are given when
`mastodon-tl--enable-notify-user-posts' has been set."
  (let ((id (cdr (assoc 'id note)))
        (status (mastodon-tl--field 'status note)))
    (mastodon-notifications--insert-status
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
        "Posted"))
     id)))

(defun mastodon-notifications--insert-status (toot body author-byline action-byline &optional id)
  "Display the content and byline of timeline element TOOT.

BODY will form the section of the toot above the byline.

AUTHOR-BYLINE is an optional function for adding the author
portion of the byline that takes one variable. By default it is
`mastodon-tl--byline-author'.

ACTION-BYLINE is also an optional function for adding an action,
such as boosting favouriting and following to the byline. It also
takes a single function. By default it is
`mastodon-tl--byline-boosted'.

ID is the notification's own id, which is attached as a property."
  (let ((start-pos (point)))
    (insert
     (propertize
      (concat "\n"
              body
              " \n"
              (mastodon-tl--byline toot author-byline action-byline))
      'toot-id      id
      'base-toot-id (mastodon-tl--toot-id toot)
      'toot-json    toot)
     "\n")
    (when mastodon-tl--display-media-p
      (mastodon-media--inline-images start-pos (point)))))

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
  (message "Loading your notifications...")
  (mastodon-tl--init-sync
   "notifications"
   "notifications"
   'mastodon-notifications--timeline))

(provide 'mastodon-notifications)
;;; mastodon-notifications.el ends here
