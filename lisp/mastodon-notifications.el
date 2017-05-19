;;; mastodon-notifications.el --- Notification functions for mastodon.el

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.5.4
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

;; mastodon-notification.el provides notification functions.

;; todo: - integrate media
;;       - extend notification types
;;       - replace `You' with `your_display_name'(@`your_user_name')
;;       - generalize mastodon-tl--more or rebind it in notifications
;;         example
;;       - fix thread access
;;       - Add keybinding of mastodon-notifications--get to `N'

;; ;; One approach for generalizing the get-more functionality
;; (defun mastodon-tl--get-more (more-function id-fun goto-fun
;;                               processing-function)
;;   "can work for both oldert toots and newest toots"
;;   (interactive)
;;   (let* ((tl (mastodon-tl--timeline-name))
;;       (id (funcall id-fun))
;;       (json (funcall more-function tl id)))
;;     (when json
;;       (with-current-buffer (current-buffer)
;;      (let ((inhibit-read-only t))
;;        (goto-char (funcall goto-fun))
;;        (funcall processing-function json))))))
;;
;; ;; Equivelent to the mastodon-tl--more
;; (mastodon-tl--get-more 'mastodon-tl--more-json 'mastodon-tl--oldest-id
;;                        point-max mastodon-tl--timeline)
;;
;; ;; Equivelent to the mastodon-tl--update
;; (mastodon-tl--get-more 'mastodon-tl--update-json 'mastodon-tl--newest-id
;;                        point-min mastodon-tl--timeline)


;;; Code:

;;(require 'mastodon-media)
(require 'mastodon-http)
(require 'mastodon-tl)

(defgroup mastodon-notifications nil
  "Mastodon Notifications."
  :prefix "mastodon-notifications-"
  :group 'mastodon)

(defvar mastodon-notifications--types
  '("mention" "follow" "favourite" "reblog" )
  "A list of notification types that have been implemented.")

(defun mastodon-notifications--byline-concat (toot message)
  "Add byline for TOOT with MESSAGE."
      (concat
       " "
       (propertize message 'face 'highlight)
       " "
       "You!"))

(defun mastodon-notifications--byline (toot message)
  "Generate byline from TOOT based on MESSAGE for notification."
  (let ((id (cdr (assoc 'id toot)))
        (faved (mastodon-tl--field 'favourited toot))
        (boosted (mastodon-tl--field 'reblogged toot)))
    (propertize
     (concat (propertize "\n | " 'face 'default)
             (when boosted
               (format "(%s) " (propertize "B" 'face 'success)))
             (when faved
               (format "(%s) " (propertize "F" 'face 'success)))
             (mastodon-tl--byline-author toot)
             (mastodon-notifications--byline-concat toot message)
             (propertize "\n  ------------" 'face 'default))
     'toot-id id
     'toot-json toot)))

(defun mastodon-notifications--mention (note)
  "Format for a `mention' NOTE."
  (let ((toot (mastodon-tl--field 'status note)))
    (insert
     (concat (mastodon-tl--content toot)
             (mastodon-notifications--byline toot "Mentioned")
             "\n\n"))))

(defun mastodon-notifications--follow (note)
  "Format for a `follow' NOTE."
  (let ((toot (mastodon-tl--field 'status note)))
  (insert
   (concat
    "Congratulations, you have a new follower!\n\n"
    (mastodon-notifications--byline note "Follows")
    "\n\n"))))


(defun mastodon-notifications--favorite(note)
  "Format for a `favorite' NOTE."
  (let ((toot (mastodon-tl--field 'status note)))
    (insert
     (concat (mastodon-tl--content toot)
             (mastodon-notifications--byline note "Favorited")
             "\n\n"))))

(defun mastodon-notifications--reblog (note)
  "Format for a `boost' NOTE."
  (let ((toot (mastodon-tl--field 'status note)))
    (insert
     (concat (mastodon-tl--content toot)
             (mastodon-notifications--byline note "Boosted")
             "\n\n"))))

(defun mastodon-notifications--caller (type note)
  "Call the apprpriate function bassed on notification TYPE.

It then processes NOTE."
  (cond ((equal type "mention") (mastodon-notifications--mention note))
        ((equal type "follow") (mastodon-notifications--follow note))
        ((equal type "favourite") (mastodon-notifications--favorite note))
        ((equal type "reblog") (mastodon-notifications--reblog note))))

(defun mastodon-notifications--note (note)
  "Filters NOTE for those listed in `mastodon-notifications--types'.

  It then passes the results to the `caller'"
  (let ((type (mastodon-tl--field 'type note)))
    (when (member type mastodon-notifications--types)
      (mastodon-notifications--caller type note))))

(defun mastodon-notifications--notifications (json)
  "Format JSON in Emacs buffer."
  (mapc #'mastodon-notifications--note json)
  (goto-char (point-min))
  (while (search-forward "\n\n\n | " nil t)
    (replace-match "\n | ")))

(defun mastodon-notifications--get()
  (interactive)
  (mastodon-tl--init
   "notifications" "notifications" 'mastodon-notifications--notifications))

;; (defun mastodon-notifications--get ()
;;   "Display NOTIFICATIONS in buffer."
;;   (interactive)
;;   (let* ((url (mastodon-http--api "notifications"))
;;          (buffer "*mastodon-notifications*")
;;          (json (mastodon-http--get-json url)))
;;     (with-output-to-temp-buffer buffer
;;       (switch-to-buffer buffer)
;;       (mastodon-notifications--notifications json))
;;     (mastodon-mode)))

(provide 'mastodon-notifications)
;;; mastodon-notifications.el ends here
