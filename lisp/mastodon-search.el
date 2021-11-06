;;; mastodon-search.el --- Search functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>, martyhiatt <mousebot@riseup.net>
;; Version: 0.9.1
;; Homepage: https://github.com/jdenen/mastodon.el
;; Package-Requires: ((emacs "26.1"))

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

;; A basic search function for mastodon.el

;;; Code:
(require 'json)

(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-tl--as-string "mastodon-tl")
(autoload 'mastodon-mode "mastodon")
(autoload 'mastodon-tl--set-face "mastodon-tl")
(autoload 'mastodon-tl--render-text "mastodon-tl")
(autoload 'mastodon-tl--as-string "mastodon-tl")
(autoload 'mastodon-auth--access-token "mastodon-auth")
(autoload 'mastodon-http--get-search-json "mastodon-http")

(defvar mastodon-instance-url)
(defvar mastodon-tl--link-keymap)
(defvar mastodon-http--timeout)
(defvar mastodon-toot--enable-completion-for-mentions)

;; functions for company completion of mentions in mastodon-toot

(defun mastodon-search--get-user-info-@ (account)
  "Get user handle, display name and account URL from ACCOUNT."
  (list (cdr (assoc 'display_name account))
        (concat "@" (cdr (assoc 'acct account)))
        (cdr (assoc 'url account))))

(defun mastodon-search--search-accounts-query (query)
  "Prompt for a search QUERY and return accounts synchronously.
Returns a nested list containing user handle, display name, and URL."
  (interactive "sSearch mastodon for: ")
  (let* ((url (format "%s/api/v1/accounts/search" mastodon-instance-url))
         ;; (buffer (format "*mastodon-search-%s*" query))
         (response (if (equal mastodon-toot--enable-completion-for-mentions "following")
                       (mastodon-http--get-search-json url query "following=true")
                     (mastodon-http--get-search-json url query))))
    (mapcar #'mastodon-search--get-user-info-@
            response)))

;; functions for mastodon search

(defun mastodon-search--search-query (query)
  "Prompt for a search QUERY and return accounts, statuses, and hashtags."
  (interactive "sSearch mastodon for: ")
  (let* ((url (format "%s/api/v2/search" mastodon-instance-url))
         (buffer (format "*mastodon-search-%s*" query))
         (response (mastodon-http--get-search-json url query))
         (accts (alist-get 'accounts response))
         (tags (alist-get 'hashtags response))
         (statuses (alist-get 'statuses response))
         (user-ids (mapcar #'mastodon-search--get-user-info
                           accts)) ; returns a list of three-item lists
         (tags-list (mapcar #'mastodon-search--get-hashtag-info
                            tags))
         ;; (status-list (mapcar #'mastodon-search--get-status-info
         ;; statuses))
         (status-ids-list (mapcar 'mastodon-search--get-id-from-status
                                  statuses))
         (toots-list-json (mapcar #'mastodon-search--fetch-full-status-from-id
                                  status-ids-list)))
    (with-current-buffer (get-buffer-create buffer)
      (switch-to-buffer buffer)
      (erase-buffer)
      (mastodon-mode)
      (let ((inhibit-read-only t))
        ;; user results:
        (insert (mastodon-tl--set-face
                 (concat "\n ------------\n"
                         " USERS\n"
                         " ------------\n\n")
                 'success))
        (mapc (lambda (el)
                (insert (propertize (car el) 'face 'mastodon-display-name-face)
                        " : \n : "
                        (propertize (concat "@" (car (cdr el)))
                                    'face 'mastodon-handle-face
                                    'mouse-face 'highlight
		                    'mastodon-tab-stop 'user-handle
		                    'keymap mastodon-tl--link-keymap
                                    'mastodon-handle (concat "@" (car (cdr el)))
		                    'help-echo (concat "Browse user profile of @" (car (cdr el))))
                        " : \n"
                        "\n"))
              user-ids)
        ;; hashtag results:
        (insert (mastodon-tl--set-face
                 (concat "\n ------------\n"
                         " HASHTAGS\n"
                         " ------------\n\n")
                 'success))
        (mapc (lambda (el)
                (insert " : #"
                        (propertize (car el)
                                    'mouse-face 'highlight
                                    'mastodon-tag (car el)
                                    'mastodon-tab-stop 'hashtag
                                    'help-echo (concat "Browse tag #" (car el))
                                    'keymap mastodon-tl--link-keymap)
                        " : \n\n"))
              tags-list)
        ;; status results:
        (insert (mastodon-tl--set-face
                 (concat "\n ------------\n"
                         " STATUSES\n"
                         " ------------\n")
                 'success))
        (mapc 'mastodon-tl--toot toots-list-json)
        (goto-char (point-min))))))

(defun mastodon-search--get-user-info (account)
  "Get user handle, display name and account URL from ACCOUNT."
  (list (alist-get 'display_name account)
        (alist-get 'acct account)
        (alist-get 'url account)))

(defun mastodon-search--get-hashtag-info (tag)
  "Get hashtag name and URL from TAG."
  (list (alist-get 'name tag)
        (alist-get 'url tag)))

(defun mastodon-search--get-status-info (status)
  "Get ID, timestamp, content, and spoiler from STATUS."
  (list (alist-get 'id status)
        (alist-get 'created_at status)
        (alist-get 'spoiler_text status)
        (alist-get 'content status)))

(defun mastodon-search--get-id-from-status (status)
  "Fetch the id from a STATUS returned by a search call to the server.

We use this to fetch the complete status from the server."
  (alist-get 'id status))

(defun mastodon-search--fetch-full-status-from-id (id)
  "Fetch the full status with id ID from the server.

This allows us to access the full account etc. details and to
render them properly."
  (let* ((url (concat mastodon-instance-url "/api/v1/statuses/" (mastodon-tl--as-string id)))
         (json (mastodon-http--get-json url)))
    json))

(provide 'mastodon-search)
;;; mastodon-search.el ends here
