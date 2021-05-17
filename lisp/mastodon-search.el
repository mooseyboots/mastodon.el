;;; mastodon-search.el --- search functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>, martyhiatt <mousebot@riseup.net>
;; Version: 0.9.0
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

(defvar mastodon-instance-url)
(defconst mastodon-http--timeout)

(defun mastodon-search--search-query (query)
  "Prompt for a search QUERY and return accounts, statuses, and hashtags."
  (interactive "sSearch mastodon for: ")
  (let* ((url (format "%s/api/v2/search" mastodon-instance-url))
         (buffer (format "*mastodon-search-%s*" query))
         (response (mastodon-http--get-search-json url query))
         (accts (cdr (assoc 'accounts response)))
         (tags (cdr (assoc 'hashtags response)))
         (statuses (cdr (assoc 'statuses response)))
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
      (setq-local inhibit-read-only t)
      (insert (mastodon-tl--set-face
               (concat "\n ------------\n"
                       " STATUSES" "\n"
                       " ------------\n")
               'success))
      (mapc 'mastodon-tl--toot toots-list-json)
      (insert (mastodon-tl--set-face
               (concat "\n ------------\n"
                       " USERS" "\n"
                       " ------------\n")
               'success))
      (mapc (lambda (el)
                (dolist (item el)
                  (insert (mastodon-tl--render-text item nil) ""))
                (insert "----\n\n"))
              ;;   (insert (mastodon-tl--render-text (car el) nil)
              ;;           " : "
              ;;           (mastodon-tl--render-text (car (cdr el)) nil)
              ;;           " : "
              ;;           (mastodon-tl--render-text (car (cdr (cdr el))) nil)
              ;;           "\n"))
              user-ids)
      (insert (mastodon-tl--set-face
               (concat "\n ------------\n"
                       " HASHTAGS" "\n"
                       " ------------\n")
               'success))
      (mapc (lambda (el)
                (dolist (item el)
                  (insert (mastodon-tl--render-text item nil) ""))
                (insert "----\n\n"))
                ;; (seq-do 'insert el))
                ;; (insert (mastodon-tl--render-text (car el) nil)
                ;;         " : "
                ;;         (mastodon-tl--render-text (car (cdr el)) nil)
                ;;         "\n"))
              tags-list)
      (goto-char (point-min))
      )))

(defun mastodon-search--get-user-info (account)
  "Get user handle, display name and account URL from ACCOUNT."
  (list (cdr (assoc 'display_name account))
        (cdr (assoc 'acct account))
        (cdr (assoc 'url account))))

(defun mastodon-search--get-hashtag-info (tag)
  "Get hashtag name and URL from TAG."
  (list (cdr (assoc 'name tag))
        (cdr (assoc 'url tag))))

(defun mastodon-search--get-status-info (status)
  "Get ID, timestamp, content, and spoiler from STATUS."
  (list (cdr (assoc 'id status))
        (cdr (assoc 'created_at status))
        (cdr (assoc 'spoiler_text status))
        (cdr (assoc 'content status))))

(defun mastodon-search--get-id-from-status (status)
    "Fetch the id from a STATUS returned by a search call to the server.

We use this to fetch the complete status from the server."
  (cdr (assoc 'id status)))

(defun mastodon-search--fetch-full-status-from-id (id)
  "Fetch the full status with id ID from the server.

This allows us to access the full account etc. details and to render them properly."
  (let* ((url (concat mastodon-instance-url "/api/v1/statuses/" (mastodon-tl--as-string id)))
        (json (mastodon-http--get-json url)))
    json))

;; http functions for search:
(defun mastodon-http--process-json-search ()
  "Process JSON returned by a search query to the server."
  (goto-char (point-min))
  (re-search-forward "^$" nil 'move)
  (let ((json-string
         (decode-coding-string
          (buffer-substring-no-properties (point) (point-max))
          'utf-8)))
    (kill-buffer)
    (json-read-from-string json-string)))

(defun mastodon-http--get-search-json (url query)
  "Make GET request to URL, searching for QUERY and return JSON response."
  (let ((buffer (mastodon-http--get-search url query)))
    (with-current-buffer buffer
      (mastodon-http--process-json-search))))

(defun mastodon-http--get-search (base-url query)
  "Make GET request to BASE-URL, searching for QUERY.

Pass response buffer to CALLBACK function."
  (let ((url-request-method "GET")
        (url (concat base-url "?q=" (url-hexify-string query)))
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer "
                                       (mastodon-auth--access-token))))))
    (if (< (cdr (func-arity 'url-retrieve-synchronously)) 4)
        (url-retrieve-synchronously url)
      (url-retrieve-synchronously url nil nil mastodon-http--timeout))))

(provide 'mastodon-search)
;;; mastodon-search.el ends here
