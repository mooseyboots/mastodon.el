;;; mastodon-search.el --- serach functions for mastodon.el  -*- lexical-binding: t -*-

;; search functions:

;; autoloads?

;; mastodon-tl--as-string
;; mastodon-tl--set-face
;; mastodon-tl--render-text
;; mastodon-tl--toot
;; mastodon-http--get-json

;; mastodon-instance-url

;; code

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
         (status-list (mapcar #'mastodon-search--get-status-info
                              statuses))
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
                       " USERS" "\n"
                       " ------------\n")
               'success))
      (mapcar (lambda (el)
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
      (mapcar (lambda (el)
                (dolist (item el)
                  (insert (mastodon-tl--render-text item nil) ""))
                (insert "----\n\n"))
                ;; (seq-do 'insert el))
                ;; (insert (mastodon-tl--render-text (car el) nil)
                ;;         " : "
                ;;         (mastodon-tl--render-text (car (cdr el)) nil)
                ;;         "\n"))
              tags-list)
      (insert (mastodon-tl--set-face
               (concat "\n ------------\n"
                       " STATUSES" "\n"
                       " ------------\n")
               'success))
      (mapcar 'mastodon-tl--toot toots-list-json)
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
  (goto-char (point-min))
  (re-search-forward "^$" nil 'move)
  (let ((json-string
         (decode-coding-string
          (buffer-substring-no-properties (point) (point-max))
          'utf-8)))
    (kill-buffer)
    (json-read-from-string json-string)))

(defun mastodon-http--get-search-json (url query)
  "Make GET request to URL. Return JSON response"
  (let ((buffer (mastodon-http--get-search url query)))
    (with-current-buffer buffer
      (mastodon-http--process-json-search))))

(defun mastodon-http--get-search (base-url query)
  "Make GET request to URL.

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
;; mastodon-search.el ends here
