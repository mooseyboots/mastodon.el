;;; mastodon-http.el --- HTTP request/response functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Maintainer: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 0.10.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))
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

;; mastodon-http.el provides HTTP request/response functions.

;;; Code:

(require 'json)
(require 'request) ; for attachments upload

(defvar mastodon-instance-url)
(defvar mastodon-toot--media-attachment-ids)
(defvar mastodon-toot--media-attachment-filenames)

(autoload 'mastodon-auth--access-token "mastodon-auth")
(autoload 'mastodon-toot--update-status-fields "mastodon-toot")


(defvar mastodon-http--api-version "v1")

(defconst mastodon-http--timeout 10
  "HTTP request timeout, in seconds.  Has no effect on Emacs < 26.1.")

(defun mastodon-http--api (endpoint)
  "Return Mastodon API URL for ENDPOINT."
  (concat mastodon-instance-url "/api/"
          mastodon-http--api-version "/" endpoint))

(defun mastodon-http--response ()
  "Capture response buffer content as string."
  (with-current-buffer (current-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mastodon-http--response-body (pattern)
  "Return substring matching PATTERN from `mastodon-http--response'."
  (let ((resp (mastodon-http--response)))
    (string-match pattern resp)
    (match-string 0 resp)))

(defun mastodon-http--status ()
  "Return HTTP Response Status Code from `mastodon-http--response'."
  (let* ((status-line (mastodon-http--response-body "^HTTP/1.*$")))
    (string-match "[0-9][0-9][0-9]" status-line)
    (match-string 0 status-line)))

(defun mastodon-http--url-retrieve-synchronously (url)
  "Retrieve URL asynchronously.

This is a thin abstraction over the system
`url-retrieve-synchronously`.  Depending on which version of this
is available we will call it with or without a timeout."
  (if (< (cdr (func-arity 'url-retrieve-synchronously)) 4)
      (url-retrieve-synchronously url)
    (url-retrieve-synchronously url nil nil mastodon-http--timeout)))

(defun mastodon-http--triage (response success)
  "Determine if RESPONSE was successful. Call SUCCESS if successful.

Message status and JSON error from RESPONSE if unsuccessful."
  (let ((status (with-current-buffer response
                  (mastodon-http--status))))
    (if (string-prefix-p "2" status)
        (funcall success)
      (switch-to-buffer response)
      (let ((json-response (mastodon-http--process-json)))
        (message "Error %s: %s" status (alist-get 'error json-response))))))

(defun mastodon-http--read-file-as-string (filename)
  "Read a file FILENAME as a string. Used to generate image preview."
  (with-temp-buffer
    (insert-file-contents filename)
    (string-to-unibyte (buffer-string))))

(defun mastodon-http--post (url args headers &optional unauthenticed-p)
  "POST synchronously to URL with ARGS and HEADERS.

Authorization header is included by default unless UNAUTHENTICED-P is non-nil."
  (let ((url-request-method "POST")
        (url-request-data
         (when args
           (mapconcat (lambda (arg)
                        (concat (url-hexify-string (car arg))
                                "="
                                (url-hexify-string (cdr arg))))
                      args
                      "&")))
        (url-request-extra-headers
	 (append
	  (unless unauthenticed-p
	    `(("Authorization" . ,(concat "Bearer " (mastodon-auth--access-token)))))
          (unless (assoc "Content-Type" headers)
            '(("Content-Type" . "application/x-www-form-urlencoded")))
	  headers)))
    (with-temp-buffer
      (mastodon-http--url-retrieve-synchronously url))))

(defun mastodon-http--get (url)
  "Make synchronous GET request to URL.

Pass response buffer to CALLBACK function."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer "
                                       (mastodon-auth--access-token))))))
    (mastodon-http--url-retrieve-synchronously url)))

(defun mastodon-http--get-json (url)
  "Make synchronous GET request to URL. Return JSON response."
  (with-current-buffer (mastodon-http--get url)
    (mastodon-http--process-json)))

(defun mastodon-http--process-json ()
  "Process JSON response."
  (goto-char (point-min))
  (re-search-forward "^$" nil 'move)
  (let ((json-string
         (decode-coding-string
          (buffer-substring-no-properties (point) (point-max))
          'utf-8)))
    (kill-buffer)
    (unless (or (string-equal "" json-string) (null json-string))
      (json-read-from-string json-string))))

(defun mastodon-http--delete (url)
  "Make DELETE request to URL."
  (let ((url-request-method "DELETE")
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer "
                                       (mastodon-auth--access-token))))))
    (with-temp-buffer
      (mastodon-http--url-retrieve-synchronously url))))

;; search functions:
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

(defun mastodon-http--get-search-json (url query &optional param)
  "Make GET request to URL, searching for QUERY and return JSON response.
PARAM is any extra parameters to send with the request."
  (let ((buffer (mastodon-http--get-search url query param)))
    (with-current-buffer buffer
      (mastodon-http--process-json-search))))

(defun mastodon-http--get-search (base-url query &optional param)
  "Make GET request to BASE-URL, searching for QUERY.
Pass response buffer to CALLBACK function.
PARAM is a formatted request parameter, eg 'following=true'."
  (let ((url-request-method "GET")
        (url (if param
                 (concat base-url "?" param "&q=" (url-hexify-string query))
               (concat base-url "?q=" (url-hexify-string query))))
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer "
                                       (mastodon-auth--access-token))))))
    (mastodon-http--url-retrieve-synchronously url)))

;; profile update functions

(defun mastodon-http--patch-json (url)
  "Make synchronous PATCH request to URL. Return JSON response."
  (with-current-buffer (mastodon-http--patch url)
    (mastodon-http--process-json)))

;; hard coded just for bio note for now:
(defun mastodon-http--patch (base-url &optional note)
  "Make synchronous PATCH request to BASE-URL.
Optionally specify the NOTE to edit.
Pass response buffer to CALLBACK function."
  (let ((url-request-method "PATCH")
        (url (if note
                 (concat base-url "?note=" (url-hexify-string note))
               base-url))
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer "
                                       (mastodon-auth--access-token))))))
    (mastodon-http--url-retrieve-synchronously url)))

 ;; Asynchronous functions

(defun mastodon-http--get-async (url &optional callback &rest cbargs)
  "Make GET request to URL.
Pass response buffer to CALLBACK function with args CBARGS."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer "
                                       (mastodon-auth--access-token))))))
    (url-retrieve url callback cbargs)))

(defun mastodon-http--get-json-async (url &optional callback &rest args)
  "Make GET request to URL. Call CALLBACK with json-vector and ARGS."
  (mastodon-http--get-async
   url
   (lambda (status)
     (when status ;; only when we actually get sth?
       (apply callback (mastodon-http--process-json) args)))))

(defun mastodon-http--post-async (url args headers &optional callback &rest cbargs)
  "POST asynchronously to URL with ARGS and HEADERS.
Then run function CALLBACK with arguements CBARGS.
Authorization header is included by default unless UNAUTHENTICED-P is non-nil."
  (let ((url-request-method "POST")
        (request-timeout 5)
        (url-request-data
         (when args
           (mapconcat (lambda (arg)
                        (concat (url-hexify-string (car arg))
                                "="
                                (url-hexify-string (cdr arg))))
                      args
                      "&")))
        (url-request-extra-headers
	 (append `(("Authorization" . ,(concat "Bearer " (mastodon-auth--access-token))))
	         headers)))
    (with-temp-buffer
      (url-retrieve url callback cbargs))))

;; TODO: test for curl first?
(defun mastodon-http--post-media-attachment (url filename caption)
  "Make POST request to upload FILENAME with CAPTION to the server's media URL.
The upload is asynchronous. On succeeding,
`mastodon-toot--media-attachment-ids' is set to the id(s) of the
item uploaded, and `mastodon-toot--update-status-fields' is run."
  (let* ((file (file-name-nondirectory filename))
         (request-backend 'curl))
    (request
     url
     :type "POST"
     :params `(("description" . ,caption))
     :files `(("file" . (,file :file ,filename
                               :mime-type "multipart/form-data")))
     :parser 'json-read
     :headers `(("Authorization" . ,(concat "Bearer "
                                            (mastodon-auth--access-token))))
     :sync nil
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (when data
                   (push (alist-get 'id data)
                         mastodon-toot--media-attachment-ids) ; add ID to list
                   (message "%s file %s with id %S and caption '%s' uploaded!"
                            (capitalize (alist-get 'type data))
                            file
                            (alist-get 'id data)
                            (alist-get 'description data))
                   (mastodon-toot--update-status-fields))))
     :error (cl-function
             (lambda (&key error-thrown &allow-other-keys)
               (cond
                ;; handle curl errors first (eg 26, can't read file/path)
                ;; because the '=' test below fails for them
                ;; they have the form (error . error message 24)
                ((not (proper-list-p error-thrown)) ; not dotted list
		 (message "Got error: %s. Shit went south." (cdr error-thrown)))
                ;; handle mastodon api errors
                ;; they have the form (error http 401)
		((= (car (last error-thrown)) 401)
                 (message "Got error: %s Unauthorized: The access token is invalid" error-thrown))
                ((= (car (last error-thrown)) 422)
                 (message "Got error: %s Unprocessable entity: file or file type is unsupported or invalid" error-thrown))
                (t
                 (message "Got error: %s Shit went south"
                          error-thrown))))))))

(provide 'mastodon-http)
;;; mastodon-http.el ends here
