;;; mastodon-http.el --- HTTP request/response functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.9.0
;; Package-Requires: ((emacs "24.4"))
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

;; mastodon-http.el provides HTTP request/response functions.

;;; Code:

(require 'json)
(require 'request) ; for attachments upload
(defvar mastodon-instance-url)
(autoload 'mastodon-auth--access-token "mastodon-auth")

(defvar mastodon-http--api-version "v1")

(defconst mastodon-http--timeout 5
  "HTTP request timeout, in seconds.  Has no effect on Emacs < 26.1.")

(defun mastodon-http--api (endpoint)
  "Return Mastondon API URL for ENDPOINT."
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

(defun mastodon-http--triage (response success)
  "Determine if RESPONSE was successful. Call SUCCESS if successful.

Open RESPONSE buffer if unsuccessful."
  (let ((status (with-current-buffer response
                  (mastodon-http--status))))
    (if (string-prefix-p "2" status)
        (funcall success)
      (switch-to-buffer response))))

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
	  headers)))
    (with-temp-buffer
      (if (< (cdr (func-arity 'url-retrieve-synchronously)) 4)
          (url-retrieve-synchronously url)
        (url-retrieve-synchronously url nil nil mastodon-http--timeout)))))

(defun mastodon-http--get (url)
  "Make GET request to URL.

Pass response buffer to CALLBACK function."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer "
                                       (mastodon-auth--access-token))))))
    (if (< (cdr (func-arity 'url-retrieve-synchronously)) 4)
        (url-retrieve-synchronously url)
      (url-retrieve-synchronously url nil nil mastodon-http--timeout))))

(defun mastodon-http--delete (url)
  "Make DELETE request to URL."
  (let ((url-request-method "DELETE")
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer "
                                       (mastodon-auth--access-token))))))
    (with-temp-buffer
      (url-retrieve-synchronously url))))

(defun mastodon-http--get-json (url)
  "Make GET request to URL. Return JSON response."
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
    (json-read-from-string json-string)))

 ;; Asynchronous functions

(defun mastodon-http--get-async (url &optional callback &rest cbargs)
  "Make GET request to URL.

Pass response buffer to CALLBACK function with args CBARGS."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer "
                                       (mastodon-auth--access-token))))))
    (url-retrieve url callback cbargs mastodon-http--timeout)))

(defun mastodon-http--get-json-async (url &optional callback &rest args)
  "Make GET request to URL. Call CALLBACK with json-vector and ARGS."
  (mastodon-http--get-async
   url
   (lambda (status)
     (apply callback (mastodon-http--process-json) args))))

(defun mastodon-http--post-async (url args headers &optional callback &rest cbargs)
  "POST asynchronously to URL with ARGS and HEADERS.

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
      (url-retrieve url callback cbargs mastodon-http--timeout))))

;; TODO: test for curl first?
(defun mastodon-http--post-media-attachment (url filename caption)
  "Make a POST request to upload file FILENAME with CAPTION to the server's media URL.

The upload is asynchronous. On succeeding, `mastodon-toot--media-attachment-ids' is set to the id(s) of the item uploaded, `mastodon-toot--media-attachments' is set to t, and `mastodon-toot--update-status-fields' is run."
  (let* ((file (file-name-nondirectory filename))
         (request-backend 'curl)
         (response
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
                          (progn
                            (push (cdr (assoc 'id data))
                                  mastodon-toot--media-attachment-ids) ; add ID to list
                            (push file mastodon-toot--media-attachment-filenames)
                            (message "%s file %s with id %S and caption '%s' uploaded!"
                                     (capitalize (cdr (assoc 'type data)))
                                     file
                                     (cdr (assoc 'id data))
                                     (cdr (assoc 'description data)))
                            (mastodon-toot--update-status-fields)))))
            :error (cl-function
                    (lambda (&key error-thrown &allow-other-keys)
                      (message "Got error: %s" error-thrown)))
            )))
    (pcase (request-response-status-code response)
      (200
       (request-response-data response)
       ))))

(provide 'mastodon-http)
;;; mastodon-http.el ends here
