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
(defvar mastodon-instance-url)
(autoload 'mastodon-auth--access-token "mastodon-auth")

(defvar mastodon-http--api-version "v1")

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
      (url-retrieve-synchronously url))))

(defun mastodon-http--get (url)
  "Make GET request to URL.

Pass response buffer to CALLBACK function."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer "
                                       (mastodon-auth--access-token))))))
    (url-retrieve-synchronously url)))

(defun mastodon-http--get-json (url)
  "Make GET request to URL. Return JSON response vector."
  (let ((json-vector
         (with-current-buffer (mastodon-http--get url)
           (goto-char (point-min))
           (re-search-forward "^$" nil 'move)
           (let ((json-string
                  (decode-coding-string
                   (buffer-substring-no-properties (point) (point-max))
                   'utf-8)))
             (kill-buffer)
             (json-read-from-string json-string)))))
    json-vector))

(defun mastodon-http--get-json-async (url callback &optional cbargs)
  "Make an async get request to URL.  On completion CALLBACK will be called with
at least three argumnts:
1) a success boolean (true of the request succeeded),
2) the list of headers, and
3) the response body (parsed json if success, raw text if failed),
any other arguments passed as CBARGS."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer "
                                       (mastodon-auth--access-token)))))
        ;; Keep url.el from spamming us with messages about connecting to hosts:
	(url-show-status nil))
    ;; Show anything already done before we go off making an HTTP request.
    (redisplay) 
    (url-retrieve url
                  #'mastodon-http--process-async-response
                  (list callback cbargs)
                  t)))

(defun mastodon-http--process-async-response (status callback cbargs)
  "Process the response of an async get request to URL.

STATUS is the retrieval status as described in `url-retrieve'.
URL, CALLBACK and CBARGS are the arguments originally passed in the
`mastodon-http--get-json-async' call."
  (goto-char (point-min))
  (let* ((is-error (eq :error (car status)))
         (body-start
          (progn
            (goto-char (point-min))
            (re-search-forward "^$" nil 'move)
            (point)))
         (header-text
          (decode-coding-string
           (buffer-substring-no-properties (point-min) (1- body-start))
           'utf-8))
         (body-text
          (decode-coding-string
           (buffer-substring-no-properties body-start (point-max))
           'utf-8)))

    (kill-buffer)

    (apply callback
           (not is-error)
           (cdr (split-string header-text "\n"))
           (if is-error
               body-text
             (json-read-from-string body-text))
           cbargs)))

(provide 'mastodon-http)
;;; mastodon-http.el ends here
