;;; mastodon-http.el --- HTTP request/response functions for mastodon.el

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

;; mastodon-http.el provides HTTP request/response functions.

;;; Code:

(require 'json)

(defgroup mastodon-http nil
  "HTTP requests and responses for Mastodon."
  :prefix "mastodon-http-"
  :group 'mastodon)

(defun mastodon-http--api (endpoint)
  "Return Mastondon API URL for ENDPOINT."
  (concat mastodon-instance-url "/api/" mastodon--api-version "/" endpoint))

(defun mastodon--http-post (url callback args &optional headers)
  "This function should be phased out in favor of `mastodon-http--post'.

Make POST request to URL.

Response buffer is passed to CALLBACK function.
ARGS and HEADERS alist arguments are part of the POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         (append '(("Content-Type" . "application/x-www-form-urlencoded")) headers))
        (url-request-data
         (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg))
                              "="
                              (url-hexify-string (cdr arg))))
                    args
                    "&")))
    (url-retrieve url callback)))

(defun mastodon--response-buffer ()
  "Capture response buffer content as string."
  (with-current-buffer (current-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mastodon--response-body-substring (pattern)
  "Return substring matching PATTERN from `mastodon--response-buffer'."
  (let ((resp (mastodon--response-buffer)))
    (progn
      (string-match pattern resp)
      (match-string 0 resp))))

(defun mastodon--response-match-p (pattern)
  "Return non-nil if `mastodon--response-buffer' matches PATTERN."
  (let ((resp (mastodon--response-buffer)))
    (string-match-p pattern resp)))

(defun mastodon--response-status-p ()
  "Return non-nil if `mastodon--response-buffer' has an HTTP Response Status-Line."
  (when (mastodon--response-match-p "^HTTP/1.*$") t))

(defun mastodon--response-json ()
  "Return string of JSON response body from `mastodon--response-buffer'."
  (mastodon--response-body-substring "\{.*\}"))

(defun mastodon--response-code ()
  "Return HTTP Response Status Code from `mastodon--response-buffer'."
  (let* ((status-line (mastodon--response-body-substring "^HTTP/1.*$")))
    (progn
      (string-match "[0-9][0-9][0-9]" status-line)
      (match-string 0 status-line))))

(defun mastodon--json-hash-table ()
  "Read JSON from `mastodon--response-json' into a hash table."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-from-string (mastodon--response-json))))

(defun mastodon--http-response-triage (status success)
  "Callback function to triage an HTTP response.

Recursively waits for `mastodon--response-buffer' to contain a Status-Line.

STATUS is passed by `url-retrieve'.
SUCCESS is a function called on a 2XX level response code.
If response code is not 2XX, switches to the response buffer created by `url-retrieve'."
  (when (not (mastodon--response-status-p))
    (mastodon--http-response-triage status))
  (if (string-prefix-p "2" (mastodon--response-code))
      (funcall success)
    (switch-to-buffer (current-buffer))))

(defun mastodon-http--triage (response success)
  (let ((status (with-current-buffer response
                  (mastodon--response-code))))
    (if (string-prefix-p "2" status)
        (funcall success)
      (switch-to-buffer response))))

(defun mastodon-http--post (url args headers)
  "POST synchronously to URL with ARGS and HEADERS.

Authorization header is included by default."
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
         `(("Authorization" . ,(concat "Bearer " mastodon-auth--token))
           ,headers)))
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
           (let ((json-string (buffer-substring-no-properties (point) (point-max))))
             (json-read-from-string json-string)))))
    json-vector))

(provide 'mastodon-http)
;;; mastodon-http.el ends here
