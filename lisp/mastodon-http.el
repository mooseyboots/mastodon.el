(require 'mastodon)

(defun mastodon--api-for (endpoint)
  "Returns Mastondon API URL for ENDPOINT."
  (concat mastodon-instance-url "/api/" mastodon--api-version "/" endpoint))

(defun mastodon--http-post (url callback args &optional headers)
  "Sends ARGS to URL as a POST request.

Response buffer is passed to the CALLBACK function."
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
  "Returns substring matching PATTERN from `mastodon--response-buffer'."
  (let ((resp (mastodon--response-buffer)))
    (progn
      (string-match pattern resp)
      (match-string 0 resp))))

(defun mastodon--response-match-p (pattern)
  "Returns non-nil if `mastodon--response-buffer' matches PATTERN."
  (let ((resp (mastodon--response-buffer)))
    (string-match-p pattern resp)))

(defun mastodon--response-status-p ()
  "Returns non-nil if `mastodon--response-buffer' has an HTTP Response Status-Line."
  (when (mastodon--response-match-p "^HTTP/1.*$") t))

(defun mastodon--response-json ()
  "Returns string of JSON response body from `mastodon--response-buffer'."
  (mastodon--response-body-substring "\{.*\}"))

(defun mastodon--response-code ()
  "Returns HTTP Response Status Code from `mastodon--response-buffer'."
  (let* ((status-line (mastodon--response-body-substring "^HTTP/1.*$")))
    (progn
      (string-match "[0-9][0-9][0-9]" status-line)
      (match-string 0 status-line))))

(defun mastodon--json-hash-table ()
  "Reads JSON from `mastodon--response-json' into a hash table."
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

(provide 'mastodon-http)
