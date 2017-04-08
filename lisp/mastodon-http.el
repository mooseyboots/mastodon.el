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
  (with-current-buffer (current-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mastodon--response-body-substring (pattern)
  (let ((resp (mastodon--response-buffer)))
    (progn
      (string-match pattern resp)
      (match-string 0 resp))))

(defun mastodon--response-match-p (pattern)
  (let ((resp (mastodon--response-buffer)))
    (string-match-p pattern resp)))

(defun mastodon--response-status-p ()
  (when (mastodon--response-match-p "^HTTP/1.*$") t))

(defun mastodon--response-json ()
    (mastodon--response-body-substring "\{.*\}"))

(defun mastodon--response-code ()
  (let* ((status-line (mastodon--response-body-substring "^HTTP/1.*$")))
         (progn
           (string-match "[0-9][0-9][0-9]" status-line)
           (match-string 0 status-line))))

(defun mastodon--json-hash-table ()
  "Reads JSON string from `mastodon--response-json' into a hash table."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-from-string (mastodon--response-json))))

(defun mastodon--http-response-triage (status success)
  (when (not (mastodon--response-status-p))
    (mastodon--http-response-triage status))
  (if (string-prefix-p "2" (mastodon--response-code))
      (funcall success)
    (switch-to-buffer (current-buffer))))

(provide 'mastodon-http)
