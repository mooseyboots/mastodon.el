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

(defun mastodon--response-json (status)
  "Returns JSON string from `mastodon--http-post' response buffer."
  (let ((resp (with-current-buffer (current-buffer)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (progn
      (string-match "\{.*\}" resp)
      (match-string 0 resp))))

(defun mastodon--json-hash-table (status)
  "Reads JSON string from `mastodon--response-json' into a hash table."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-from-string (mastodon--response-json status))))
