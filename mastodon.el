;;
;; TODO resolve the FIXMEs
;;

(defvar mastodon--api-version "v1")

(defcustom mastodon-instance-url "https://mastodon.social"
  "Base URL for the Masto instance from which you toot."
  :group 'mastodon
  :type 'string)

;; TODO
(defcustom mastodon-token-file (concat user-emacs-directory "mastodon.plstore")
  "File path where Mastodon access tokens are stored."
  :group 'mastodon
  :type 'file)

(defun mastodon--read-access-token-file ()
  (let* ((plstore (plstore-open mastodon-token-file))
         (plist (cdr (plstore-get plstore "mastodon"))))
    (if (plist-get plist :access_token)
        plist)))

(defun mastodon--access-token-p ()
  (if (mastodon--get-client-token)
      t))

(defun mastodon--read-or-get-access-token ()
  (if (mastodon--access-token-p)
      (mastodon--get-client-token)
    (progn
      (mastodon--client-id-and-secret)
      (sleep-for 1) ;; FIXME
      (mastodon--access-token)
      (sleep-for 2) ;; FIXME
      (mastodon--store-access-token-file)
      (mastodon--get-client-token))))

(defun mastodon--get-client-token ()
  (plist-get mastodon--client-plist :access_token))

(defun mastodon--client-id-and-secret ()
  "Adds client_id and client_secret to `mastodon--client-plist'."
  (mastodon--http-post (mastodon--api-for "apps")
                       (lambda (status) (let ((client-data (mastodon--json-hash-table status)))
                                          (setq mastodon--client-plist `(:client_id ,(gethash "client_id" client-data) :client_secret ,(gethash "client_secret" client-data)))))
                       '(("client_name" . "mastodon.el")
                         ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                         ("scopes" . "read write"))))

(defun mastodon--store-access-token-file ()
  (let ((plstore (plstore-open mastodon-token-file)))
    (plstore-put plstore "mastodon" nil
                 `(:client_id
                   ,(plist-get mastodon--client-plist :client_id)
                   :client_secret
                   ,(plist-get mastodon--client-plist :client_secret)
                   :access_token
                   ,(plist-get mastodon--client-plist :access_token)))
    (plstore-save plstore)))

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

(defun mastodon--access-token ()
  "Adds access_token to `mastodon--client-plist'."
  (mastodon--http-post (concat mastodon-instance-url "/oauth/token")
                       (lambda (status) (let ((token-data (mastodon--json-hash-table status)))
                                          (setq mastodon--client-plist (plist-put mastodon--client-plist :access_token (gethash "access_token" token-data)))))
                       `(("client_id" . ,(plist-get mastodon--client-plist :client_id))
                        ("client_secret" . ,(plist-get mastodon--client-plist :client_secret))
                        ("grant_type" . "password")
                        ("username" . ,(read-string "Email: "))
                        ("password" . ,(read-passwd "Password: ")))))

(defvar mastodon--client-plist (mastodon--read-access-token-file)
  "Stores CLIENT_ID, CLIENT_SECRET, and ACCESS_TOKEN.

Reads values from `mastodon-token-file' if they exist.")

(defvar mastodon--token (mastodon--read-or-get-access-token)
  "API token for Mastodon.")
