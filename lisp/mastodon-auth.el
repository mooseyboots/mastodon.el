(defun mastodon--read-access-token-file ()
  (let* ((plstore (plstore-open mastodon-token-file))
         (plist (cdr (plstore-get plstore "mastodon"))))
    (if (plist-get plist :access_token)
        plist)))

(defun mastodon--access-token-p ()
  (if (mastodon--get-client-token)
      t))

(defun mastodon--get-client-token ()
  (plist-get mastodon--client-plist :access_token))

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
