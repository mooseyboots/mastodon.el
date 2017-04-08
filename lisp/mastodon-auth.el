(require 'mastodon)
(require 'mastodon-http)

(defgroup mastodon-auth nil
  "Authenticate with Mastodon."
  :group 'mastodon)

(defvar mastodon--client-app-plist nil)
(defvar mastodon--api-token-string nil)

(defun mastodon--register-client-app-triage (status)
  (mastodon--http-response-triage status
                                  (lambda () (let ((client-data (mastodon--json-hash-table)))
                                               (setq mastodon--client-app-plist
                                                     `(:client_id
                                                       ,(gethash "client_id" client-data)
                                                       :client_secret
                                                       ,(gethash "client_secret" client-data)))))))

(defun mastodon--register-client-app ()
  "Adds client_id and client_secret to `mastodon--client-plist'.

  Returns a `plist' with CLIENT_ID and CLIENT_SECRET."
  (mastodon--http-post (mastodon--api-for "apps")
                       'mastodon--register-client-app-triage
                       '(("client_name" . "mastodon.el")
                         ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                         ("scopes" . "read write follow"))))

(defun mastodon--register-and-return-client-app ()
  (progn
    (mastodon--register-client-app)
    mastodon--client-app-plist))

(defun mastodon--store-client-id-and-secret ()
  (let ((client-plist (mastodon--register-and-return-client-app))
        (plstore (plstore-open mastodon-token-file)))
    (plstore-put plstore "mastodon" `(:client_id
                                      ,(plist-get client-plist :client_id)
                                      :client_secret
                                      ,(plist-get client-plist :client_secret))
                 nil)
    (plstore-save plstore)
    client-plist))

(defun mastodon--client-app ()
  (if (plist-get mastodon--client-app-plist :client_secret)
      mastodon--client-app-plist
    (let* ((plstore (plstore-open mastodon-token-file))
           (mastodon (plstore-get plstore "mastodon")))
      (if mastodon
          (progn
            (setq mastodon--client-app-plist (delete "mastodon" mastodon))
            mastodon--client-app-plist)
        (progn
          (setq mastodon--client-app-plist (mastodon--store-client-id-and-secret))
          mastodon--client-app-plist)))))

(defun mastodon--get-access-token-triage (status)
  (mastodon--http-response-triage status
                                  (lambda ()
                                    (let ((token-data (mastodon--json-hash-table)))
                                      (progn
                                        (setq mastodon--api-token-string (gethash "access_token" token-data))
                                        mastodon--api-token-string)))))

(defun mastodon--get-access-token ()
  (mastodon--http-post (concat mastodon-instance-url "/oauth/token")
                       'mastodon--get-access-token-triage
                       `(("client_id" . ,(plist-get (mastodon--client-app) :client_id))
                         ("client_secret" . ,(plist-get (mastodon--client-app) :client_secret))
                         ("grant_type" . "password")
                         ("username" . ,(read-string "Email: "))
                         ("password" . ,(read-passwd "Password: ")))))

(defun mastodon--access-token ()
  (if mastodon--api-token-string
      mastodon--api-token-string
    (progn
      (mastodon--get-access-token)
      (while (not mastodon--api-token-string)
        (sleep-for 1)
        (mastodon--access-token))
      mastodon--api-token-string)))

(provide 'mastodon-auth)
