;;; mastodon-auth.el --- Auth functions for mastodon.el

;;; Commentary:

;; mastodon.el is an Emacs client for Mastodon, the federated microblogging
;; social network. It is very much a work-in-progress, but it is a labor of
;; love.

;; mastodon-auth.el supports authorizing and authenticating with Mastodon.

;;; Code:

(require 'plstore)
(require 'mastodon)
(require 'mastodon-http)

(defgroup mastodon-auth nil
  "Authenticate with Mastodon."
  :prefix "mastodon-auth-"
  :group 'mastodon)

(defvar mastodon--client-app-plist nil)
(defvar mastodon--api-token-string nil)

(defun mastodon--register-client-app-triage (status)
  "Callback function to triage `mastodon--register-client-app' response.

STATUS is passed by `url-retrieve'."
  (mastodon--http-response-triage status
                                  (lambda () (let ((client-data (mastodon--json-hash-table)))
                                               (setq mastodon--client-app-plist
                                                     `(:client_id
                                                       ,(gethash "client_id" client-data)
                                                       :client_secret
                                                       ,(gethash "client_secret" client-data)))))))

(defun mastodon--register-client-app ()
  "Add `:client_id' and `client_secret' to `mastodon--client-plist'."
  (mastodon--http-post (mastodon--api-for "apps")
                       'mastodon--register-client-app-triage
                       '(("client_name" . "mastodon.el")
                         ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                         ("scopes" . "read write follow"))))

(defun mastodon--register-and-return-client-app ()
  "Register `mastodon' with an instance. Return `mastodon--client-app-plist'."
  (progn
    (mastodon--register-client-app)
    mastodon--client-app-plist))

(defun mastodon--store-client-id-and-secret ()
  "Store `:client_id' and `:client_secret' in a plstore."
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
  "Return `mastodon--client-app-plist'.

If not set, retrieves client data from `mastodon-token-file'.
If no data can be found in the token file, registers the app and stores its data via `mastodon--store-client-id-and-secret'."
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
  "Callback function to triage `mastodon--get-access-token' response.

STATUS is passed by `url-retrieve'."
  (mastodon--http-response-triage status
                                  (lambda ()
                                    (let ((token-data (mastodon--json-hash-table)))
                                      (progn
                                        (setq mastodon--api-token-string (gethash "access_token" token-data))
                                        mastodon--api-token-string)))))

(defun mastodon--get-access-token ()
  "Retrieve access token from instance.

Authenticates with email address and password. Neither are not stored."
  (mastodon--http-post (concat mastodon-instance-url "/oauth/token")
                       'mastodon--get-access-token-triage
                       `(("client_id" . ,(plist-get (mastodon--client-app) :client_id))
                         ("client_secret" . ,(plist-get (mastodon--client-app) :client_secret))
                         ("grant_type" . "password")
                         ("username" . ,(read-string "Email: "))
                         ("password" . ,(read-passwd "Password: "))
                         ("scope" . "read write follow"))))

(defun mastodon--access-token ()
  "Return `mastodon--api-token-string'.

If not set, retrieves token with `mastodon--get-access-token'."
  (if mastodon--api-token-string
      mastodon--api-token-string
    (progn
      (mastodon--get-access-token)
      (while (not mastodon--api-token-string)
        (sleep-for 1)
        (mastodon--access-token))
      mastodon--api-token-string)))

(provide 'mastodon-auth)
;;; mastodon-auth.el ends here
