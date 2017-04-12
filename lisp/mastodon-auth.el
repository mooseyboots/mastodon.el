;;; mastodon-auth.el --- Auth functions for mastodon.el

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

(defun mastodon-auth--token-file ()
  "Returns `mastodon-token-file' string."
  mastodon-token-file)

(defun mastodon-auth--registration-success ()
  (let ((client-data (mastodon--json-hash-table)))
    (setq mastodon--client-app-plist
          `(:client_id
            ,(gethash "client_id" client-data)
            :client_secret
            ,(gethash "client_secret" client-data)))))

(defun mastodon--register-client-app-triage (status)
  "Callback function to triage `mastodon--register-client-app' response.

STATUS is passed by `url-retrieve'."
  (mastodon--http-response-triage status
                                  'mastodon-auth--registration-success))

(defun mastodon--register-client-app ()
  "Add `:client_id' and `client_secret' to `mastodon--client-plist'."
  (mastodon--http-post (mastodon--api-for "apps")
                       'mastodon--register-client-app-triage
                       '(("client_name" . "mastodon.el")
                         ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                         ("scopes" . "read write follow")
                         ("website" . "https://github.com/jdenen/mastodon.el"))))

(defun mastodon-auth--client-app-secret-p ()
  "Return t if `mastodon--client-app-plist' has a :client_secret value."
  (when (plist-get mastodon--client-app-plist :client_secret) t))

(defun mastodon-auth--client ()
  "Return `mastodon--client-app-plist' value."
  mastodon--client-app-plist)

(defun mastodon--register-and-return-client-app ()
  "Register `mastodon' with an instance. Return `mastodon--client-app-plist'."
  (if (mastodon-auth--client-app-secret-p)
      (mastodon-auth--client)
    (progn
      (mastodon--register-client-app)
      (sleep-for 2)
      (mastodon--register-and-return-client-app))))

(defun mastodon--store-client-id-and-secret ()
  "Store `:client_id' and `:client_secret' in a plstore."
  (let ((client-plist (mastodon--register-and-return-client-app))
        (plstore (plstore-open (mastodon-auth--token-file))))
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
    (let* ((plstore (plstore-open (mastodon-auth--token-file)))
           (mastodon (plstore-get plstore "mastodon")))
      (if mastodon
          (progn
            (setq mastodon--client-app-plist (delete "mastodon" mastodon))
            mastodon--client-app-plist)
        (progn
          (setq mastodon--client-app-plist (mastodon--store-client-id-and-secret))
          mastodon--client-app-plist)))))

(defun mastodon-auth--get-token-success ()
  (let ((token-data (mastodon--json-hash-table)))
    (progn
      (setq mastodon--api-token-string (gethash "access_token" token-data))
      mastodon--api-token-string)))

(defun mastodon--get-access-token-triage (status)
  "Callback function to triage `mastodon--get-access-token' response.

STATUS is passed by `url-retrieve'."
  (mastodon--http-response-triage status
                                  'mastodon-auth--get-token-success))

(defun mastodon-auth--user-and-passwd ()
  "Prompt for user email and password."
  (let ((email (read-string "Email: "))
        (passwd (read-passwd "Password: ")))
    (cons email passwd)))

(defun mastodon--get-access-token ()
  "Retrieve access token from instance.

Authenticates with email address and password. Neither are not stored."
  (let* ((creds (mastodon-auth--user-and-passwd))
         (email (car creds))
         (passwd (cdr creds)))
    (mastodon--http-post (concat mastodon-instance-url "/oauth/token")
                         'mastodon--get-access-token-triage
                         `(("client_id" . ,(plist-get (mastodon--client-app) :client_id))
                           ("client_secret" . ,(plist-get (mastodon--client-app) :client_secret))
                           ("grant_type" . "password")
                           ("username" . ,email)
                           ("password" . ,passwd)
                           ("scope" . "read write follow")))))

(defun mastodon-auth--token ()
  "Return `mastodon--api-token-string'."
  mastodon--api-token-string)

(defun mastodon--access-token ()
  "Return `mastodon--api-token-string'.

If not set, retrieves token with `mastodon--get-access-token'."
  (or (mastodon-auth--token)
      (progn
        (mastodon--get-access-token)
        (or (mastodon-auth--token)
            (progn
              (sleep-for 2)
              (mastodon--access-token))))))

(provide 'mastodon-auth)
;;; mastodon-auth.el ends here
