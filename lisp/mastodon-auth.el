;;; mastodon-auth.el --- Auth functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.9.0
;; Homepage: https://github.com/jdenen/mastodon.el
;; Package-Requires: ((emacs "24.4"))

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
(require 'auth-source)
(require 'json)

(autoload 'mastodon-client "mastodon-client")
(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")
(defvar mastodon-instance-url)

(defgroup mastodon-auth nil
  "Authenticate with Mastodon."
  :prefix "mastodon-auth-"
  :group 'mastodon)

(defcustom mastodon-auth-source-file ""
  "Filename to use to store user names and passwords.

Leave empty to not permanently store any secrets.
Otherwise set to e.g. \"~/.authinfo.gpg\" to have encrypted storage, or
if you are happy with unencryped storage use e.g. \"~/authinfo\"."
  :group 'mastodon-auth
  :type 'string)

(defvar mastodon-auth--token-alist nil
  "Alist of User access tokens keyed by instance url.")

(defvar mastodon-auth--acct-alist nil
  "Alist of account accts (name@domain) keyed by instance url.")

(defun mastodon-auth--generate-token ()
  "Make POST to generate auth token."
  (if (or (null mastodon-auth-source-file)
	  (string= "" mastodon-auth-source-file))
      (mastodon-auth--generate-token-no-storing-credentials)
    (mastodon-auth--generate-token-and-store)))

(defun mastodon-auth--generate-token-no-storing-credentials ()
  "Make POST to generate auth token."
  (mastodon-http--post
   (concat mastodon-instance-url "/oauth/token")
   `(("client_id" . ,(plist-get (mastodon-client) :client_id))
     ("client_secret" . ,(plist-get (mastodon-client) :client_secret))
     ("grant_type" . "password")
     ("username" . ,(read-string "Email: " user-mail-address))
     ("password" . ,(read-passwd "Password: "))
     ("scope" . "read write follow"))
   nil
   :unauthenticated))

(defun mastodon-auth--generate-token-and-store ()
  "Make POST to generate auth token.

Reads and/or stores secres in `MASTODON-AUTH-SOURCE-FILE'."
  (let* ((auth-sources (list mastodon-auth-source-file))
	 (auth-source-creation-prompts
          '((user . "Enter email for %h: ")
            (secret . "Password: ")))
         (credentials-plist (nth 0 (auth-source-search
                                    :create t
                                    :host mastodon-instance-url
                                    :port 443
                                    :require '(:user :secret)))))
    (prog1
        (mastodon-http--post
         (concat mastodon-instance-url "/oauth/token")
         `(("client_id" . ,(plist-get (mastodon-client) :client_id))
           ("client_secret" . ,(plist-get (mastodon-client) :client_secret))
           ("grant_type" . "password")
           ("username" . ,(plist-get credentials-plist :user))
           ("password" . ,(let ((secret (plist-get credentials-plist :secret)))
                            (if (functionp secret)
                                (funcall secret)
                              secret)))
           ("scope" . "read write follow"))
         nil
	 :unauthenticated)
      (when (functionp (plist-get credentials-plist :save-function))
        (funcall (plist-get credentials-plist :save-function))))))

(defun mastodon-auth--get-token ()
  "Make auth token request and return JSON response."
  (with-current-buffer (mastodon-auth--generate-token)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (let ((json-object-type 'plist)
          (json-key-type 'keyword)
          (json-array-type 'vector)
          (json-string (buffer-substring-no-properties (point) (point-max))))
      (json-read-from-string json-string))))

(defun mastodon-auth--access-token ()
  "Return the access token to use with the current `mastodon-instance-url'.

Generate token and set if none known yet."
  (if-let ((token (cdr (assoc mastodon-instance-url mastodon-auth--token-alist))))
      token

    (mastodon-auth--handle-token-response (mastodon-auth--get-token))))

(defun mastodon-auth--handle-token-response (response)
  (pcase response
    ((and (let token (plist-get response :access_token))
          (guard token))
     (cdar (push (cons mastodon-instance-url token)
                 mastodon-auth--token-alist)))

    (`(:error ,class :error_description ,error)
     (error "mastodon-auth--access-token: %s: %s" class error))

    (_ (error "Unknown response from mastodon-auth--get-token!"))))

(defun mastodon-auth--get-account-name ()
  "Request user credentials and return an account name."
  (cdr (assoc
        'acct
        (mastodon-http--get-json
         (mastodon-http--api
          "accounts/verify_credentials")))))

(defun mastodon-auth--user-acct ()
  "Return a mastodon user acct name."
  (or (cdr (assoc  mastodon-instance-url mastodon-auth--acct-alist))
      (let ((acct (mastodon-auth--get-account-name)))
        (push (cons mastodon-instance-url acct) mastodon-auth--acct-alist)
        acct)))

(provide 'mastodon-auth)
;;; mastodon-auth.el ends here
