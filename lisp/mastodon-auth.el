;;; mastodon-auth.el --- Auth functions for mastodon.el

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.5.5
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
(require 'mastodon-client nil t)

(defgroup mastodon-auth nil
  "Authenticate with Mastodon."
  :prefix "mastodon-auth-"
  :group 'mastodon)

(defvar mastodon-auth--token nil
  "User access token.")

(defun mastodon-auth--generate-token ()
  "Make POST to generate auth token."
  (mastodon-http--post
   (concat mastodon-instance-url "/oauth/token")
   `(("client_id" . ,(plist-get (mastodon-client) :client_id))
     ("client_secret" . ,(plist-get (mastodon-client) :client_secret))
     ("grant_type" . "password")
     ("username" . ,(read-string "Email: "))
     ("password" . ,(read-passwd "Password: "))
     ("scope" . "read write follow"))
   nil))

(defun mastodon-auth--get-token ()
  "Make auth token request and return JSON response."
  (with-current-buffer (mastodon-auth--generate-token)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (let ((json-object-type 'plist)
          (json-key-type 'keyword)
          (json-array-type 'vector))
      (json-read))))

(defun mastodon-auth--access-token ()
  "Return `mastodon-auth--token'.

Generate token and set `mastodon-auth--token' if nil."
  (or mastodon-auth--token
      (progn
        (let* ((json (mastodon-auth--get-token))
               (token (plist-get json :access_token)))
          (setq mastodon-auth--token token)))))

(provide 'mastodon-auth)
;;; mastodon-auth.el ends here
