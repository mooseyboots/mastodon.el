;;; mastodon-client.el --- Client functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.7.1
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

;; mastodon-client.el supports registering the Emacs client with your Mastodon instance.

;;; Code:

(require 'plstore)
(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")


(defcustom mastodon-client--token-file (concat user-emacs-directory "mastodon.plstore")
  "File path where Mastodon access tokens are stored."
  :group 'mastodon
  :type 'file)

(defvar mastodon-client--client-details nil
  "Client id and secret.")

(defun mastodon-client--register ()
  "POST client to Mastodon."
  (mastodon-http--post
   (mastodon-http--api "apps")
   '(("client_name" . "mastodon.el")
     ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
     ("scopes" . "read write follow")
     ("website" . "https://github.com/jdenen/mastodon.el"))
   nil))

(defun mastodon-client--fetch ()
  "Return JSON from `mastodon-client--register' call."
  (with-current-buffer (mastodon-client--register)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (let ((json-object-type 'plist)
          (json-key-type 'keyword)
          (json-array-type 'vector)
          (json-string (buffer-substring-no-properties (point) (point-max))))
      (json-read-from-string json-string))))

(defun mastodon-client--token-file ()
  "Return `mastodon-client--token-file'."
  mastodon-client--token-file)

(defun mastodon-client--store ()
  "Store client_id and client_secret in `mastodon-client--token-file'.

Make `mastodon-client--fetch' call to determine client values."
  (let ((plstore (plstore-open (mastodon-client--token-file)))
        (client (mastodon-client--fetch)))
    (plstore-put plstore "mastodon" client nil)
    (plstore-save plstore)
    (plstore-close plstore)
    client))

(defun mastodon-client--read ()
  "Retrieve client_id and client_secret from `mastodon-client--token-file'."
  (let* ((plstore (plstore-open (mastodon-client--token-file)))
         (mastodon (plstore-get plstore "mastodon")))
    (when mastodon
      (delete "mastodon" mastodon))))

(defun mastodon-client ()
  "Return variable `mastodon-client--client-details' plist.

Read plist from `mastodon-client--token-file' if variable is nil.
Fetch and store plist if `mastodon-client--read' returns nil."
  (or mastodon-client--client-details
      (setq mastodon-client--client-details
            (or (mastodon-client--read)
                (mastodon-client--store)))))

(provide 'mastodon-client)
;;; mastodon-client.el ends here
