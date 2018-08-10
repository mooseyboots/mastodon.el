;;; mastodon-client.el --- Client functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.8.0
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
(defvar mastodon-instance-url)
(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")


(defcustom mastodon-client--token-file (concat user-emacs-directory "mastodon.plstore")
  "File path where Mastodon access tokens are stored."
  :group 'mastodon
  :type 'file)

(defvar mastodon-client--client-details-alist nil
  "An alist of Client id and secrets keyed by the instance url.")

(defun mastodon-client--register ()
  "POST client to Mastodon."
  (mastodon-http--post
   (mastodon-http--api "apps")
   '(("client_name" . "mastodon.el")
     ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
     ("scopes" . "read write follow")
     ("website" . "https://github.com/jdenen/mastodon.el"))
   nil
   :unauthenticated))

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
	(client (mastodon-client--fetch))
	;; alexgriffith reported seeing ellipses in the saved output
	;; which indicate some output truncating. Nothing in `plstore-save'
	;; seems to ensure this cannot happen so let's do that ourselves:
	(print-length nil)
	(print-level nil))
    (plstore-put plstore (concat "mastodon-" mastodon-instance-url) client nil)
    (plstore-save plstore)
    (plstore-close plstore)
    client))

(defun mastodon-client--read ()
  "Retrieve client_id and client_secret from `mastodon-client--token-file'."
  (let* ((plstore (plstore-open (mastodon-client--token-file)))
         (mastodon (plstore-get plstore (concat "mastodon-" mastodon-instance-url))))
    (cdr mastodon)))

(defun mastodon-client ()
  "Return variable client secrets to use for the current `mastodon-instance-url'..

Read plist from `mastodon-client--token-file' if variable is nil.
Fetch and store plist if `mastodon-client--read' returns nil."
  (let ((client-details
         (cdr (assoc mastodon-instance-url mastodon-client--client-details-alist))))
    (unless client-details
      (setq client-details
            (or (mastodon-client--read)
                (mastodon-client--store)))
      (push (cons mastodon-instance-url client-details)
            mastodon-client--client-details-alist))
    client-details))

(provide 'mastodon-client)
;;; mastodon-client.el ends here

