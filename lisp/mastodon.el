;;; mastodon.el -- Mastodon client for Emacs

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.1.0
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

;; mastodon.el is an Emacs client for Mastodon <https://github.com/tootsuite/mastodon>,
;; the federated microblogging social network. It is very much a work-in-progress, but
;; it is a labor of love.

;;; Code:

(require 'mastodon-auth)

(defgroup mastodon nil
  "Interface with Mastodon."
  :prefix "mastodon-"
  :group 'external)

(defcustom mastodon-instance-url "https://mastodon.social"
  "Base URL for the Masto instance from which you toot."
  :group 'mastodon
  :type 'string)

(defcustom mastodon-token-file (concat user-emacs-directory "mastodon.plstore")
  "File path where Mastodon access tokens are stored."
  :group 'mastodon
  :type 'file)

(defvar mastodon-mode-map
  (make-sparse-keymap)
  "Keymap for `mastodon-mode'.")

(defvar mastodon--api-version "v1")

;; FIXME #25
(defun mastodon-version ()
  "Message package version."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "../.version")
    (message
     (concat "mastodon.el v" (buffer-string)))))

;;;###autoload
(defun mastodon ()
  (interactive)
  (require 'mastodon-tl)
  (mastodon-tl--get "home"))

;;;###autoload
(defun mastodon-toot ()
  "Update a Mastodon instance with new toot. Content is captured in a new buffer."
  (interactive)
  (require 'mastodon-toot)
  (progn
    (switch-to-buffer-other-window (get-buffer-create "*new toot*"))
    (mastodon-toot-mode t)))

;;;###autoload
(defun mastodon-register ()
  "Registers mastodon.el with the Mastodon instance."
  (interactive)
  (progn
    (mastodon--store-client-id-and-secret)))

(define-derived-mode mastodon-mode nil "Mastodon"
  "Major mode for Mastodon, the federated microblogging network."
  :group 'mastodon
  (let ((map mastodon-mode-map))
    (define-key map (kbd "F") (lambda () (interactive) (mastodon-tl--get "public")))
    (define-key map (kbd "H") (lambda () (interactive) (mastodon-tl--get "home")))
    (define-key map (kbd "L") (lambda () (interactive) (mastodon-tl--get "public?local=true")))
    (define-key map (kbd "n") #'mastodon-toot)
    (define-key map (kbd "q") #'kill-this-buffer)
    (define-key map (kbd "Q") #'kill-buffer-and-window)
    (define-key map (kbd "T") (lambda () (interactive)
                                (let ((tag (read-string "Tag: ")))
                                  (mastodon-tl--get (concat "tag/" tag)))))))

(provide 'mastodon)
;;; mastodon.el ends here
