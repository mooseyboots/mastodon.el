;;; mastodon.el --- Client for Mastodon

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.5.5
;; Package-Requires: ((emacs "24.4"))
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

(require 'mastodon-auth nil t)

(defgroup mastodon nil
  "Interface with Mastodon."
  :prefix "mastodon-"
  :group 'external)

(defconst mastodon-version "0.5.5"
  "Current `mastodon' package version.")

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

(defcustom mastodon-mode-hook nil
  "Hook run when entering Mastodon mode."
  :type 'hook
  :options '(provide-discover-context-menu)
  :group 'mastodon)

(defun mastodon-version ()
  "Message package version."
  (interactive)
  (message "Mastodon version %s" mastodon-version))

;;;###autoload
(defun mastodon ()
  "Connect Mastodon client to `mastodon-instance-url' instance."
  (interactive)
  (require 'mastodon-tl nil t)
  (mastodon-tl--get "home"))

;;;###autoload
(defun mastodon-toot (&optional user reply-to-id)
  "Update instance with new toot. Content is captured in a new buffer.

If USER is non-nil, insert after @ symbol to begin new toot.
If REPLY-TO-ID is non-nil, attach new toot to a conversation."
  (interactive)
  (require 'mastodon-toot nil t)
  (progn
    (switch-to-buffer-other-window (get-buffer-create "*new toot*"))
    (when user
      (insert (format "@%s " user))
      (setq mastodon-toot--reply-to-id reply-to-id))
    (mastodon-toot-mode t)))

;;;###autoload
(defun mastodon-register ()
  "Registers mastodon.el with the Mastodon instance."
  (interactive)
  (progn
    (mastodon--store-client-id-and-secret)))

;;;###autoload
(add-hook 'mastodon-mode-hook (lambda ()
                                (when (require 'emojify nil :noerror)
                                  (emojify-mode t))))

(define-derived-mode mastodon-mode nil "Mastodon"
  "Major mode for Mastodon, the federated microblogging network."
  :group 'mastodon
  (let ((map mastodon-mode-map))
    (define-key map (kbd "b") #'mastodon-toot--boost)
    (define-key map (kbd "f") #'mastodon-toot--favourite)
    (define-key map (kbd "F") #'mastodon-tl--get-federated-timeline)
    (define-key map (kbd "H") #'mastodon-tl--get-home-timeline)
    (define-key map (kbd "j") #'mastodon-tl--goto-next-toot)
    (define-key map (kbd "k") #'mastodon-tl--goto-prev-toot)
    (define-key map (kbd "L") #'mastodon-tl--get-local-timeline)
    (define-key map (kbd "n") #'mastodon-toot)
    (define-key map (kbd "q") #'kill-this-buffer)
    (define-key map (kbd "Q") #'kill-buffer-and-window)
    (define-key map (kbd "r") #'mastodon-toot--reply)
    (define-key map (kbd "t") #'mastodon-tl--thread)
    (define-key map (kbd "T") #'mastodon-tl--get-tag-timeline)
    (define-key map (kbd "u") #'mastodon-tl--update)))

(with-eval-after-load 'mastodon
  (when (require 'discover nil :noerror)
    (discover-add-context-menu
     :bind "?"
     :mode 'mastodon-mode
     :mode-hook 'mastodon-mode-hook
     :context-menu '(mastodon
                     (description "Mastodon feed viewer")
                     (actions
                      ("Toots"
                       ("b" "Boost" mastodon-toot--boost)
                       ("f" "Favourite" mastodon-toot--favourite)
                       ("j" "Next" mastodon-tl--goto-next-toot)
                       ("k" "Prev" mastodon-tl--goto-prev-toot)
                       ("n" "Send" mastodon-toot)
                       ("r" "Reply" mastodon-toot--reply)
                       ("t" "Thread" mastodon-tl--thread)
                       ("u" "Update" mastodon-tl--update))
                      ("Timelines"
                       ("F" "Federated" mastodon-tl--get-federated-timeline)
                       ("H" "Home" mastodon-tl--get-home-timeline)
                       ("L" "Local" mastodon-tl--get-local-timeline)
                       ("T" "Tag" mastodon-tl--get-tag-timeline))
                      ("Quit"
                       ("q" "Quit mastodon buffer. Leave window open." kill-this-buffer)
                       ("Q" "Quit mastodon buffer and kill window." kill-buffer-and-window)))))))

(provide 'mastodon)
;;; mastodon.el ends here
