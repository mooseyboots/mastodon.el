;;; mastodon.el --- Client for Mastodon  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.9.1
;; Package-Requires: ((emacs "25.1") (request "0.2.0") (seq "1.8"))
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
(require 'cl-lib) ; for some call in mastodon

(declare-function discover-add-context-menu "discover")
(declare-function emojify-mode "emojify")
(declare-function request "request")
(autoload 'special-mode "simple")
(autoload 'mastodon-tl--get-federated-timeline "mastodon-tl")
(autoload 'mastodon-tl--get-home-timeline "mastodon-tl")
(autoload 'mastodon-tl--get-local-timeline "mastodon-tl")
(autoload 'mastodon-tl--get-tag-timeline "mastodon-tl")
(autoload 'mastodon-tl--goto-next-toot "mastodon-tl")
(autoload 'mastodon-tl--goto-prev-toot "mastodon-tl")
(autoload 'mastodon-tl--next-tab-item "mastodon-tl")
(autoload 'mastodon-tl--previous-tab-item "mastodon-tl")
(autoload 'mastodon-tl--thread "mastodon-tl")
(autoload 'mastodon-tl--toggle-spoiler-text-in-toot "mastodon-tl")
(autoload 'mastodon-tl--update "mastodon-tl")
(autoload 'mastodon-notifications--get "mastodon-notifications")
(autoload 'mastodon-profile--get-toot-author "mastodon-profile")
(autoload 'mastodon-profile--make-author-buffer "mastodon-profile")
(autoload 'mastodon-profile--show-user "mastodon-profile")
(autoload 'mastodon-toot--compose-buffer "mastodon-toot")
(autoload 'mastodon-toot--reply "mastodon-toot")
(autoload 'mastodon-toot--toggle-boost "mastodon-toot")
(autoload 'mastodon-toot--toggle-favourite "mastodon-toot")
(autoload 'mastodon-discover "mastodon-discover")

(autoload 'mastodon-tl--block-user "mastodon-tl")
(autoload 'mastodon-tl--unblock-user "mastodon-tl")
(autoload 'mastodon-tl--mute-user "mastodon-tl")
(autoload 'mastodon-tl--unmute-user "mastodon-tl")
(autoload 'mastodon-tl--follow-user "mastodon-tl")
(autoload 'mastodon-tl--unfollow-user "mastodon-tl")
(autoload 'mastodon-profile--my-profile "mastodon-profile")
(autoload 'mastodon-profile--view-favourites "mastodon-profile")
(autoload 'mastodon-profile--view-follow-requests "mastodon-profile")
(autoload 'mastodon-search--search-query "mastodon-search")
(autoload 'mastodon-toot--delete-toot "mastodon-toot")
(autoload 'mastodon-toot--copy-toot-url "mastodon-toot")
(autoload 'mastodon-toot--pin-toot-toggle "mastodon-toot")
(autoload 'mastodon-auth--get-account-name "mastodon-auth")
(autoload 'mastodon-async--stream-federated "mastodon-async")
(autoload 'mastodon-async--stream-local "mastodon-async")
(autoload 'mastodon-async--stream-home "mastodon-async")
(autoload 'mastodon-async--stream-notifications "mastodon-async")
(autoload 'mastodon-profile--update-user-profile-note "mastodon-profile")

(defgroup mastodon nil
  "Interface with Mastodon."
  :prefix "mastodon-"
  :group 'external)

(defcustom mastodon-instance-url "https://mastodon.social"
  "Base URL for the Masto instance from which you toot."
  :group 'mastodon
  :type 'string)

(defcustom mastodon-toot-timestamp-format "%F %T"
  "Format to use for timestamps.

For valid formatting options see `format-time-string`.
The default value \"%F %T\" prints ISO8601-style YYYY-mm-dd HH:MM:SS.
Use. e.g. \"%c\" for your locale's date and time format."
  :group 'mastodon
  :type 'string)

(defvar mastodon-mode-map
  (let ((map (make-sparse-keymap)))
    ;; navigation inside a timeline
    (define-key map (kbd "n") #'mastodon-tl--goto-next-toot)
    (define-key map (kbd "p") #'mastodon-tl--goto-prev-toot)
    (define-key map (kbd "M-n") #'mastodon-tl--next-tab-item)
    (define-key map (kbd "M-p") #'mastodon-tl--previous-tab-item)
    (define-key map [?\t] #'mastodon-tl--next-tab-item)
    (define-key map [backtab] #'mastodon-tl--previous-tab-item)
    (define-key map [?\S-\t] #'mastodon-tl--previous-tab-item)
    (define-key map [?\M-\t] #'mastodon-tl--previous-tab-item)
    ;; navigation between timelines
    (define-key map (kbd "#") #'mastodon-tl--get-tag-timeline)
    (define-key map (kbd "A") #'mastodon-profile--get-toot-author)
    (define-key map (kbd "F") #'mastodon-tl--get-federated-timeline)
    (define-key map (kbd "H") #'mastodon-tl--get-home-timeline)
    (define-key map (kbd "L") #'mastodon-tl--get-local-timeline)
    (define-key map (kbd "N") #'mastodon-notifications--get)
    (define-key map (kbd "P") #'mastodon-profile--show-user)
    (define-key map (kbd "T") #'mastodon-tl--thread)
    ;; navigation out of mastodon
    (define-key map (kbd "q") #'kill-this-buffer)
    (define-key map (kbd "Q") #'kill-buffer-and-window)
    ;; timeline actions
    (define-key map (kbd "b") #'mastodon-toot--toggle-boost)
    (define-key map (kbd "c") #'mastodon-tl--toggle-spoiler-text-in-toot)
    (define-key map (kbd "f") #'mastodon-toot--toggle-favourite)
    (define-key map (kbd "r") #'mastodon-toot--reply)
    (define-key map (kbd "u") #'mastodon-tl--update)
    ;; new toot
    (define-key map (kbd "t") #'mastodon-toot)
    ;; override special mode binding
    (define-key map (kbd "g") #'undefined)
    ;; mousebot additions
    (define-key map (kbd "W") #'mastodon-tl--follow-user)
    (define-key map (kbd "C-S-W") #'mastodon-tl--unfollow-user)
    (define-key map (kbd "B") #'mastodon-tl--block-user)
    (define-key map (kbd "C-S-B") #'mastodon-tl--unblock-user)
    (define-key map (kbd "M") #'mastodon-tl--mute-user)
    (define-key map (kbd "C-S-M") #'mastodon-tl--unmute-user)
    (define-key map (kbd "C-S-P") #'mastodon-profile--my-profile)
    (define-key map (kbd "S") #'mastodon-search--search-query)
    (define-key map (kbd "d") #'mastodon-toot--delete-toot)
    (define-key map (kbd "C") #'mastodon-toot--copy-toot-url)
    (define-key map (kbd "i") #'mastodon-toot--pin-toot-toggle)
    (define-key map (kbd "v") #'mastodon-profile--view-favourites)
    (define-key map (kbd "R") #'mastodon-profile--view-follow-requests)
    (define-key map (kbd "C-c h") #'mastodon-async--stream-home)
    (define-key map (kbd "C-c f") #'mastodon-async--stream-federated)
    (define-key map (kbd "C-c l") #'mastodon-async--stream-local)
    (define-key map (kbd "C-c n") #'mastodon-async--stream-notifications)
    (define-key map (kbd "U") #'mastodon-profile--update-user-profile-note)
    map)

  "Keymap for `mastodon-mode'.")

(defcustom mastodon-mode-hook nil
  "Hook run when entering Mastodon mode."
  :type 'hook
  :options '(provide-discover-context-menu)
  :group 'mastodon)

(defface mastodon-handle-face
  '((t :inherit default))
  "Face used for user display names.")

(defface mastodon-display-name-face
  '((t :inherit warning))
  "Face used for user display names.")

(defface mastodon-boosted-face
  '((t :inherit success :weight bold))
  "Face to indicate that a toot is boosted.")

(defface mastodon-boost-fave-face
  '((t :inherit success))
  "Face to indicate that you have boosted or favourited a toot.")

(defface mastodon-cw-face
  '((t :inherit success))
  "Face used for content warning.")

;;;###autoload
(defun mastodon ()
  "Connect Mastodon client to `mastodon-instance-url' instance."
  (interactive)
  (let* ((tls (list "home"
                    "local"
                    "federated"
                    (concat (mastodon-auth--get-account-name) "-statuses") ; profile
                    "favourites"
                    "search"))
         (buffer (cl-some (lambda (el)
                           (get-buffer (concat "*mastodon-" el "*")))
                         tls))) ; return first buff that exists
    (if buffer
        (switch-to-buffer buffer)
      (mastodon-tl--get-home-timeline)
      (message "Loading Mastodon account %s on %s..." (mastodon-auth--get-account-name) mastodon-instance-url))))

;;;###autoload
(defun mastodon-toot (&optional user reply-to-id)
  "Update instance with new toot. Content is captured in a new buffer.

If USER is non-nil, insert after @ symbol to begin new toot.
If REPLY-TO-ID is non-nil, attach new toot to a conversation."
  (interactive)
  (mastodon-toot--compose-buffer user reply-to-id))

;;;###autoload
(add-hook 'mastodon-mode-hook (lambda ()
                                (when (require 'emojify nil :noerror)
                                  (emojify-mode t))))

(define-derived-mode mastodon-mode special-mode "Mastodon"
  "Major mode for Mastodon, the federated microblogging network."
  :group 'mastodon
  (read-only-mode 1))

(provide 'mastodon)
;;; mastodon.el ends here
