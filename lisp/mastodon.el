;;; mastodon.el --- Client for Mastodon  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.9.0
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
(declare-function discover-add-context-menu "discover")
(declare-function emojify-mode "emojify")
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

(defcustom mastodon-auth-mechanism 'oauth2
  "Mechanism to be used to authenticate with Mastodon."
  :type '(choice
          (const :tag "plain" plain)
          (const :tag "oauth2" oauth2))
  :group 'mastodon)

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
  '((t :inherit highlight :weight bold))
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
  (mastodon-tl--get-home-timeline)
  (message "Loading Mastodon account %s on %s..." (mastodon-auth--get-account-name) mastodon-instance-url))

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
