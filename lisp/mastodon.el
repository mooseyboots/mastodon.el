;;; mastodon.el -- Mastodon client for Emacs

;;; Commentary:

;; mastodon.el is an Emacs client for Mastodon, the federated microblogging
;; social network. It is very much a work-in-progress, but it is a labor of
;; love.

;;; Code:

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

(defvar mastodon--api-version "v1")

;;;###autoload
(defun mastodon-toot ()
  "Update a Mastodon instance with new toot. Content is captured in a new buffer."
  (interactive)
  (progn
    (require 'mastodon-toot)
    (switch-to-buffer-other-window (get-buffer-create "*new toot*"))
    (mastodon-toot-mode t)))

;;;###autoload
(defun mastodon-register ()
  "Registers mastodon.el with the Mastodon instance."
  (interactive)
  (progn
    (require 'mastodon-auth)
    (mastodon--store-client-id-and-secret)))

(provide 'mastodon)
;;; mastodon.el ends here
