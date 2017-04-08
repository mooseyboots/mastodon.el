(defgroup mastodon nil
  "Interface with Mastodon.")

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
  "Updates a Mastodon instance with new toot. Content is captured in a new buffer."
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
