(defgroup mastodon nil
  "Interface with Mastodon.")

(defcustom mastodon-instance-url "https://mastodon.social"
  "Base URL for the Masto instance from which you toot."
  :group 'mastodon
  :type 'string)

;; TODO
(defcustom mastodon-token-file (concat user-emacs-directory "mastodon.plstore")
  "File path where Mastodon access tokens are stored."
  :group 'mastodon
  :type 'file)

(defvar mastodon--api-version "v1")

;;;###autoload
(defun mastodon ()
  (interactive)
  (load-file "mastodon-http.el")
  (load-file "mastodon-auth.el")
  (load-file "mastodon-toot.el"))

(provide 'mastodon)
