(load-file "mastodon-auth.el")
(load-file "mastodon-http.el")

(defvar mastodon--api-version "v1")

(defcustom mastodon-instance-url "https://mastodon.social"
  "Base URL for the Masto instance from which you toot."
  :group 'mastodon
  :type 'string)

;; TODO
(defcustom mastodon-token-file (concat user-emacs-directory "mastodon.plstore")
  "File path where Mastodon access tokens are stored."
  :group 'mastodon
  :type 'file)

(defvar mastodon--client-plist (mastodon--read-access-token-file)
  "Stores CLIENT_ID, CLIENT_SECRET, and ACCESS_TOKEN.

Reads values from `mastodon-token-file' if they exist.")

(defvar mastodon--token (mastodon--read-or-get-access-token)
  "API token for Mastodon.")
