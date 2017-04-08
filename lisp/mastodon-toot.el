(require 'mastodon-auth)
(require 'mastodon-http)

(defgroup mastodon-toot nil
  "Capture Mastodon toots."
  :group 'mastodon)

(defun mastodon-new-toot ()
  "Updates a Mastodon instance with new toot. Content is captured in a new buffer."
  (interactive)
  (progn
    (switch-to-buffer-other-window (get-buffer-create "*new toot*"))
    (mastodon-toot-mode t)))

(defun mastodon-toot--send-triage (status)
  "Callback function to triage toot POST.

STATUS is passed by `url-retrieve'."
  (mastodon--http-response-triage status
                                  (lambda () (switch-to-buffer (current-buffer))))) ;; FIXME

(defun mastodon-toot--send ()
  "Kills new-toot buffer/window and POSTs contents to the Mastodon instance."
  (interactive)
  (let ((toot (buffer-string))
        (endpoint (mastodon--api-for "statuses")))
    (progn
      (kill-buffer-and-window)
      (mastodon--http-post endpoint
                           'mastodon-toot--send-triage
                           `(("status" . ,toot))
                           `(("Authorization" . ,(concat
                                                  "Bearer "
                                                  (mastodon--access-token))))))))

(defun mastodon-toot--cancel ()
  "Kills new-toot buffer/window. Does not POST content to Mastodon."
  (interactive)
  (kill-buffer-and-window))

(defvar mastodon-toot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mastodon-toot--send)
    (define-key map (kbd "C-c C-k") #'mastodon-toot--cancel)
      map)
  "Keymap for `mastodon-toot-mode'.")

(define-minor-mode mastodon-toot-mode
  "Minor mode to capture Mastodon toots."
  :group 'mastodon-toot
  :keymap mastodon-toot-mode-map
  :global nil)

(provide 'mastodon-toot)
