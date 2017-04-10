;;; mastodon-toot.el --- Minor mode for sending Mastodon toots

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
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

;; mastodon-toot.el supports POSTing status data to Mastodon.

;;; Code:

(require 'mastodon-auth)
(require 'mastodon-http)

(defgroup mastodon-toot nil
  "Capture Mastodon toots."
  :prefix "mastodon-toot-"
  :group 'mastodon)

(defun mastodon-toot--send-triage (status)
  "Callback function to triage toot POST.

STATUS is passed by `url-retrieve'."
  (mastodon--http-response-triage status
                                  (lambda () (switch-to-buffer (current-buffer))))) ;; FIXME

(defun mastodon-toot--send ()
  "Kill new-toot buffer/window and POST contents to the Mastodon instance."
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
  "Kill new-toot buffer/window. Does not POST content to Mastodon."
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
;;; mastodon-toot.el ends here
