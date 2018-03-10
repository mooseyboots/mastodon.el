;;; mastodon-profile.el --- Functions for inspecting Mastodon profiles -*- lexical-binding: t -*-

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.7.2
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

;; mastodon-profile.el generates stream of users toots.
;; To fix
;; 1. Render Image at top of frame [x]
;; 2. Get toot author [x]
;; 3. Load more toots [x]
;; Later
;; 1. List followers [x]
;; 2. List people they follow [x]
;; 3. Option to follow
;; 4. wheather they follow you or not
;; 5. Show only Media

;;; Code:

(defun mastodon-profile--toot-json ()
  "Get the next toot-json."
  (interactive)
  (mastodon-tl--property 'toot-json))

(defun mastodon-profile--make-author-buffer (status)
  "Take a STATUS and inserts a user account into a new buffer."
  (let* ((id (mastodon-profile--field status 'id))
         (acct (mastodon-profile--field status 'acct))
         accounts/id/statuses
         (url (mastodon-http--api
               (concat "accounts/"
                       (format "%s" id)
                       "/statuses" )))
         (buffer (concat "*mastodon-" acct  "*"))
         (account (cdr(assoc 'account status)))
         (note (mastodon-profile--field status 'note))
         (json (mastodon-http--get-json url)))
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      (mastodon-mode)
      (setq mastodon-tl--buffer-spec
            `(buffer-name ,buffer
                          endpoint ,(format "accounts/%s/statuses" id)
                          update-function
                          ,'mastodon-tl--timeline json))
      (let ((inhibit-read-only t))
        "\n"
        (mastodon-profile--image-from-status account)
        "\n"
        (propertize (mastodon-profile--field status 'display_name
                                             'face 'mastodon-display-name-face))
        "\n"
        (propertize acct
                    'face 'default)
        "\n ------------\n"
        (mastodon-tl--render-text note status)
        (mastodon-tl--set-face
         (concat " ------------\n"
                 "     TOOTS   \n"
                 " ------------\n")
         'success)
        (mastodon-tl--timeline json)))
    (mastodon-tl--goto-next-toot)))

(defun mastodon-profile--get-next-author ()
  "Jump to the next author and generate a buffer."
  (interactive)
  (mastodon-profile--make-author-buffer (mastodon-profile--toot-json)))

(defun mastodon-profile--image-from-status (status)
  "Generate an image from a STATUS."
  (let ((url (cdr(assoc 'avatar_static status))))
    (unless (equal url "/avatars/original/missing.png")
      (mastodon-media--get-media-link-rendering url))))

(defun mastodon-profile--field (status field)
  "The STATUS is a nested alist.

FIELD is used to identify regions under 'account"
  (cdr (assoc field (assoc 'account status))))

(defun mastodon-profile--get-next-authour-id ()
  "Get the author id of the next toot."
  (interactive)
  (get-authour-id (toot-proporties)))

(defun mastodon-profile--add-author-bylines (tootv)
  "Convert TOOTV into a author-bylines and insert."
  (let ((inhibit-read-only t))
    (mapc (lambda(toot)
            (insert (propertize (mastodon-tl--byline-author
                                 (list (append (list 'account) toot)))
                                'byline  't
                                'toot-id (cdr(assoc 'id toot)) 'toot-json toot)
                    "\n"))
          tootv))
  (mastodon-media--inline-images))


(defun mastodon-profile--get-following ()
  "Request a list of those who the user under point follows."
  (interactive)
  (mastodon-profile--make-follow-buffer "following"))

(defun mastodon-profile--followers ()
  "Request a list of those following the user under point."
  (interactive)
  (mastodon-profile--make-follow-buffer "followers"))

(defun mastodon-profile--make-follow-buffer (string)
  "Make a buffer contining followers or following of user under point.

STRING is an endpoint, either following or followers."
  (let* ((id (mastodon-profile--field (mastodon-profile--toot-json) 'id))
         (acct (mastodon-profile--field (mastodon-profile--toot-json) 'acct))
         (buffer (format  "*%s-%s*" string acct))
         (tootv (mastodon-http--get-json
                 (mastodon-http--api (format "accounts/%s/%s"
                                             id string)))))
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      (mastodon-mode)
      (setq mastodon-tl--buffer-spec
            `(buffer-name ,buffer
                          endpoint ,(format "accounts/%s/%s" id string)
                          update-function
                          ,'mastodon-profile--add-author-bylines))
      (mastodon-profile--add-author-bylines tootv))))


(provide 'mastodon-profile)
;;; mastodon-profile.el ends here
