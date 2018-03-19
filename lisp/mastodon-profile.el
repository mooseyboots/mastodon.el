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

;; mastodon-profile.el generates a stream of users toots.
;; To add
;;  - Option to follow
;;  - wheather they follow you or not
;;  - Show only Media

;;; Code:

(defun mastodon-profile--toot-json ()
  "Get the next toot-json."
  (interactive)
  (mastodon-tl--property 'toot-json))

(defun mastodon-profile--make-author-buffer (account)
  "Take a ACCOUNT and inserts a user account into a new buffer."
  (let* ((id (mastodon-profile--account-field account 'id))
         (acct (mastodon-profile--account-field account 'acct))         
         (url (mastodon-http--api
               (concat "accounts/"
                       (format "%s" id)
                       "/statuses" )))
         (buffer (concat "*mastodon-" acct  "*"))         
         (note (mastodon-profile--account-field account 'note))
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
        (insert
         "\n"
         (mastodon-profile--image-from-account account)
         "\n"
         (propertize (mastodon-profile--account-field
                      account 'display_name)
                     'face 'mastodon-display-name-face)
         "\n"
         (propertize acct
                     'face 'default)
         "\n ------------\n"
         (mastodon-tl--render-text note nil)
         (mastodon-tl--set-face
          (concat " ------------\n"
                  "     TOOTS   \n"
                  " ------------\n")
          'success))
        (setq mastodon-tl-update-point (point))
        (mastodon-media--inline-images (point-min) (point))
        (mastodon-tl--timeline json)))
    (mastodon-tl--goto-next-toot)))

(defun mastodon-profile--get-toot-author ()
  "Opens authors profile of toot under point."
  (interactive)
  (mastodon-profile--make-author-buffer
   (cdr (assoc 'account (mastodon-profile--toot-json)))))

(defun mastodon-profile--image-from-account (status)
  "Generate an image from a STATUS."
  (let ((url (cdr (assoc 'avatar_static status))))
    (unless (equal url "/avatars/original/missing.png")
      (mastodon-media--get-media-link-rendering url))))

(defun mastodon-profile--account-field (account field)
  "Return FIELD from the ACCOUNT.

FIELD is used to identify regions under 'account"
  (cdr (assoc field account)))

(defun mastodon-profile--get-next-authour-id ()
  "Get the author id of the next toot."
  (interactive)
  (get-authour-id (toot-proporties)))

(defun mastodon-profile--add-author-bylines (tootv)
  "Convert TOOTV into a author-bylines and insert."
  (let ((inhibit-read-only t))
    (mapc (lambda(toot)
            (insert (propertize
                     (mastodon-tl--byline-author
                      (list (append (list 'account) toot)))
                     'byline  't
                     'toot-id (cdr (assoc 'id toot)) 'toot-json toot)
                    "\n"))
          tootv)))

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
  (let* ((account
          (cdr (assoc 'account (mastodon-profile--toot-json))))
         (id (mastodon-profile--account-field
              account 'id))
         (acct (mastodon-profile--account-field
                account 'acct))
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

(defun mastodon-profile--search-account-by-handle (handle)  
  "Return an account based on a users HANDLE.

If the handle does not match a search return then retun NIL."  
  (let* ((handle (if (string= "@" (substring handle 0 1))
                     (substring handle 1 (length handle))
                   handle))
         (matching-account
          (remove-if-not
           (lambda(x) (string= (cdr (assoc 'acct x)) handle))
           (mastodon-http--get-json
            (mastodon-http--api (format "accounts/search?q=%s" handle))))))
    (when (equal 1 (length matching-account))
      (elt matching-account 0))))

(defun mastodon-profile--account-from-id (user-id)
  "Request an account object relating to a USER-ID from Mastodon."
  (mastodon-http--get-json
   (mastodon-http--api (format "accounts/%s" user-id))))

(provide 'mastodon-profile)
;;; mastodon-profile.el ends here
