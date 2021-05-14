;;; mastodon-profile.el --- Functions for inspecting Mastodon profiles -*- lexical-binding: t -*-

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

;; mastodon-profile.el generates a stream of users toots.
;; To add
;;  - Option to follow
;;  - wheather they follow you or not
;;  - Show only Media

;;; Code:
(require 'seq)

(autoload 'mastodon-http--api "mastodon-http.el")
(autoload 'mastodon-http--get-json "mastodon-http.el")
(autoload 'mastodon-media--get-media-link-rendering "mastodon-media.el")
(autoload 'mastodon-media--inline-images "mastodon-media.el")
(autoload 'mastodon-mode "mastodon.el")
(autoload 'mastodon-tl--byline-author "mastodon-tl.el")
(autoload 'mastodon-tl--goto-next-toot "mastodon-tl.el")
(autoload 'mastodon-tl--property "mastodon-tl.el")
(autoload 'mastodon-tl--render-text "mastodon-tl.el")
(autoload 'mastodon-tl--set-face "mastodon-tl.el")
(autoload 'mastodon-tl--timeline "mastodon-tl.el")
(autoload 'mastodon-tl--toot-id "mastodon-tl")

(defvar mastodon-instance-url)
(defvar mastodon-tl--buffer-spec)
(defvar mastodon-tl--update-point)

(defvar mastodon-profile--account nil
  "The data for the account being described in the current profile buffer.")
(make-variable-buffer-local 'mastodon-profile--account)

(define-minor-mode mastodon-profile-mode
  "Toggle mastodon profile minor mode.

This minor mode is used for mastodon profile pages and adds a couple of
extra keybindings."
  :init-value nil
  ;; The mode line indicator.
  :lighter " Profile"
  ;; The key bindings
  :keymap '(((kbd "F") . mastodon-profile--open-followers)
            ((kbd "f") . mastodon-profile--open-following))
  :group 'mastodon)

(defun mastodon-profile--toot-json ()
  "Get the next toot-json."
  (interactive)
  (mastodon-tl--property 'toot-json))

(defun mastodon-profile--make-author-buffer (account)
  "Take a ACCOUNT and inserts a user account into a new buffer."
  (mastodon-profile--make-profile-buffer-for
   account "statuses" #'mastodon-tl--timeline))

(defun mastodon-profile--open-following ()
  "Open a profile buffer for the current profile showing the accounts
that current profile follows."
  (interactive)
  (if mastodon-profile--account
      (mastodon-profile--make-profile-buffer-for
       mastodon-profile--account
       "following"
       #'mastodon-profile--add-author-bylines)
    (error "Not in a mastodon profile")))

(defun mastodon-profile--open-followers ()
  "Open a profile buffer for the current profile showing the accounts
following the current profile."
  (interactive)
  (if mastodon-profile--account
      (mastodon-profile--make-profile-buffer-for
       mastodon-profile--account
       "followers"
       #'mastodon-profile--add-author-bylines)
    (error "Not in a mastodon profile")))

(defun mastodon-profile--relationships-get (id)
  "Fetch info about logged in user's relationship to user with id ID."
  (interactive)
  (let* ((their-id id)
         (url (mastodon-http--api (format
                                   "accounts/relationships?id[]=%s"
                                   their-id))))
    (mastodon-http--get-json url)))

(defun mastodon-profile--fields-get (account)
  "Fetch the fields vector from a profile.

Returns a list of lists."
  (let ((fields (mastodon-profile--account-field account 'fields)))
    (when fields
      (mapcar
       (lambda (el)
         (list
          (cdr (assoc 'name el))
          (cdr (assoc 'value el))))
       fields))))

(defun mastodon-profile--fields-insert (fields)
  "Format and insert field pairs in FIELDS."
  (let* ((car-fields (mapcar 'car fields))
         ;; (cdr-fields (mapcar 'cadr fields))
         ;; (cdr-fields-rendered
          ;; (list
           ;; (mapcar (lambda (x)
                     ;; (mastodon-tl--render-text x nil))
                   ;; cdr-fields)))
         (left-width (car (sort (mapcar 'length car-fields) '>))))
         ;; (right-width (car (sort (mapcar 'length cdr-fields) '>))))
    (mapconcat (lambda (field)
                 (mastodon-tl--render-text
                  (concat
                   (format "_ %s " (car field))
                   (make-string (- (+ 1 left-width) (length (car field))) ?_)
                   (format " :: %s" (cadr field)))
                   ;; (make-string (- (+ 1 right-width) (length (cdr field))) ?_)
                   ;; " |")
                  nil))
               fields "")))

(defun mastodon-profile--make-profile-buffer-for (account endpoint-type update-function)
  (let* ((id (mastodon-profile--account-field account 'id))
         (acct (mastodon-profile--account-field account 'acct))
         (url (mastodon-http--api (format "accounts/%s/%s"
                                          id endpoint-type)))
         (buffer (concat "*mastodon-" acct "-" endpoint-type  "*"))
         (note (mastodon-profile--account-field account 'note))
         (json (mastodon-http--get-json url))
         (id (mastodon-profile--account-field account 'id))
         (followers-count (mastodon-tl--as-string
                           (mastodon-profile--account-field
                            account 'followers_count)))
         (following-count (mastodon-tl--as-string
                           (mastodon-profile--account-field
                            account 'following_count)))
         (toots-count (mastodon-tl--as-string
                       (mastodon-profile--account-field
                        account 'statuses_count)))
         (followed-by-you (cdr (assoc 'following
                                  (aref (mastodon-profile--relationships-get id) 0))))
         (follows-you (cdr (assoc 'followed_by
                                  (aref (mastodon-profile--relationships-get id) 0))))
         (followsp (or (equal follows-you 't) (equal followed-by-you 't)))
         (fields (mastodon-profile--fields-get account)))
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      (mastodon-mode)
      (mastodon-profile-mode)
      (setq mastodon-profile--account account
            mastodon-tl--buffer-spec
            `(buffer-name ,buffer
                          endpoint ,(format "accounts/%s/%s" id endpoint-type)
                          update-function ,update-function))
      (let* ((inhibit-read-only t)
             (is-statuses (string= endpoint-type "statuses"))
             (is-followers (string= endpoint-type "followers"))
             (is-following (string= endpoint-type "following"))
             (endpoint-name (cond
                              (is-statuses "     TOOTS   ")
                              (is-followers "  FOLLOWERS  ")
                              (is-following "  FOLLOWING  "))))
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
         (if fields
             (progn
               (concat "\n"
                       (mastodon-tl--set-face
                        (mastodon-profile--fields-insert fields)
                        'success)
                       "\n"))
           "")
         (mastodon-tl--set-face
          (concat " ------------\n"
                  " TOOTS: " toots-count " | "
                  "FOLLOWERS: " followers-count " | "
                  "FOLLOWING: " following-count "\n"
                  " ------------\n\n")
          'success)
         (if followsp
             (mastodon-tl--set-face
              (concat (if (equal follows-you 't)
                          " | FOLLOWS YOU")
                      (if (equal followed-by-you 't)
                          " | FOLLOWED BY YOU")
                      "\n\n")
              'success)
           "") ; if no followsp we still need str-or-char-p for insert
         (mastodon-tl--set-face
          (concat " ------------\n"
                  endpoint-name "\n"
                  " ------------\n")
          'success))
        (setq mastodon-tl--update-point (point))
        (mastodon-media--inline-images (point-min) (point))
        (funcall update-function json)))
    (mastodon-tl--goto-next-toot)))

(defun mastodon-profile--get-toot-author ()
  "Open profile of author of toot under point.

If toot is a boost, opens the profile of the booster."
  (interactive)
  (mastodon-profile--make-author-buffer
   (cdr (assoc 'account (mastodon-profile--toot-json)))))

(defun mastodon-profile--image-from-account (status)
  "Generate an image from a STATUS."
  (let ((url (cdr (assoc 'avatar_static status))))
    (unless (equal url "/avatars/original/missing.png")
      (mastodon-media--get-media-link-rendering url))))

(defun mastodon-profile--show-user (user-handle)
  "Query user for user id from current status and show that user's profile."
  (interactive
   (list
    (let ((user-handles (mastodon-profile--extract-users-handles
                         (mastodon-profile--toot-json))))
      (completing-read "User handle: "
                       user-handles
                       nil ; predicate
                       'confirm))))
  (let ((account (mastodon-profile--lookup-account-in-status
                  user-handle (mastodon-profile--toot-json))))
    (if account
        (mastodon-profile--make-author-buffer account)
      (message "Cannot find a user with handle %S" user-handle))))

(defun mastodon-profile--account-field (account field)
  "Return FIELD from the ACCOUNT.

FIELD is used to identify regions under 'account"
  (cdr (assoc field account)))

(defun mastodon-profile--add-author-bylines (tootv)
  "Convert TOOTV into a author-bylines and insert."
  (let ((inhibit-read-only t))
    (mapc (lambda (toot)
            (let ((start-pos (point)))
              (insert "\n"
                      (propertize
                       (mastodon-tl--byline-author `((account . ,toot)))
                       'byline  't
                       'toot-id (cdr (assoc 'id toot))
                       'base-toot-id (mastodon-tl--toot-id toot)
                       'toot-json toot))
              (mastodon-media--inline-images start-pos (point))
              (insert "\n"
                      (mastodon-tl--render-text (cdr (assoc 'note toot)) nil)
                      "\n")))
          tootv)))

(defun mastodon-profile--search-account-by-handle (handle)
  "Return an account based on a users HANDLE.

If the handle does not match a search return then retun NIL."
  (let* ((handle (if (string= "@" (substring handle 0 1))
                     (substring handle 1 (length handle))
                   handle))
         (matching-account
          (seq-remove
           (lambda(x) (not (string= (cdr (assoc 'acct x)) handle)))
           (mastodon-http--get-json
            (mastodon-http--api (format "accounts/search?q=%s" handle))))))
    (when (equal 1 (length matching-account))
      (elt matching-account 0))))

(defun mastodon-profile--account-from-id (user-id)
  "Request an account object relating to a USER-ID from Mastodon."
  (mastodon-http--get-json
   (mastodon-http--api (format "accounts/%s" user-id))))

(defun mastodon-profile--extract-users-handles (status)
  "Return all user handles found in STATUS.

These include the author, author of reblogged entries and any user mentioned."
  (when status
    (let ((this-account (cdr (assoc 'account status)))
	  (mentions (cdr (assoc 'mentions status)))
	  (reblog (cdr (assoc 'reblog status))))
      (seq-filter
       'stringp
       (seq-uniq
        (seq-concatenate
         'list
         (list (cdr (assoc 'acct this-account)))
         (mastodon-profile--extract-users-handles reblog)
         (mapcar (lambda (mention)
                   (cdr (assoc 'acct mention)))
                 mentions)))))))

(defun mastodon-profile--lookup-account-in-status (handle status)
  "Return account for HANDLE using hints in STATUS if possible."
  (let* ((this-account (cdr (assoc 'account status)))
         (reblog-account (cdr (assoc 'account (cdr (assoc 'reblog status)))))
         (mention-id (seq-some
                      (lambda (mention)
                        (when (string= handle
                                       (cdr (assoc 'acct mention)))
                          (cdr (assoc 'id mention))))
                      (cdr (assoc 'mentions status)))))
    (cond ((string= handle
                    (cdr (assoc 'acct this-account)))
           this-account)
          ((string= handle
                    (cdr (assoc 'acct reblog-account)))
           reblog-account)
          (mention-id
           (mastodon-profile--account-from-id mention-id))
          (t
           (mastodon-profile--search-account-by-handle handle)))))

(provide 'mastodon-profile)
;;; mastodon-profile.el ends here
