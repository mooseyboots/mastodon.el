;;; mastodon-profile.el --- Functions for inspecting Mastodon profiles -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.9.1
;; Package-Requires: ((emacs "26.1") (seq "1.0"))
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
(autoload 'mastodon-http--post "mastodon-http.el")
(autoload 'mastodon-http--triage "mastodon-http.el")
(autoload 'mastodon-auth--get-account-name "mastodon-auth.el")
(autoload 'mastodon-http--get-json-async "mastodon-http.el")
(autoload 'mastodon-media--get-media-link-rendering "mastodon-media.el")
(autoload 'mastodon-media--inline-images "mastodon-media.el")
(autoload 'mastodon-mode "mastodon.el")
(autoload 'mastodon-tl--byline-author "mastodon-tl.el")
(autoload 'mastodon-tl--goto-next-toot "mastodon-tl.el")
(autoload 'mastodon-tl--property "mastodon-tl.el")
(autoload 'mastodon-tl--find-property-range "mastodon-tl.el")
(autoload 'mastodon-tl--render-text "mastodon-tl.el")
(autoload 'mastodon-tl--set-face "mastodon-tl.el")
(autoload 'mastodon-tl--timeline "mastodon-tl.el")
(autoload 'mastodon-tl--as-string "mastodon-tl.el")
(autoload 'mastodon-tl--toot-id "mastodon-tl")
(autoload 'mastodon-tl--toot "mastodon-tl")
(autoload 'mastodon-tl--init "mastodon-tl.el")
(autoload 'mastodon-http--patch "mastodon-http")
(autoload 'mastodon-http--patch-json "mastodon-http")

(defvar mastodon-instance-url)
(defvar mastodon-tl--buffer-spec)
(defvar mastodon-tl--update-point)


(defvar-local mastodon-profile--account nil
  "The data for the account being described in the current profile buffer.")

;; this way you can update it with C-M-x:
(defvar mastodon-profile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "O") #'mastodon-profile--open-followers)
    (define-key map (kbd "o") #'mastodon-profile--open-following)
    (define-key map (kbd "a") #'mastodon-profile--follow-request-accept)
    (define-key map (kbd "j") #'mastodon-profile--follow-request-reject)
    map)
  "Keymap for `mastodon-profile-mode'.")

(define-minor-mode mastodon-profile-mode
  "Toggle mastodon profile minor mode.

This minor mode is used for mastodon profile pages and adds a couple of
extra keybindings."
  :init-value nil
  ;; The mode line indicator.
  :lighter " Profile"
  :keymap mastodon-profile-mode-map
  ;; :keymap '(((kbd "O") . mastodon-profile--open-followers)
  ;;           ((kbd "o") . mastodon-profile--open-following)
  ;;           ((kbd "a") . mastodon-profile--follow-request-accept)
  ;;           ((kbd "r") . mastodon-profile--follow-request-reject)
  :group 'mastodon
  :global nil)

(defvar mastodon-profile-update-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mastodon-profile--user-profile-send-updated)
    (define-key map (kbd "C-c C-k") #'kill-buffer-and-window)
    map)
  "Keymap for `mastodon-profile-update-mode'.")

(define-minor-mode mastodon-profile-update-mode
  "Minor mode to update Mastodon user profile."
  :group 'mastodon-profile
  :keymap mastodon-profile-update-mode-map
  :global nil)

(defun mastodon-profile--toot-json ()
  "Get the next toot-json."
  (interactive)
  (mastodon-tl--property 'toot-json))

(defun mastodon-profile--make-author-buffer (account)
  "Take an ACCOUNT json and insert a user account into a new buffer."
  (mastodon-profile--make-profile-buffer-for
   account "statuses" #'mastodon-tl--timeline))

(defun mastodon-profile--open-following ()
  "Open a profile buffer showing the accounts that current profile follows."
  (interactive)
  (if mastodon-profile--account
      (mastodon-profile--make-profile-buffer-for
       mastodon-profile--account
       "following"
       #'mastodon-profile--add-author-bylines)
    (error "Not in a mastodon profile")))

(defun mastodon-profile--open-followers ()
  "Open a profile buffer showing the accounts following the current profile."
  (interactive)
  (if mastodon-profile--account
      (mastodon-profile--make-profile-buffer-for
       mastodon-profile--account
       "followers"
       #'mastodon-profile--add-author-bylines)
    (error "Not in a mastodon profile")))

(defun mastodon-profile--view-favourites ()
  "Open a new buffer displaying the user's favourites."
  (interactive)
  (message "Loading your favourited toots...")
  (mastodon-tl--init "favourites"
                     "favourites"
                     'mastodon-tl--timeline))

(defun mastodon-profile--view-bookmarks ()
  "Open a new buffer displaying the user's bookmarks."
  (interactive)
  (message "Loading your bookmarked toots...")
  (mastodon-tl--init "bookmarks"
                     "bookmarks"
                     'mastodon-tl--timeline))

(defun mastodon-profile--view-follow-requests ()
  "Open a new buffer displaying the user's follow requests."
  (interactive)
  (mastodon-profile-mode)
  (mastodon-tl--init "follow-requests"
                     "follow_requests"
                     'mastodon-profile--add-author-bylines))

(defun mastodon-profile--follow-request-accept ()
  "Accept the follow request of user at point."
  (interactive)
  (if (mastodon-tl--find-property-range 'toot-json (point))
      (let* ((acct-json (mastodon-profile--toot-json))
             (id (alist-get 'id acct-json))
             (handle (alist-get 'acct acct-json))
             (name (alist-get 'username acct-json)))
        (if id
            (let ((response
                   (mastodon-http--post
                    (concat
                     (mastodon-http--api "follow_requests")
                     (format "/%s/authorize" id))
                    nil nil)))
              (mastodon-http--triage response
                                     (lambda ()
                                       (message "Follow request of %s (@%s) accepted!"
                                                name handle))))
          (message "No account result at point?")))
    (message "No follow request at point?")))

(defun mastodon-profile--follow-request-reject ()
  "Reject the follow request of user at point."
  (interactive)
  (if (mastodon-tl--find-property-range 'toot-json (point))
      (let* ((acct-json (mastodon-profile--toot-json))
             (id (alist-get 'id acct-json))
             (handle (alist-get 'acct acct-json))
             (name (alist-get 'username acct-json)))
        (if id
            (let ((response
                   (mastodon-http--post
                    (concat
                     (mastodon-http--api "follow_requests")
                     (format "/%s/reject" id))
                    nil nil)))
              (mastodon-http--triage response
                                     (lambda ()
                                       (message "Follow request of %s (@%s) rejected!"
                                                name handle))))
          (message "No account result at point?")))
    (message "No follow request at point?")))

(defun mastodon-profile--update-user-profile-note ()
  "Fetch user's profile note and display for editing."
  (interactive)
  (let* ((url (concat mastodon-instance-url
                      "/api/v1/accounts/update_credentials"))
         ;; (buffer (mastodon-http--patch url))
         (json (mastodon-http--patch-json url))
         (source (alist-get 'source json))
         (note (alist-get 'note source))
         (buffer (get-buffer-create "*mastodon-update-profile*"))
         (inhibit-read-only t))
    (switch-to-buffer-other-window buffer)
    (mastodon-profile-update-mode t)
    (insert note)
    (goto-char (point-min))
    (message "Edit your profile note. C-c C-c to send, C-c C-k to cancel.")))

(defun mastodon-profile--user-profile-send-updated ()
  "Send PATCH request with the updated profile note."
  (interactive)
  (let* ((note (buffer-substring-no-properties (point-min) (point-max)))
         (url (concat mastodon-instance-url
                      "/api/v1/accounts/update_credentials")))
    (kill-buffer-and-window)
    (let ((response (mastodon-http--patch url note)))
      (mastodon-http--triage response
                             (lambda () (message "Profile note updated!"))))))

(defun mastodon-profile--relationships-get (id)
  "Fetch info about logged-in user's relationship to user with id ID."
  (let* ((their-id id)
         (url (mastodon-http--api (format
                                   "accounts/relationships?id[]=%s"
                                   their-id))))
    (mastodon-http--get-json url)))

(defun mastodon-profile--fields-get (account)
  "Fetch the fields vector (aka profile metadata) from profile of ACCOUNT.

Returns a list of lists."
  (let ((fields (mastodon-profile--account-field account 'fields)))
    (when fields
      (mapcar
       (lambda (el)
         (list
          (alist-get 'name el)
          (alist-get 'value el)))
       fields))))

(defun mastodon-profile--fields-insert (fields)
  "Format and insert field pairs (a.k.a profile metadata) in FIELDS."
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
                  field)) ; nil)) ; hack to make links tabstops
               fields "")))

(defun mastodon-profile--get-statuses-pinned (account)
  "Fetch the pinned toots for ACCOUNT."
  (let* ((id (mastodon-profile--account-field account 'id))
         (url (mastodon-http--api (format "accounts/%s/statuses?pinned=true" id))))
    (mastodon-http--get-json url)))

(defun mastodon-profile--insert-statuses-pinned (pinned-statuses)
  "Insert each of the PINNED-STATUSES for a given account."
  (mapc (lambda (pinned-status)
          (insert (mastodon-tl--set-face
                   "   :pinned: " 'success))
          (mastodon-tl--toot pinned-status))
        pinned-statuses))

(defun mastodon-profile--make-profile-buffer-for (account endpoint-type update-function)
  "Display profile of ACCOUNT, using ENDPOINT-TYPE and UPDATE-FUNCTION."
  (let* ((id (mastodon-profile--account-field account 'id))
         (url (mastodon-http--api (format "accounts/%s/%s" id endpoint-type)))
         (acct (mastodon-profile--account-field account 'acct))
         (buffer (concat "*mastodon-" acct "-" endpoint-type  "*"))
         (note (mastodon-profile--account-field account 'note))
         (json (mastodon-http--get-json url))
         (locked (mastodon-profile--account-field account 'locked))
         (followers-count (mastodon-tl--as-string
                           (mastodon-profile--account-field
                            account 'followers_count)))
         (following-count (mastodon-tl--as-string
                           (mastodon-profile--account-field
                            account 'following_count)))
         (toots-count (mastodon-tl--as-string
                       (mastodon-profile--account-field
                        account 'statuses_count)))
         (relationships (mastodon-profile--relationships-get id))
         (followed-by-you (alist-get 'following
                                     (aref relationships 0)))
         (follows-you (alist-get 'followed_by
                                 (aref relationships 0)))
         (followsp (or (equal follows-you 't) (equal followed-by-you 't)))
         (fields (mastodon-profile--fields-get account))
         (pinned (mastodon-profile--get-statuses-pinned account)))
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
         (propertize (concat "@" acct)
                     'face 'default)
         (if (equal locked t)
             (if (fontp (char-displayable-p #10r9993))
                 " 🔒"
               " [locked]")
           "")
         "\n ------------\n"
         (mastodon-tl--render-text note account)
         ;; account here to enable tab-stops in profile note
         (if fields
             (concat "\n"
                     (mastodon-tl--set-face
                      (mastodon-profile--fields-insert fields)
                      'success)
                     "\n")
           "")
         ;; insert counts
         (mastodon-tl--set-face
          (concat " ------------\n"
                  " TOOTS: " toots-count " | "
                  "FOLLOWERS: " followers-count " | "
                  "FOLLOWING: " following-count "\n"
                  " ------------\n\n")
          'success)
         ;; insert relationship (follows)
         (if followsp
             (mastodon-tl--set-face
              (concat (if (equal follows-you 't)
                          " | FOLLOWS YOU")
                      (if (equal followed-by-you 't)
                          " | FOLLOWED BY YOU")
                      "\n\n")
              'success)
           "") ; if no followsp we still need str-or-char-p for insert
         ;; insert endpoint
         (mastodon-tl--set-face
          (concat " ------------\n"
                  endpoint-name "\n"
                  " ------------\n")
          'success))
        (setq mastodon-tl--update-point (point))
        (mastodon-media--inline-images (point-min) (point))
        ;; insert pinned toots first
        (when (and pinned (equal endpoint-type "statuses"))
          (mastodon-profile--insert-statuses-pinned pinned)
          (setq mastodon-tl--update-point (point))) ;updates to follow pinned toots
        (funcall update-function json)))
    ;;(mastodon-tl--goto-next-toot)
    (goto-char (point-min))))

(defun mastodon-profile--get-toot-author ()
  "Open profile of author of toot under point.

If toot is a boost, opens the profile of the booster."
  (interactive)
  (mastodon-profile--make-author-buffer
   (alist-get 'account (mastodon-profile--toot-json))))

(defun mastodon-profile--image-from-account (status)
  "Generate an image from a STATUS."
  (let ((url (alist-get 'avatar_static status)))
    (unless (equal url "/avatars/original/missing.png")
      (mastodon-media--get-media-link-rendering url))))

(defun mastodon-profile--show-user (user-handle)
  "Query user for USER-HANDLE from current status and show that user's profile."
  (interactive
   (list
    (let ((user-handles (mastodon-profile--extract-users-handles
                         (mastodon-profile--toot-json))))
      (completing-read "View profile of user [choose or enter any handle]: "
                       user-handles
                       nil ; predicate
                       'confirm))))
  (let ((account (mastodon-profile--lookup-account-in-status
                  user-handle (mastodon-profile--toot-json))))
    (if account
        (progn
          (message "Loading profile of user %s..." user-handle)
          (mastodon-profile--make-author-buffer account))
      (message "Cannot find a user with handle %S" user-handle))))

(defun mastodon-profile--my-profile ()
  "Show the profile of the currently signed in user."
  (interactive)
  (message "Loading your profile...")
  (mastodon-profile--show-user (mastodon-auth--get-account-name)))

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
                       'toot-id (alist-get 'id toot)
                       'base-toot-id (mastodon-tl--toot-id toot)
                       'toot-json toot))
              (mastodon-media--inline-images start-pos (point))
              (insert "\n"
                      (mastodon-tl--render-text (alist-get 'note toot) nil)
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
           (lambda(x) (not (string= (alist-get 'acct x) handle)))
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
    (let ((this-account (alist-get 'account status))
	  (mentions (alist-get 'mentions status))
	  (reblog (alist-get 'reblog status)))
      (seq-filter
       'stringp
       (seq-uniq
        (seq-concatenate
         'list
         (list (alist-get 'acct this-account))
         (mastodon-profile--extract-users-handles reblog)
         (mapcar (lambda (mention)
                   (alist-get 'acct mention))
                 mentions)))))))

(defun mastodon-profile--lookup-account-in-status (handle status)
  "Return account for HANDLE using hints in STATUS if possible."
  (let* ((this-account (alist-get 'account status))
         (reblog-account (alist-get 'account (alist-get 'reblog status)))
         (mention-id (seq-some
                      (lambda (mention)
                        (when (string= handle
                                       (alist-get 'acct mention))
                          (alist-get 'id mention)))
                      (alist-get 'mentions status))))
    (cond ((string= handle
                    (alist-get 'acct this-account))
           this-account)
          ((string= handle
                    (alist-get 'acct reblog-account))
           reblog-account)
          (mention-id
           (mastodon-profile--account-from-id mention-id))
          (t
           (mastodon-profile--search-account-by-handle handle)))))

(provide 'mastodon-profile)
;;; mastodon-profile.el ends here
