;; Notifications
(require 'mastodon-media)
(require 'mastodon-http)

(defvar mastodon-notifications--types '("mention" "follow" "favourite" "reblog" ))

(defun mastodon-notifications--mention(note)
  (let* ((status (mastodon-notifications--field 'status note))
	 (account (mastodon-notifications--field 'account status))
	 (spoiler (mastodon-notifications--field 'spoiler_text status))
	 (content (mastodon-notifications--field 'content status))
	 (media (mastodon-notifications--field 'media status))
    )))

(defun mastodon-notifications--follow(note)
  (let* ((account (mastodon-notifications--field 'account note))
	 (username (mastodon-notifications--field 'username account))
	 (display (mastodon-notifications--field 'display account))
	 ))
  )

(defun mastodon-notifications--favorite(note)
  "note"
  )

(defun mastodon-notifications--reblog(note)
  "note"
  )

(defun mastodon-notifications--caller (type note)
  (cond ((equal type "mention") (mastodon-notifications--mention note))
	((equal type "follow") (mastodon-notifications--follow note))
	((equal type "favourite") (mastodon-notifications--favorite note))
	((equal type "reblog") (mastodon-notifications--reblog note))
	)
  )

(defun mastodon-notifications--note (note)
  (let ((type (mastodon-notifications--field 'type note)))
    (when (member type mastodon-notifications--types)
      (insert
       (concat (mastodon-notifications--caller type note)
	   "\n")))))


(defun mastodon-notifications--notifications (json)
  (mapcar #'mastodon-notifications--note json)  )

(defun mastodon-notifications--get()
  (let* ((url (mastodon-http--api "notifications"))
	 (buffer "*mastodon-notifications*")
	 (json (mastodon-http--get-json url)))    
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      (mastodon-notifications--notifications json))
      (mastodon-mode)
    ))

(defun mastodon-notifications--field (field notification)
  (cdr(assoc field notification)))



(remove-if-not (lambda (x) (equal "mention" (cdr(second x))))(mastodon-http--get-json (mastodon-http--api "notifications")))


(defvar *sample-mention* (elt (remove-if-not (lambda (x) (equal "mention" (cdr(second x))))(mastodon-http--get-json (mastodon-http--api "notifications"))) 0))

(defvar *sample-favorite* (elt (remove-if-not (lambda (x) (equal "favourite" (cdr(second x))))(mastodon-http--get-json (mastodon-http--api "notifications"))) 0))

(defvar *sample-reblog* (elt (remove-if-not (lambda (x) (equal "reblog" (cdr(second x))))(mastodon-http--get-json (mastodon-http--api "notifications"))) 0))

(defvar *sample-follow* (elt (remove-if-not (lambda (x) (equal "follow" (cdr(second x))))(mastodon-http--get-json (mastodon-http--api "notifications"))) 0))
