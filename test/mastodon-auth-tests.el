(require 'el-mock)
(load-file "../lisp/mastodon-auth.el")

(ert-deftest mastodon-auth:registration-success ()
  (let ((hash (make-hash-table :test 'equal))
        (client-plist '(:client_id "client-id" :client_secret "client-secret")))
    (puthash "client_id" "client-id" hash)
    (puthash "client_secret" "client-secret" hash)
    (with-mock
      (stub mastodon--json-hash-table => hash)
      (mastodon-auth--registration-success)
      (should (equal client-plist mastodon--client-app-plist)))))
