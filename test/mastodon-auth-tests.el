(require 'el-mock)
(load-file "../lisp/mastodon-auth.el")

(ert-deftest mastodon-auth:registration-success ()
  "Should set `mastodon--client-app-plist' on succesful registration."
  (let ((hash (make-hash-table :test 'equal))
        (client-plist '(:client_id "client-id" :client_secret "client-secret")))
    (puthash "client_id" "client-id" hash)
    (puthash "client_secret" "client-secret" hash)
    (with-mock
      (stub mastodon--json-hash-table => hash)
      (mastodon-auth--registration-success)
      (should (equal client-plist mastodon--client-app-plist)))))

(ert-deftest mastodon-auth:register-client-app ()
  "Should POST client data to /apps endpoint."
  (with-mock
    (mock (mastodon--api-for "apps") => "https://instance/api/v1/apps")
    (mock (mastodon--http-post "https://instance/api/v1/apps"
                               'mastodon--register-client-app-triage
                               '(("client_name" . "mastodon.el")
                                 ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                                 ("scopes" . "read write follow")
                                 ("website" . "https://github.com/jdenen/mastodon.el"))))
    (should (eq nil (mastodon--register-client-app)))))

(ert-deftest mastodon-auth:register-client-app-triage ()
  "Should wrap `mastodon--http-response-triage' call."
  (with-mock
    (mock (mastodon--http-response-triage "status" 'mastodon-auth--registration-success))
    (should (eq nil (mastodon--register-client-app-triage "status")))))
