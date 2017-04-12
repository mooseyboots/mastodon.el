(require 'el-mock)

(load-file "../lisp/mastodon.el")
(load-file "../lisp/mastodon-http.el")
(load-file "../lisp/mastodon-auth.el")

(ert-deftest mastodon-auth:token-file ()
  "Should return `mastodon-token-file' value."
  (should (string= (mastodon-auth--token-file) "~/.emacs.d/mastodon.plstore")))

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
  "Should POST client data to /apps endpoint and return client plist."
  (let ((app-plist '("id" "id-val" "secret" "secret-val")))
    (with-mock
      (mock (mastodon--api-for "apps") => "https://instance/api/v1/apps")
      (mock (mastodon--register-client-app-triage "status") => app-plist)
      (mock (mastodon--http-post "https://instance/api/v1/apps"
                                 'mastodon--register-client-app-triage
                                 '(("client_name" . "mastodon.el")
                                   ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                                   ("scopes" . "read write follow")
                                   ("website" . "https://github.com/jdenen/mastodon.el")))
            => (funcall 'mastodon--register-client-app-triage "status"))
      (should (eq app-plist (mastodon--register-client-app))))))

(ert-deftest mastodon-auth:register-client-app-triage ()
  "Should wrap `mastodon--http-response-triage' call and return client plist."
  (let ((app-plist '("id" "id-val" "secret" "secret-val")))
    (with-mock
      (mock (mastodon-auth--registration-success) => app-plist)
      (mock (mastodon--http-response-triage "status" 'mastodon-auth--registration-success)
            => (funcall 'mastodon-auth--registration-success))
      (should (eq app-plist (mastodon--register-client-app-triage "status"))))))

(ert-deftest mastodon-auth:register-and-return-client-app ()
  "Should return a plist of client_id and client_secret after registration."
  (let ((app-plist '("id" "id-val" "secret" "secret-val")))
    (with-mock
      (mock (mastodon--register-client-app) => app-plist)
      (should (equal app-plist (mastodon--register-and-return-client-app))))))

(defun helper:read-plstore (file key)
  (let* ((plstore (plstore-open file))
         (masto (delete "mastodon" (plstore-get plstore "mastodon"))))
    (progn
      (plstore-close plstore)
      (plist-get masto key))))

(ert-deftest mastodon-auth:store-client-id-and-secret ()
  "Should create plstore from client plist. Should return plist."
  (let ((app-plist '(:client_id "id-val" :client_secret "secret-val")))
    (with-mock
      (mock (mastodon--register-and-return-client-app) => app-plist)
      (mock (mastodon-auth--token-file) => "stubfile.plstore")
      (should (eq app-plist (mastodon--store-client-id-and-secret)))
      (should (string= (helper:read-plstore (mastodon-auth--token-file) :client_id) "id-val"))
      (should (string= (helper:read-plstore (mastodon-auth--token-file) :client_secret) "secret-val")))))
