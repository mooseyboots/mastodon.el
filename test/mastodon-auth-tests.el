(require 'el-mock)

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

(ert-deftest mastodon-auth:register-and-return-client-app:with-p ()
  "Should return a plist of client_id and client_secret without registration."
  (let ((app-plist '(:client_id "id-val" :client_secret "secret-val")))
    (with-mock
      (mock (mastodon-auth--client-app-secret-p) => t)
      (mock (mastodon-auth--client) => app-plist)
      (stub mastodon-auth--registration-success => mastodon--client-app-plist)
      (not-called sleep-for)
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

(ert-deftest mastodon-auth:client-app:memoized ()
  "Should return `mastodon--client-app-plist' if it has a :client_secret."
  (with-mock
    (mock (plist-get mastodon--client-app-plist :client_secret) => t)
    (should (eq (mastodon--client-app) mastodon--client-app-plist))))

(ert-deftest mastodon-auth:client-app:stored ()
  "Should retrieve from `mastodon-token-file' if not memoized."
  (with-mock
    (stub plist-get)
    (mock (mastodon-auth--token-file) => "fixture/client.plstore")
    (should (equal (mastodon--client-app) '(:client_id "id" :client_secret "secret")))))

(ert-deftest mastodon-auth:client-app:generated ()
  "Should generate `mastodon--client-app-plist' if not memoized or stored."
  (with-mock
    (stub plist-get)
    (mock (mastodon-auth--token-file) => "fixture/empty.plstore")
    (mock (mastodon--store-client-id-and-secret))
    (mastodon--client-app)))

(ert-deftest mastodon-auth:get-token-success ()
  "Should return access token from `url-retrieve' response JSON."
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "access_token" "token-value" hash)
    (with-mock
      (mock (mastodon--json-hash-table) => hash)
      (should (string= (mastodon-auth--get-token-success) "token-value")))))

(ert-deftest mastodon-auth:get-access-token-triage ()
  "Should wrap `mastodon--http-response-triage'."
  (with-mock
    (mock (mastodon--http-response-triage "status" 'mastodon-auth--get-token-success))
    (mastodon--get-access-token-triage "status")))

(ert-deftest mastodon-auth:get-access-token ()
  "Should POST auth data to retrieve access token."
  (let ((client-app '(:client_id "id" :client_secret "secret")))
    (with-mock
      (mock (mastodon-auth--user-and-passwd) => (cons "email" "password"))
      (mock (mastodon--client-app) => client-app)
      (mock (mastodon--http-post "https://mastodon.social/oauth/token"
                                 'mastodon--get-access-token-triage
                                 '(("client_id" . "id")
                                   ("client_secret" . "secret")
                                   ("grant_type" . "password")
                                   ("username" . "email")
                                   ("password" . "password")
                                   ("scope" . "read write follow"))))
      (mastodon--get-access-token))))

(ert-deftest mastodon-auth:access-token:memoized ()
  "Should return `mastodon--api-token-string' if set."
  (with-mock
    (mock (mastodon-auth--token) => "foobar")
    (should (string= (mastodon--access-token) "foobar"))))

(ert-deftest mastodon-auth:access-token:generated ()
  "Should generate `mastodon--api-token-string' if not memoized."
  (with-mock
    (mock (mastodon-auth--token) => nil)
    (mock (mastodon--get-access-token)
          => (mock (mastodon-auth--token) => "foobar"))
    (should (string= (mastodon--access-token) "foobar"))))
