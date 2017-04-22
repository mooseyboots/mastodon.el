(require 'el-mock)
(load-file "../lisp/mastodon-client.el")

(ert-deftest register ()
  "Should POST to /apps."
  (with-mock
   (mock (mastodon--api-for "apps") => "https://instance.url/api/v1/apps")
   (mock (mastodon-http--post "https://instance.url/api/v1/apps"
                              '(("client_name" . "mastodon.el")
                                ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                                ("scopes" . "read write follow")
                                ("website" . "https://github.com/jdenen/mastodon.el"))
                              nil))
   (mastodon-client--register)))

(ert-deftest fetch ()
  "Should return client registration JSON."
  (with-temp-buffer
    (with-mock
      (mock (mastodon-client--register) => (progn
                                                   (insert "\n\n{\"foo\":\"bar\"}")
                                                   (current-buffer)))
      (should (equal (mastodon-client--fetch) '(:foo "bar"))))))

(ert-deftest store-1 ()
  "Should return the client plist."
  (let ((plist '(:client_id "id" :client_secret "secret")))
    (with-mock
      (mock (mastodon-client--token-file) => "stubfile.plstore")
      (mock (mastodon-client--fetch) => '(:client_id "id" :client_secret "secret"))
      (let* ((plstore (plstore-open "stubfile.plstore"))
             (client (delete "mastodon" (plstore-get plstore "mastodon"))))
        (should (equal (mastodon-client--store) plist))
        ))))

(ert-deftest store-2 ()
   "Should store client in `mastodon-client--token-file'."
   (let* ((plstore (plstore-open "stubfile.plstore"))
          (client (delete "mastodon" (plstore-get plstore "mastodon"))))
     (plstore-close plstore)
     (should (string= (plist-get client :client_id) "id"))
     (should (string= (plist-get client :client_secret) "secret"))))

(ert-deftest read-1 ()
  "Should return mastodon client from `mastodon-token-file' if it exists."
  (with-mock
    (mock (mastodon-client--token-file) => "fixture/client.plstore")
    (should (equal (mastodon-client--read) '(:client_id "id" :client_secret "secret")))))

(ert-deftest read-2 ()
  "Should return nil if mastodon client is not present in the plstore."
  (with-mock
    (mock (mastodon-client--token-file) => "fixture/empty.plstore")
    (should (equal (mastodon-client--read) nil))))

(ert-deftest client-1 ()
  "Should return `mastondon-client' if non-nil."
  (let ((mastodon-client t))
    (should (eq (mastodon-client) t))))

(ert-deftest client-2 ()
  "Should read from `mastodon-token-file' if available."
  (let ((mastodon-client nil))
    (with-mock
      (mock (mastodon-client--read) => '(:client_id "foo" :client_secret "bar"))
      (should (equal (mastodon-client) '(:client_id "foo" :client_secret "bar")))
      (should (equal mastodon-client '(:client_id "foo" :client_secret "bar"))))))

(ert-deftest client-3 ()
  "Should store client data in plstore if it can't be read."
  (let ((mastodon-client nil))
    (with-mock
      (mock (mastodon-client--read))
      (mock (mastodon-client--store) => '(:client_id "foo" :client_secret "baz"))
      (should (equal (mastodon-client) '(:client_id "foo" :client_secret "baz")))
      (should (equal mastodon-client '(:client_id "foo" :client_secret "baz"))))))
