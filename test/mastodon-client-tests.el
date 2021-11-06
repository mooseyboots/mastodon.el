(require 'el-mock)

(ert-deftest register ()
  "Should POST to /apps."
  (with-mock
   (mock (mastodon-http--api "apps") => "https://instance.url/api/v1/apps")
   (mock (mastodon-http--post "https://instance.url/api/v1/apps"
                              '(("client_name" . "mastodon.el")
                                ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                                ("scopes" . "read write follow")
                                ("website" . "https://github.com/jdenen/mastodon.el"))
                              nil
                              :unauthenticated))
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
  (let ((mastodon-instance-url "http://mastodon.example")
        (plist '(:client_id "id" :client_secret "secret")))
    (with-mock
     (mock (mastodon-client--token-file) => "stubfile.plstore")
     (mock (mastodon-client--fetch) => '(:client_id "id" :client_secret "secret"))
     (let* ((plstore (plstore-open "stubfile.plstore"))
            (client (cdr (plstore-get plstore "mastodon-http://mastodon.example"))))
       (should (equal (mastodon-client--store) plist))))))

(ert-deftest store-2 ()
  "Should store client in `mastodon-client--token-file'."
  (let* ((mastodon-instance-url "http://mastodon.example")
         (plstore (plstore-open "stubfile.plstore"))
         (client (cdr (plstore-get plstore "mastodon-http://mastodon.example"))))
    (plstore-close plstore)
    (should (string= (plist-get client :client_id) "id"))
    (should (string= (plist-get client :client_secret) "secret"))))

(ert-deftest read-finds-match ()
  "Should return mastodon client from `mastodon-token-file' if it exists."
  (let ((mastodon-instance-url "http://mastodon.example"))
    (with-mock
     (mock (mastodon-client--token-file) => "fixture/client.plstore")
     (should (equal (mastodon-client--read)
                    '(:client_id "id2" :client_secret "secret2"))))))

(ert-deftest read-finds-no-match ()
  "Should return mastodon client from `mastodon-token-file' if it exists."
  (let ((mastodon-instance-url "http://mastodon.social"))
    (with-mock
     (mock (mastodon-client--token-file) => "fixture/client.plstore")
     (should (equal (mastodon-client--read) nil)))))

(ert-deftest read-empty-store ()
  "Should return nil if mastodon client is not present in the plstore."
  (with-mock
   (mock (mastodon-client--token-file) => "fixture/empty.plstore")
   (should (equal (mastodon-client--read) nil))))

(ert-deftest client-set-and-matching ()
  "Should return `mastondon-client' if `mastodon-client--client-details-alist' is non-nil and instance url is included."
  (let ((mastodon-instance-url "http://mastodon.example")
        (mastodon-client--client-details-alist '(("https://other.example" . :no-match)
                                                 ("http://mastodon.example" . :matches))))
    (should (eq (mastodon-client) :matches))))

(ert-deftest client-set-but-not-matching ()
  "Should read from `mastodon-token-file' if wrong data is cached."
  (let ((mastodon-instance-url "http://mastodon.example")
        (mastodon-client--client-details-alist '(("http://other.example" :wrong))))
    (with-mock
     (mock (mastodon-client--read) => '(:client_id "foo" :client_secret "bar"))
     (should (equal (mastodon-client) '(:client_id "foo" :client_secret "bar")))
     (should (equal mastodon-client--client-details-alist
                    '(("http://mastodon.example" :client_id "foo" :client_secret "bar")
                      ("http://other.example" :wrong)))))))

(ert-deftest client-unset ()
  "Should read from `mastodon-token-file' if available."
  (let ((mastodon-instance-url "http://mastodon.example")
        (mastodon-client--client-details-alist nil))
    (with-mock
     (mock (mastodon-client--read) => '(:client_id "foo" :client_secret "bar"))
     (should (equal (mastodon-client) '(:client_id "foo" :client_secret "bar")))
     (should (equal mastodon-client--client-details-alist
                    '(("http://mastodon.example" :client_id "foo" :client_secret "bar")))))))

(ert-deftest client-unset-and-not-in-storage ()
  "Should store client data in plstore if it can't be read."
  (let ((mastodon-instance-url "http://mastodon.example")
        (mastodon-client--client-details-alist nil))
    (with-mock
     (mock (mastodon-client--read))
     (mock (mastodon-client--store) => '(:client_id "foo" :client_secret "baz"))
     (should (equal (mastodon-client) '(:client_id "foo" :client_secret "baz")))
     (should (equal mastodon-client--client-details-alist
                    '(("http://mastodon.example" :client_id "foo" :client_secret "baz")))))))
