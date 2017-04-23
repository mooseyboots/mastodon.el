(require 'el-mock)

(ert-deftest generate-token ()
  "Should make `mastdon-http--post' request to generate auth token."
  (with-mock
    (let ((mastodon-instance-url "https://instance.url"))
      (mock (mastodon-client) => '(:client_id "id" :client_secret "secret"))
      (mock (read-string "Email: ") => "foo@bar.com")
      (mock (read-passwd "Password: ") => "password")
      (mock (mastodon-http--post "https://instance.url/oauth/token"
                                 '(("client_id" . "id")
                                   ("client_secret" . "secret")
                                    ("grant_type" . "password")
                                    ("username" . "foo@bar.com")
                                    ("password" . "password")
                                    ("scope" . "read write follow"))
                                 nil))
      (mastodon-auth--generate-token))))

(ert-deftest get-token ()
  "Should generate token and return JSON response."
  (with-temp-buffer
    (with-mock
      (mock (mastodon-auth--generate-token) => (progn
                                                  (insert "\n\n{\"access_token\":\"abcdefg\"}")
                                                  (current-buffer)))
      (should (equal (mastodon-auth--get-token) '(:access_token "abcdefg"))))))

(ert-deftest access-token-1 ()
  "Should return `mastodon-auth--token' if non-nil."
  (let ((mastodon-auth--token "foobar"))
    (should (string= (mastodon-auth--access-token) "foobar"))))

(ert-deftest access-token-2 ()
  "Should set and return `mastodon-auth--token' if nil."
  (let ((mastodon-auth--token nil))
    (with-mock
      (mock (mastodon-auth--get-token) => '(:access_token "foobaz"))
      (should (string= (mastodon-auth--access-token) "foobaz"))
      (should (string= mastodon-auth--token "foobaz")))))
