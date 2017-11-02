(require 'el-mock)

(ert-deftest generate-token ()
  "Should make `mastdon-http--post' request to generate auth token."
  (with-mock
   (let ((mastodon-instance-url "https://instance.url"))
     (mock (mastodon-client) => '(:client_id "id" :client_secret "secret"))
     (mock (auth-source-search :create t
                               :host "https://instance.url"
                               :port 443
                               :require '(:user :secret))
           => '((:user "foo@bar.com" :secret (lambda () "password"))))
     (mock (mastodon-http--post "https://instance.url/oauth/token"
                                '(("client_id" . "id")
                                  ("client_secret" . "secret")
                                  ("grant_type" . "password")
                                  ("username" . "foo@bar.com")
                                  ("password" . "password")
                                  ("scope" . "read write follow"))
                                nil
				:unauthenticated))
     (mastodon-auth--generate-token))))

(ert-deftest get-token ()
  "Should generate token and return JSON response."
  (with-temp-buffer
    (with-mock
      (mock (mastodon-auth--generate-token) => (progn
                                                  (insert "\n\n{\"access_token\":\"abcdefg\"}")
                                                  (current-buffer)))
      (should (equal (mastodon-auth--get-token) '(:access_token "abcdefg"))))))

(ert-deftest access-token-found ()
  "Should return value in `mastodon-auth--token-alist' if found."
  (let ((mastodon-instance-url "https://instance.url")
        (mastodon-auth--token-alist '(("https://instance.url" . "foobar")) ))
    (should (string= (mastodon-auth--access-token) "foobar"))))

(ert-deftest access-token-2 ()
  "Should set and return `mastodon-auth--token' if nil."
  (let ((mastodon-instance-url "https://instance.url")
        (mastodon-auth--token nil))
    (with-mock
      (mock (mastodon-auth--get-token) => '(:access_token "foobaz"))
      (should (string= (mastodon-auth--access-token) "foobaz"))
      (should (equal mastodon-auth--token-alist '(("https://instance.url" . "foobaz")))))))
