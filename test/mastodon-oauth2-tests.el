(require 'el-mock)

(ert-deftest oauth2-is-called-while-generating-token ()
  "Should call `mastodon-auth-oauth2--generate-token-and-store' to generate auth token."
  :expected-result :failed
  (with-mock
   (let ((mastodon-instance-url "https://instance.url")
         (mastodon-auth-mechanism "oauth2"))
     (mock (mastodon-auth-oauth2--generate-token-and-store) => '())
     (mock (read-string "Enter the code your browser displayed: ") => "12345")
     (should (equal (mastodon-auth--generate-token) '())))))

(ert-deftest oauth2-access-token-is-returned ()
  "Should return oauth2 access token."
  (with-mock
   (let ((mastodon-instance-url "https://instance.url")
         (mastodon-auth-mechanism "oauth2"))
     (mock (mastodon-oauth2--access-token) => '())
     (should (equal (mastodon-oauth2--access-token) '())))))

(ert-deftest oauth2-is-called-while-making-post-request ()
  "Should call `oauth2-url-retrieve-synchronously' to generate auth token.
Expected result is failed, because oauth2.el is not going to be able to get token."
  :expected-result :failed
  (with-mock
    (let ((mastodon-instance-url "https://instance.url")
          (mastodon-auth-mechanism "oauth2"))
      (mock (mastodon-oauth2--access-token) => '())
      (mock (oauth2-url-retrieve-synchronously) => '())
      (mastodon-http--post "https://instance.url/oauth/token"
                           '(("client_id" . "id")
                             ("client_secret" . "secret")
                             ("grant_type" . "password")
                             ("username" . "foo@bar.com")
                             ("password" . "password")
                             ("scope" . "read write follow"))
                           nil
                           :unauthenticated))))

(ert-deftest oauth2-is-called-while-making-get-request ()
  "Should call `oauth2-url-retrieve-synchronously' to generate auth token.
Expected result is failed, because oauth2.el is not going to be able to get token."
  :expected-result :failed
  (with-mock
    (let ((mastodon-instance-url "https://instance.url")
          (mastodon-auth-mechanism "oauth2"))
      (mock (mastodon-oauth2--access-token) => '())
      (mock (oauth2-url-retrieve-synchronously) => '())
      (mastodon-http--get "https://instance.url/oauth/token"))))
