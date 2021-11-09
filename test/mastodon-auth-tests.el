;;; mastodon-auth-test.el --- Tests for mastodon-auth.el  -*- lexical-binding: nil -*-

(require 'el-mock)

(ert-deftest mastodon-auth--handle-token-response--good ()
  "Should extract the access token from a good response."
  (should
   (string=
    "foo"
    (mastodon-auth--handle-token-response
     '(:access_token "foo" :token_type "Bearer" :scope "read write follow" :created_at 0)))))

(ert-deftest mastodon-auth--handle-token-response--unknown ()
  "Should throw an error when the response is unparsable."
  (should
   (equal
    '(error "Unknown response from mastodon-auth--get-token!")
    (condition-case error
        (progn
          (mastodon-auth--handle-token-response '(:herp "derp"))
          nil)
      (t error)))))

(ert-deftest mastodon-auth--handle-token-response--failure ()
  "Should throw an error when the response indicates an error."
  (let ((error-message "The provided authorization grant is invalid, expired, revoked, does not match the redirection URI used in the authorization request, or was issued to another client."))
    (should
     (equal
      `(error ,(format "Mastodon-auth--access-token: invalid_grant: %s" error-message))
      (condition-case error
          (mastodon-auth--handle-token-response
           `(:error "invalid_grant" :error_description ,error-message))
        (t error))))))

(ert-deftest mastodon-auth--generate-token--no-storing-credentials ()
  "Should make `mastdon-http--post' request to generate auth token."
  (with-mock
    (let ((mastodon-auth-source-file "")
	  (mastodon-instance-url "https://instance.url"))
      (mock (mastodon-client) => '(:client_id "id" :client_secret "secret"))
      (mock (read-string "Email: " user-mail-address) => "foo@bar.com")
      (mock (read-passwd "Password: ") => "password")
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

(ert-deftest mastodon-auth--generate-token--storing-credentials ()
  "Should make `mastdon-http--post' request to generate auth token."
  (with-mock
    (let ((mastodon-auth-source-file "~/.authinfo")
	  (mastodon-instance-url "https://instance.url"))
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

(ert-deftest mastodon-auth--get-token ()
  "Should generate token and return JSON response."
  (with-temp-buffer
    (with-mock
      (mock (mastodon-auth--generate-token) => (progn
                                                 (insert "\n\n{\"access_token\":\"abcdefg\"}")
                                                 (current-buffer)))
      (should
       (equal (mastodon-auth--get-token)
              '(:access_token "abcdefg"))))))

(ert-deftest mastodon-auth--access-token-found ()
  "Should return value in `mastodon-auth--token-alist' if found."
  (let ((mastodon-instance-url "https://instance.url")
        (mastodon-auth--token-alist '(("https://instance.url" . "foobar")) ))
    (should
     (string= (mastodon-auth--access-token) "foobar"))))

(ert-deftest mastodon-auth--access-token-not-found ()
  "Should set and return `mastodon-auth--token' if nil."
  (let ((mastodon-instance-url "https://instance.url")
        (mastodon-auth--token-alist nil))
    (with-mock
      (mock (mastodon-auth--get-token) => '(:access_token "foobaz"))
      (should
       (string= (mastodon-auth--access-token)
                "foobaz"))
      (should
       (equal mastodon-auth--token-alist
              '(("https://instance.url" . "foobaz")))))))
