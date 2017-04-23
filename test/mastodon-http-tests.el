(require 'el-mock)

(ert-deftest mastodon-http:get:retrieves-endpoint ()
  "Should make a `url-retrieve' of the given URL."
  (let ((callback-double (lambda () "double")))
    (with-mock
      (mock (url-retrieve-synchronously "https://foo.bar/baz"))
      (mock (mastodon--access-token) => "test-token")
      (mastodon-http--get "https://foo.bar/baz"))))
