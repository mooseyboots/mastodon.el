;;; mastodon-http-test.el --- Tests for mastodon-http.el  -*- lexical-binding: nil -*-

(require 'el-mock)

(ert-deftest mastodon-http--get-retrieves-endpoint ()
  "Should make a `url-retrieve' of the given URL."
  (with-mock
    (mock (mastodon-http--url-retrieve-synchronously "https://foo.bar/baz"))
    (mock (mastodon-auth--access-token) => "test-token")
    (mastodon-http--get "https://foo.bar/baz")))
