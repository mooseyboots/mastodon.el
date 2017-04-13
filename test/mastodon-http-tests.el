(require 'el-mock)

(load-file "../lisp/mastodon-http.el")

(ert-deftest mastodon-http:get:retrieves-endpoint ()
  "Should make a `url-retrieve' of the given URL."
  (let ((callback-double (lambda () "double")))
    (with-mock
      (mock (url-retrieve "https://foo.bar/baz" 'callback-double))
      (mastodon-http--get "https://foo.bar/baz" 'callback-double))))
