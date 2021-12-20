;;; mastodon-http-test.el --- Tests for mastodon-http.el  -*- lexical-binding: nil -*-

(require 'el-mock)

(defconst mastodon-http--example-200
  "HTTP/1.1 200 OK
Date: Mon, 20 Dec 2021 13:42:29 GMT
Content-Type: application/json; charset=utf-8
Transfer-Encoding: chunked
Connection: keep-alive
Server: Mastodon
X-Frame-Options: DENY
X-Content-Type-Options: nosniff
X-XSS-Protection: 1; mode=block
Permissions-Policy: interest-cohort=()
X-RateLimit-Limit: 300
X-RateLimit-Remaining: 298
X-RateLimit-Reset: 2021-12-20T13:45:00.630990Z
Cache-Control: no-store
Vary: Accept, Accept-Encoding, Origin
ETag: W/\"bee52f489c87e9a305e5d0b7bdca7ac1\"
X-Request-Id: 5be9a64e-7d97-41b4-97f3-17b5e972a675
X-Runtime: 0.371914
Strict-Transport-Security: max-age=63072000; includeSubDomains
Strict-Transport-Security: max-age=31536000

{\"id\":\"18173\",\"following\":true,\"showing_reblogs\":true,\"notifying\":true,\"followed_by\":true,\"blocking\":false,\"blocked_by\":false,\"muting\":false,\"muting_notifications\":false,\"requested\":false,\"domain_blocking\":false,\"endorsed\":false,\"note\":\"\"}")

(defconst mastodon-http--example-400
  "HTTP/1.1 444 OK
Date: Mon, 20 Dec 2021 13:42:29 GMT
Content-Type: application/json; charset=utf-8
Transfer-Encoding: chunked
Connection: keep-alive
Server: Mastodon
X-Frame-Options: DENY
X-Content-Type-Options: nosniff
X-XSS-Protection: 1; mode=block
Permissions-Policy: interest-cohort=()
X-RateLimit-Limit: 300
X-RateLimit-Remaining: 298
X-RateLimit-Reset: 2021-12-20T13:45:00.630990Z
Cache-Control: no-store
Vary: Accept, Accept-Encoding, Origin
ETag: W/\"bee52f489c87e9a305e5d0b7bdca7ac1\"
X-Request-Id: 5be9a64e-7d97-41b4-97f3-17b5e972a675
X-Runtime: 0.371914
Strict-Transport-Security: max-age=63072000; includeSubDomains
Strict-Transport-Security: max-age=31536000

{\"error\":\"some unhappy complaint\"}")

(ert-deftest mastodon-http--get-retrieves-endpoint ()
  "Should make a `url-retrieve' of the given URL."
  (with-mock
    (mock (mastodon-http--url-retrieve-synchronously "https://foo.bar/baz"))
    (mock (mastodon-auth--access-token) => "test-token")
    (mastodon-http--get "https://foo.bar/baz")))

(ert-deftest mastodon-http--triage-success ()
  "Should run success function for 200 HTML response."
  (let ((response-buffer
         (get-buffer-create "mastodon-http--triage-buffer")))
    (with-current-buffer response-buffer
        (erase-buffer)
      (insert mastodon-http--example-200))
    (should (equal (mastodon-http--triage
                    response-buffer
                    (lambda ()
                      (message "success call")))
                   "success call"))))

(ert-deftest mastodon-http--triage-failure ()
  "Should return formatted JSON error from bad HTML response buffer.
  Should not run success function."
  (let ((response-buffer
         (get-buffer-create "mastodon-http--triage-buffer")))
    (with-current-buffer response-buffer
        (erase-buffer)
      (insert mastodon-http--example-400))
    (should (equal (mastodon-http--triage
                    response-buffer
                    (lambda ()
                      (message "success call")))
                   "Error 444: some unhappy complaint"))))
