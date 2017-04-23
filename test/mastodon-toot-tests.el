(require 'el-mock)

(ert-deftest mastodon-toot:cancel ()
  (with-mock
    (mock (kill-buffer-and-window))
    (should (eq nil (mastodon-toot--cancel)))))

(ert-deftest mastodon-toot:send ()
  (with-mock
    (stub mastodon--api-for => "https://instance/api/v/statuses")
    (stub buffer-string => "This is a test toot")
    (stub kill-buffer-and-window)
    (stub mastodon--access-token => "access-token-string")
    (mock (mastodon--http-post "https://instance/api/v/statuses"
                               'mastodon-toot--send-triage
                               '(("status" . "This is a test toot"))
                               '(("Authorization" . "Bearer access-token-string"))))
    (should (eq nil (mastodon-toot--send)))))

(ert-deftest mastodon-toot:send-triage ()
  (with-mock
    (mock (mastodon--http-response-triage "status"
                                          (lambda () (switch-to-buffer (current-buffer)))))
    (should (eq nil (mastodon-toot--send-triage "status")))))
