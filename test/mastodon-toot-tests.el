;;; mastodon-toot-test.el --- Tests for mastodon-toot.el  -*- lexical-binding: nil -*-

(require 'el-mock)

(defconst mastodon-toot--multi-mention
  '((mentions .
              [((id . "1")
                (username . "federated")
                (url . "https://site.cafe/@federated")
                (acct . "federated@federated.cafe"))
               ((id . "1")
                (username . "federated")
                (url . "https://site.cafe/@federated")
                (acct . "federated@federated.social"))
               ((id . "1")
                (username . "local")
                (url . "")
                (acct . "local"))])))

(defconst mastodon-toot-no-mention
  '((mentions . [])))

(ert-deftest mastodon-toot--multi-mentions ()
  "Should build a correct mention string from the test toot data.

Even the local name \"local\" gets a domain name added."
  (let ((mastodon-auth--acct-alist '(("https://local.social". "null")))
        (mastodon-instance-url "https://local.social"))
    (should (string=
             (mastodon-toot--mentions mastodon-toot--multi-mention)
             "@local@local.social @federated@federated.social @federated@federated.cafe "))))

(ert-deftest mastodon-toot--multi-mentions-with-name ()
  "Should build a correct mention string omitting self.

Here \"local\" is the user themselves and gets omitted from the
mention string."
  (let ((mastodon-auth--acct-alist
         '(("https://local.social". "local")))
        (mastodon-instance-url "https://local.social"))
    (should (string=
             (mastodon-toot--mentions mastodon-toot--multi-mention)
             "@federated@federated.social @federated@federated.cafe "))))

(ert-deftest mastodon-toot--no-mention ()
  "Should construct an empty mention string without mentions."
  (let ((mastodon-auth--acct-alist
         '(("https://local.social". "null")))
        (mastodon-instance-url "https://local.social"))
    (should (string= (mastodon-toot--mentions mastodon-toot-no-mention) ""))))

(ert-deftest mastodon-toot--cancel ()
  "Should kill the buffer when cancelling the toot."
  (with-mock
    (mock (kill-buffer-and-window))
    (mastodon-toot--cancel)
    (mock-verify)))
