(require 'el-mock)

(defconst mastodon-toot-multi-mention
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

(ert-deftest toot-multi-mentions ()
  (let ((mastodon-auth--acct-alist '(("https://local.social". "null")))
        (mastodon-instance-url "https://local.social"))
    (should (string=
             (mastodon-toot--mentions mastodon-toot-multi-mention)
             "@local@local.social @federated@federated.social @federated@federated.cafe "))))

(ert-deftest toot-multi-mentions-with-name ()
  (let ((mastodon-auth--acct-alist
         '(("https://local.social". "local")))
        (mastodon-instance-url "https://local.social"))
    (should (string=
             (mastodon-toot--mentions mastodon-toot-multi-mention)
             "@federated@federated.social @federated@federated.cafe "))))

(ert-deftest toot-no-mention ()
  (let ((mastodon-auth--acct-alist
         '(("https://local.social". "null")))
        (mastodon-instance-url "https://local.social"))
    (should (string= (mastodon-toot--mentions mastodon-toot-no-mention) ""))))

(ert-deftest cancel ()
  (with-mock
    (mock (kill-buffer-and-window))
    (mastodon-toot--cancel)
    (mock-verify)))
