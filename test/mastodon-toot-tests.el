;;; mastodon-toot-test.el --- Tests for mastodon-toot.el  -*- lexical-binding: nil -*-

(require 'el-mock)
(require 'mastodon-http)

(defconst mastodon-toot--200-html
  "HTTP/1.1 200 OK
Date: Mon, 20 Dec 2021 13:42:29 GMT
Content-Type: application/json; charset=utf-8
Transfer-Encoding: chunked")

(defconst mastodon-toot-test-base-toot
  '((id . 61208)
    (created_at . "2017-04-24T19:01:02.000Z")
    (in_reply_to_id)
    (in_reply_to_account_id)
    (sensitive . :json-false)
    (spoiler_text . "")
    (visibility . "public")
    (account (id . 42)
             (username . "acct42")
             (acct . "acct42@example.space")
             (display_name . "Account 42")
             (locked . :json-false)
             (created_at . "2017-04-01T00:00:00.000Z")
             (followers_count . 99)
             (following_count . 13)
             (statuses_count . 101)
             (note . "E"))
    (media_attachments . [])
    (mentions . [])
    (tags . [])
    (uri . "tag:example.space,2017-04-24:objectId=654321:objectType=Status")
    (url . "https://example.space/users/acct42/updates/123456789")
    (content . "<p>Just some text</p>")
    (reblogs_count . 0)
    (favourites_count . 0)
    (reblog))
  "A sample toot (parsed json)")

(defconst mastodon-toot--mock-toot
  (propertize "here is a mock toot text."
              'toot-json mastodon-toot-test-base-toot))

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

;; TODO: test y-or-no-p with matodon-toot--cancel
(ert-deftest mastodon-toot--kill ()
  "Should kill the buffer when cancelling the toot."
  (with-mock
    (mock (kill-buffer-and-window))
    (mastodon-toot--kill)
    (mock-verify)))

(ert-deftest mastodon-toot--own-toot-p-fail ()
  (let ((toot mastodon-toot-test-base-toot))
    (with-mock
      (mock (mastodon-auth--user-acct) => "joebogus@bogus.space")
      (should (not (equal (mastodon-toot--own-toot-p toot)
                          t))))))

(ert-deftest mastodon-toot--own-toot-p ()
  (let ((toot mastodon-toot-test-base-toot))
    (with-mock
      (mock (mastodon-auth--user-acct) => "acct42@example.space")
      (should (equal (mastodon-toot--own-toot-p toot)
                     t)))))

(ert-deftest mastodon-toot--delete-toot-fail ()
  "Should refuse to delete toot."
  (let ((toot mastodon-toot-test-base-toot))
    (with-mock
      (mock (mastodon-auth--user-acct) => "joebogus")
      ;; (mock (mastodon-toot--own-toot-p toot) => nil)
      (mock (mastodon-tl--property 'toot-json) => mastodon-toot-test-base-toot)
      (should (equal (mastodon-toot--delete-toot)
                     "You can only delete (and redraft) your own toots.")))))

(ert-deftest mastodon-toot--delete-toot ()
  "Should return correct triaged response to a legitimate DELETE request."
  (with-temp-buffer
    (insert mastodon-toot--200-html)
    (let ((delete-response (current-buffer))
          (toot mastodon-toot-test-base-toot))
      (with-mock
        (mock (mastodon-tl--property 'toot-json) => mastodon-toot-test-base-toot)
        ;; (mock (mastodon-toot--own-toot-p toot) => t)
        (mock (mastodon-auth--user-acct) => "acct42@example.space")
        (mock (mastodon-http--api (format "statuses/61208"))
              => "https://example.space/statuses/61208")
        (mock (y-or-n-p "Delete this toot? ") => t)
        (mock (mastodon-http--delete "https://example.space/statuses/61208")
              => delete-response)
        (should (equal (mastodon-toot--delete-toot)
                       "Toot deleted!"))))))
