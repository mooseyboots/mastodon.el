(require 'el-mock)

(defconst mastodon-tl-test-base-toot
  '((id . 61208)
    (created_at . "2017-04-24T19:01:02.000Z")
    (in_reply_to_id)
    (in_reply_to_account_id)
    (sensitive . :json-false)
    (spoiler_text . "Spoiler text")
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
    (reblogs_count . 0)
    (favourites_count . 0)
    (reblog))
  "A sample toot (parsed json)")

(defconst mastodon-tl-test-base-boosted-toot
  '((id . 61208)
    (created_at . "2017-04-24T20:59:59.000Z")
    (in_reply_to_id)
    (in_reply_to_account_id)
    (sensitive . :json-false)
    (spoiler_text . "Spoiler text")
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
    (reblogs_count . 0)
    (favourites_count . 0)
    (reblog (id . 4543919)
            (created_at . "2017-04-24T19:01:02.000Z")
            (in_reply_to_id)
            (in_reply_to_account_id)
            (sensitive . :json-false)
            (spoiler_text . "")
            (visibility . "public")
            (application)
            (account (id . 43)
                     (username . "acct43")
                     (acct . "acct43@example.space")
                     (display_name . "Account 43")
                     (locked . :json-false)
                     (created_at . "2017-04-02T00:00:00.000Z")
                     (followers_count . 1)
                     (following_count . 1)
                     (statuses_count . 1)
                     (note . "Other account"))
            (media_attachments . [])
            (mentions . [((url . "https://mastodon.social/@johnson")
                          (acct . "acct42")
                          (id . 42)
                          (username . "acct42"))])
            (tags . [])
            (uri . "tag:example.space,2017-04-24:objectId=654321:objectType=Status")
            (content . "<p><span class=\"h-card\"><a href=\"https://example.spacs/@acct42\">@<span>acct42</span></a></span> boost</p>")
            (url . "https://example.space/users/acct42/updates/123456789")
            (reblogs_count . 1)
            (favourites_count . 1)
            (favourited)
            (reblogged)))
  "A sample reblogged/boosted toot (parsed json)")

(ert-deftest remove-html-1 ()
  "Should remove all <span> tags."
  (let ((input "<span class=\"h-card\">foobar</span> <span>foobaz</span>"))
    (should (string= (mastodon-tl--remove-html input) "foobar foobaz"))))

(ert-deftest remove-html-2 ()
  "Should replace <\p> tags with two new lines."
  (let ((input "foobar</p>"))
    (should (string= (mastodon-tl--remove-html input) "foobar\n\n"))))

(ert-deftest more-json ()
  "Should request toots older than max_id."
  (let ((mastodon-instance-url "https://instance.url"))
    (with-mock
      (mock (mastodon-http--get-json "https://instance.url/api/v1/timelines/foo?max_id=12345"))
      (mastodon-tl--more-json "timelines/foo" 12345))))

(ert-deftest mastodon-tl--byline-regular ()
  "Should format the regular toot correctly."
  (let ((mastodon-tl--show-avatars-p nil)
        (timestamp (cdr (assoc 'created_at mastodon-tl-test-base-toot))))
    (with-mock
      (mock (date-to-time timestamp) => '(22782 21551))
      (mock (format-time-string mastodon-toot-timestamp-format '(22782 21551)) => "2999-99-99 00:11:22")

      (should (string= (substring-no-properties
                        (mastodon-tl--byline mastodon-tl-test-base-toot))
                       "
 | Account 42 (@acct42@example.space) 2999-99-99 00:11:22
  ------------")))))

(ert-deftest mastodon-tl--byline-regular-with-avatar ()
  "Should format the regular toot correctly."
  (let ((mastodon-tl--show-avatars-p t)
        (timestamp (cdr (assoc 'created_at mastodon-tl-test-base-toot))))
    (with-mock
      (stub create-image => '(image "fake data"))
      (mock (date-to-time timestamp) => '(22782 21551))
      (mock (format-time-string mastodon-toot-timestamp-format '(22782 21551)) => "2999-99-99 00:11:22")

      (should (string= (substring-no-properties
                        (mastodon-tl--byline mastodon-tl-test-base-toot))
                       "
 |   Account 42 (@acct42@example.space) 2999-99-99 00:11:22
  ------------")))))

(ert-deftest mastodon-tl--byline-boosted ()
  "Should format the boosted toot correctly."
  (let* ((mastodon-tl--show-avatars-p nil)
         (toot (cons '(reblogged . t) mastodon-tl-test-base-toot))
         (timestamp (cdr (assoc 'created_at toot))))
    (with-mock
      (mock (date-to-time timestamp) => '(22782 21551))
      (mock (format-time-string mastodon-toot-timestamp-format '(22782 21551)) => "2999-99-99 00:11:22")

      (should (string= (substring-no-properties (mastodon-tl--byline toot))
                      "
 | (B) Account 42 (@acct42@example.space) 2999-99-99 00:11:22
  ------------")))))

(ert-deftest mastodon-tl--byline-favorited ()
  "Should format the favourited toot correctly."
  (let* ((mastodon-tl--show-avatars-p nil)
         (toot (cons '(favourited . t) mastodon-tl-test-base-toot))
         (timestamp (cdr (assoc 'created_at toot))))
    (with-mock
      (mock (date-to-time timestamp) => '(22782 21551))
      (mock (format-time-string mastodon-toot-timestamp-format '(22782 21551)) => "2999-99-99 00:11:22")

      (should (string= (substring-no-properties (mastodon-tl--byline toot))
                       "
 | (F) Account 42 (@acct42@example.space) 2999-99-99 00:11:22
  ------------")))))


(ert-deftest mastodon-tl--byline-boosted/favorited ()
  "Should format the boosted & favourited toot correctly."
  (let* ((mastodon-tl--show-avatars-p nil)
         (toot `((favourited . t) (reblogged . t) ,@mastodon-tl-test-base-toot))
         (timestamp (cdr (assoc 'created_at toot))))
    (with-mock
      (mock (date-to-time timestamp) => '(22782 21551))
      (mock (format-time-string mastodon-toot-timestamp-format '(22782 21551)) => "2999-99-99 00:11:22")

      (should (string= (substring-no-properties (mastodon-tl--byline toot))
                       "
 | (B) (F) Account 42 (@acct42@example.space) 2999-99-99 00:11:22
  ------------")))))

(ert-deftest mastodon-tl--byline-reblogged ()
  "Should format the reblogged toot correctly."
  (let* ((mastodon-tl--show-avatars-p nil)
         (toot mastodon-tl-test-base-boosted-toot)
         (original-toot (cdr (assoc 'reblog mastodon-tl-test-base-boosted-toot)))
         (timestamp (cdr (assoc 'created_at toot)))
         (original-timestamp (cdr (assoc 'created_at original-toot))))
    (with-mock
      ;; We don't expect to use the toot's timestamp but the timestamp of the
      ;; reblogged toot:
      (mock (date-to-time timestamp) => '(1 2))
      (mock (format-time-string mastodon-toot-timestamp-format '(1 2)) => "reblogging time")
      (mock (date-to-time original-timestamp) => '(3 4))
      (mock (format-time-string mastodon-toot-timestamp-format '(3 4)) => "original time")

      (should (string= (substring-no-properties (mastodon-tl--byline toot))
                      "
 | Account 42 (@acct42@example.space) Boosted Account 43 (@acct43@example.space) original time
  ------------")))))

(ert-deftest mastodon-tl--byline-reblogged-with-avatars ()
  "Should format the reblogged toot correctly."
  (let* ((mastodon-tl--show-avatars-p t)
         (toot mastodon-tl-test-base-boosted-toot)
         (original-toot (cdr (assoc 'reblog mastodon-tl-test-base-boosted-toot)))
         (timestamp (cdr (assoc 'created_at toot)))
         (original-timestamp (cdr (assoc 'created_at original-toot))))
    (with-mock
      ;; We don't expect to use the toot's timestamp but the timestamp of the
      ;; reblogged toot:
      (stub create-image => '(image "fake data"))
      (mock (date-to-time timestamp) => '(1 2))
      (mock (format-time-string mastodon-toot-timestamp-format '(1 2)) => "reblogging time")
      (mock (date-to-time original-timestamp) => '(3 4))
      (mock (format-time-string mastodon-toot-timestamp-format '(3 4)) => "original time")

      (should (string= (substring-no-properties (mastodon-tl--byline toot))
                      "
 |   Account 42 (@acct42@example.space) Boosted   Account 43 (@acct43@example.space) original time
  ------------")))))

(ert-deftest mastodon-tl--byline-reblogged-boosted/favorited ()
  "Should format the reblogged toot that was also boosted & favoritedcorrectly."
  (let* ((mastodon-tl--show-avatars-p nil)
         (toot `((favourited . t) (reblogged . t) ,@mastodon-tl-test-base-boosted-toot))
         (original-toot (cdr (assoc 'reblog mastodon-tl-test-base-boosted-toot)))
         (timestamp (cdr (assoc 'created_at toot)))
         (original-timestamp (cdr (assoc 'created_at original-toot))))
    (with-mock
      ;; We don't expect to use the toot's timestamp but the timestamp of the
      ;; reblogged toot:
      (mock (date-to-time timestamp) => '(1 2))
      (mock (format-time-string mastodon-toot-timestamp-format '(1 2)) => "reblogging time")
      (mock (date-to-time original-timestamp) => '(3 4))
      (mock (format-time-string mastodon-toot-timestamp-format '(3 4)) => "original time")

      (should (string= (substring-no-properties (mastodon-tl--byline toot))
                      "
 | (B) (F) Account 42 (@acct42@example.space) Boosted Account 43 (@acct43@example.space) original time
  ------------")))))

