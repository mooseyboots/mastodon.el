(require 'cl-lib)
(require 'cl-macs)
(require 'el-mock)

(defconst mastodon-notifications-test-base-mentioned
  '((id . "1234")
    (type . "mention")
    (created_at . "2018-03-06T04:27:21.288Z" )
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
    (status (id . 61208)
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
            (reblog))))

(defconst mastodon-notifications-test-base-favourite
  '((id . "1234")
    (type . "favourite")
    (created_at . "2018-03-06T04:27:21.288Z" )
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
    (status (id . 61208)
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
            (reblog))))

(defconst mastodon-notifications-test-base-boosted
  '((id . "1234")
    (type . "reblog")
    (created_at . "2018-03-06T04:27:21.288Z" )
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
    (status (id . 61208)
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
            (reblog))))

(defconst mastodon-notifications-test-base-followed
  '((id . "1234")
    (type . "follow")
    (created_at . "2018-03-06T04:27:21.288Z" )
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
    (status (id . 61208)
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
            (reblog))))

(defconst mastodon-notifications-test-base-favourite
  '((id . "1234")
    (type . "mention")
    (created_at . "2018-03-06T04:27:21.288Z" )
    (account (id . 42)
             (username . "acct42")
             (acct . "acct42@example.space")
             (display_name . "Account 42")
             (locked . :json-false)
             (created_at . "2017-04-01T00:00:00.000Z")
             (followers_count . 99)
             (following_count . 13)
             (statuses_count . 101)
             (note . "E"))))

(ert-deftest notification-get ()
  "Ensure get request format for notifictions is accurate."
  (let ((mastodon-instance-url "https://instance.url"))
    (with-mock
      (mock (mastodon-http--get-json "https://instance.url/api/v1/notifications"))
      (mastodon-notifications--get))))

(defun mastodon-notifications--test-type (fun sample)
  "Test notification draw functions.

FUN is the notificiation function to be called and SAMPLE is the
notification to be tested."
  (let ((mastodon-tl--show-avatars-p nil)
        (timestamp (cdr (assoc 'created_at  sample))))
    (with-temp-buffer (funcall fun sample)
                      (buffer-substring-no-properties (point-min) (point-max)))))

(ert-deftest mastodon-notifications--test-boost ()
  "Ensure boost notification is formated properly."
  (should (string= (mastodon-notifications--test-type
                    'mastodon-notifications--reblog
                    mastodon-notifications-test-base-boosted)
                   "Just some text
 | Account 42 (@acct42@example.space) Boosted your status reblogging time
  ------------

")))

(ert-deftest mastodon-notifications--test-mention ()
  "Ensure mention notification is formated properly."
  (should (string= (mastodon-notifications--test-type
                    'mastodon-notifications--mention
                    mastodon-notifications-test-base-mentioned)
                   "Just some text
 | Account 42 (@acct42@example.space) Mentioned you reblogging time
  ------------

")))

(ert-deftest mastodon-notifications--test-follow ()
  "Ensure follow notification is formated properly."
  (should (string= (mastodon-notifications--test-type
                    'mastodon-notifications--follow
                    mastodon-notifications-test-base-followed)
                   "Congratulations, you have a new follower!
 | Account 42 (@acct42@example.space) Followed you reblogging time
  ------------

")))

(ert-deftest mastodon-notifications--test-favourite ()
  "Ensure favourite notification is formated properly."
  (should (string= (mastodon-notifications--test-type
                    'mastodon-notifications--favourite
                    mastodon-notifications-test-base-favourite)
                   "Just some text
 | Account 42 (@acct42@example.space) Favourited your status reblogging time
  ------------

")))
         
(ert-deftest mastodon-notifications--test-byline-concat ()
  "Ensure proper suffix is appended to action."
  (should (and
           (string= " Mentioned you"
                    (mastodon-notifications--byline-concat "Mentioned"))
           (string= " Followed you"
                    (mastodon-notifications--byline-concat "Followed"))
           (string= " Favourited your status"
                    (mastodon-notifications--byline-concat "Favourited"))
           (string= " Boosted your status"
                    (mastodon-notifications--byline-concat "Boosted")))))


