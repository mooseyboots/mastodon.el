(require 'el-mock)
(require 'cl-macs)

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

(ert-deftest more-json-id-string ()
  "Should request toots older than max_id.

`mastodon-tl--more-json' should accept and id that is either
a string or a numeric."
  (let ((mastodon-instance-url "https://instance.url"))
    (with-mock
      (mock (mastodon-http--get-json "https://instance.url/api/v1/timelines/foo?max_id=12345"))
      (mastodon-tl--more-json "timelines/foo" "12345"))))

(ert-deftest update-json-id-string ()
  "Should request toots more recent than since_id.

`mastodon-tl--updated-json' should accept and id that is either
a string or a numeric."  
  (let ((mastodon-instance-url "https://instance.url"))
    (with-mock
      (mock (mastodon-http--get-json "https://instance.url/api/v1/timelines/foo?since_id=12345"))
      (mastodon-tl--updated-json "timelines/foo" "12345"))))

(ert-deftest mastodon-tl--relative-time-description ()
  "Should format relative time as expected"
  (cl-labels ((minutes (n) (* n 60))
              (hours (n) (* n (minutes 60)))
              (days (n) (* n (hours 24)))
              (weeks (n) (* n (days 7)))
              (years (n) (* n (days 365)))
              (format-seconds-since (seconds)
                                    (let ((timestamp (time-subtract (current-time) (seconds-to-time seconds))))
                                      (mastodon-tl--relative-time-description timestamp)))
              (check (seconds expected)
                     (should (string= (format-seconds-since seconds) expected))))
    (check 1 "less than a minute ago")
    (check 59 "less than a minute ago")
    (check 60 "one minute ago")
    (check 89 "one minute ago")            ;; rounding down
    (check 91 "2 minutes ago")             ;; rounding up
    (check (minutes 3.49) "3 minutes ago") ;; rounding down
    (check (minutes 3.52) "4 minutes ago")
    (check (minutes 59) "59 minutes ago")
    (check (minutes 60) "one hour ago")
    (check (minutes 89) "one hour ago")
    (check (minutes 91) "2 hours ago")
    (check (hours 3.49) "3 hours ago") ;; rounding down
    (check (hours 3.51) "4 hours ago") ;; rounding down
    (check (hours 23.4) "23 hours ago")
    (check (hours 23.6) "one day ago") ;; rounding up
    (check (days 1.48) "one day ago")  ;; rounding down
    (check (days 1.52) "2 days ago")   ;; rounding up
    (check (days 6.6) "one week ago")  ;; rounding up
    (check (weeks 2.49) "2 weeks ago") ;; rounding down
    (check (weeks 2.51) "3 weeks ago") ;; rounding down
    (check (1- (weeks 52)) "52 weeks ago")
    (check (weeks 52) "one year ago")
    (check (years 2.49) "2 years ago") ;; rounding down
    (check (years 2.51) "3 years ago") ;; rounding down
    ))

(ert-deftest mastodon-tl--relative-time-details--next-update ()
  "Should calculate the next update time information as expected"
  (let ((current-time (current-time)))
    (cl-labels ((minutes (n) (* n 60))
                (hours (n) (* n (minutes 60)))
                (days (n) (* n (hours 24)))
                (weeks (n) (* n (days 7)))
                (years (n) (* n (days 365.25)))
                (next-update (seconds-ago)
                             (let* ((timestamp (time-subtract current-time
                                                              (seconds-to-time seconds-ago))))
                               (cdr (mastodon-tl--relative-time-details timestamp current-time))))
                (check (seconds-ago)
                       (let* ((timestamp (time-subtract current-time (seconds-to-time seconds-ago)))
                              (at-now (mastodon-tl--relative-time-description timestamp current-time))
                              (at-one-second-before (mastodon-tl--relative-time-description
                                                     timestamp
                                                     (time-subtract (next-update seconds-ago)
                                                                    (seconds-to-time 1))))
                              (at-result (mastodon-tl--relative-time-description
                                          timestamp
                                          (next-update seconds-ago))))
                         (when nil  ;; change to t to debug test failures
                           (prin1 (format "\nFor %s: %s / %s"
                                          seconds-ago
                                          (time-to-seconds
                                           (time-subtract (next-update seconds-ago)
                                                          timestamp))
                                          (round
                                           (time-to-seconds
                                            (time-subtract (next-update seconds-ago)
                                                           current-time))))))
                         ;; a second earlier the description is the same as at current time
                         (should (string= at-now at-one-second-before))
                         ;; but at the result time it is different
                         (should-not (string= at-one-second-before at-result)))))
      (check 0)
      (check 1)
      (check 59)
      (check 60)
      (check 89)
      (check 90)
      (check 149)
      (check 150)
      (check (1- (hours 1.5))) ;; just before we switch from "one hour" to "2 hours"
      (check (hours 1.5))
      (check (hours 2.1))
      (check (1- (hours 23.5))) ;; just before "23 hours" -> "one day"
      (check (hours 23.5))
      (check (1- (days 1.5))) ;; just before "one day" -> "2 days"
      (check (days 1.5)) ;; just before "one day" -> "2 days"
      (check (days 2.1))
      (check (1- (days 6.5))) ;; just before "6 days" -> "one week"
      (check (days 6.5)) ;; "one week" -> "2 weeks"
      (check (weeks 2.1))
      (check (1- (weeks 52))) ;; just before "52 weeks" -> "one year"
      (check (weeks 52))
      (check (days 365))
      (check (days 366))
      (check (years 2.1))
      )))

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

(ert-deftest mastodon-tl--byline-timestamp-has-relative-display ()
  "Should display the timestamp with a relative time."
  (let ((mastodon-tl--show-avatars-p nil)
        (timestamp (cdr (assoc 'created_at mastodon-tl-test-base-toot))))
    (with-mock
      (mock (date-to-time timestamp) => '(22782 21551))
      (mock (current-time) => '(22782 22000))
      (mock (format-time-string mastodon-toot-timestamp-format '(22782 21551)) => "2999-99-99 00:11:22")

      (let* ((formatted-string (mastodon-tl--byline mastodon-tl-test-base-toot))
             (timestamp-start (string-match "2999-99-99" formatted-string))
             (properties (text-properties-at timestamp-start formatted-string)))
        (should (equal '(22782 21551) (plist-get properties 'timestamp)))
        (should (string-equal "7 minutes ago" (plist-get properties 'display)))))))

(ert-deftest mastodon-tl--consider-timestamp-for-updates-no-active-callback ()
  "Should update the timestamp update variables as expected."

  (let* ((now (current-time))
         (soon-in-the-future (time-add now (seconds-to-time 10000)))
         (long-in-the-future (time-add now (seconds-to-time 10000000))))
    (with-temp-buffer
      ;; start with timer way into the future and no active callback
      (setq mastodon-tl--timestamp-next-update long-in-the-future
            mastodon-tl--timestamp-update-timer nil)

      ;; something a later update doesn't update:
      (with-mock
        (mock (mastodon-tl--relative-time-details 'fake-timestamp) =>
              (cons "xxx ago" (time-add long-in-the-future (seconds-to-time 100))))

        (mastodon-tl--consider-timestamp-for-updates 'fake-timestamp)

        (should (null mastodon-tl--timestamp-update-timer))
        (should (eq mastodon-tl--timestamp-next-update long-in-the-future)))

      ;; something only shortly sooner doesn't update:
      (with-mock
        (mock (mastodon-tl--relative-time-details 'fake-timestamp) =>
              (cons "xxx ago" (time-subtract long-in-the-future (seconds-to-time 9))))

        (mastodon-tl--consider-timestamp-for-updates 'fake-timestamp)

        (should (null mastodon-tl--timestamp-update-timer))
        (should (eq mastodon-tl--timestamp-next-update long-in-the-future)))

      ;; something much sooner, does update
      (with-mock
        (mock (mastodon-tl--relative-time-details 'fake-timestamp) =>
              (cons "xxx ago"  soon-in-the-future))

        (mastodon-tl--consider-timestamp-for-updates 'fake-timestamp)

        (should (null mastodon-tl--timestamp-update-timer))
        (should (eq mastodon-tl--timestamp-next-update soon-in-the-future)))
      )))

(ert-deftest mastodon-tl--consider-timestamp-for-updates-with-active-callback ()
  "Should update the timestamp update variables as expected."

  (let* ((now (current-time))
         (soon-in-the-future (time-add now (seconds-to-time 10000)))
         (long-in-the-future (time-add now (seconds-to-time 10000000))))
    (with-temp-buffer
      ;; start with timer way into the future and no active callback
      (setq mastodon-tl--timestamp-next-update long-in-the-future
            mastodon-tl--timestamp-update-timer 'initial-timer)

      ;; something a later update doesn't update:
      (with-mock
        (mock (mastodon-tl--relative-time-details 'fake-timestamp) =>
              (cons "xxx ago" (time-add long-in-the-future (seconds-to-time 100))))

        (mastodon-tl--consider-timestamp-for-updates 'fake-timestamp)

        (should (eq 'initial-timer mastodon-tl--timestamp-update-timer))
        (should (eq mastodon-tl--timestamp-next-update long-in-the-future)))

      ;; something much sooner, does update
      (with-mock
        (mock (mastodon-tl--relative-time-details 'fake-timestamp) =>
              (cons "xxx ago"  soon-in-the-future))
        (mock (cancel-timer 'initial-timer))
        (mock (run-at-time soon-in-the-future nil
                           #'mastodon-tl--update-timestamps-callback
                           (current-buffer) nil) => 'new-timer)

        (mastodon-tl--consider-timestamp-for-updates 'fake-timestamp)

        (should (eq 'new-timer mastodon-tl--timestamp-update-timer))
        (should (eq mastodon-tl--timestamp-next-update soon-in-the-future)))
      )))

(ert-deftest mastodon-tl--find-property-range--no-tag ()
  "Should cope with a buffer completely lacking the tag."
  (with-temp-buffer
    (insert "Just some random text")
    (insert (propertize "More text with a different property" 'other-property 'set))

    (should (null (mastodon-tl--find-property-range 'test-property 2)))))

(ert-deftest mastodon-tl--find-property-range--earlier-tag ()
  "Should cope with a buffer completely lacking the tag."
  (with-temp-buffer
    (insert (propertize "Just some text with a the sought property" 'test-property 'set))
    (let ((end-of-region (point)))
      (insert "More random text")

      (should (null (mastodon-tl--find-property-range 'test-property end-of-region))))))

(ert-deftest mastodon-tl--find-property-range--successful-finding ()
  "Should find the sought tag in all expected circumstances."
  (with-temp-buffer
    (insert "Previous text")
    (let ((start-of-region (point))
          end-of-region)
      (insert (propertize "Just some text with a the sought property" 'test-property 'set))
      (setq end-of-region (point))
      (insert "More random text")

      ;; before the region
      (should (equal (cons start-of-region end-of-region)
                     (mastodon-tl--find-property-range 'test-property 1)))
      ;; in the region
      (should (equal (cons start-of-region end-of-region)
                     (mastodon-tl--find-property-range 'test-property (+ 2 start-of-region))))
      ;; at end of region
      (should (equal (cons start-of-region end-of-region)
                     (mastodon-tl--find-property-range 'test-property (1- end-of-region)))))))

(ert-deftest mastodon-tl--find-property-range--successful-finding-at-start ()
  "Should cope with a tag at start."
  (with-temp-buffer
    (insert (propertize "Just some text with a the sought property" 'test-property 'set))
    (let ((end-of-region (point)))
      (insert "More random text")

      ;; at start of the region
      (should (equal (cons 1 end-of-region)
                     (mastodon-tl--find-property-range 'test-property 1)))
      ;; in the region
      (should (equal (cons 1 end-of-region)
                     (mastodon-tl--find-property-range 'test-property 3)))
      ;; at end of region
      (should (equal (cons 1 end-of-region)
                     (mastodon-tl--find-property-range 'test-property (1- end-of-region)))))))

(ert-deftest mastodon-tl--find-property-range--successful-finding-at-end ()
  "Should cope with a tag at end."
  (with-temp-buffer
    (insert "More random text")
    (let ((start-of-region (point))
          end-of-region)
      (insert (propertize "Just some text with a the sought property" 'test-property 'set))
      (setq end-of-region (point-max))

      ;; before the region
      (should (equal (cons start-of-region end-of-region)
                     (mastodon-tl--find-property-range 'test-property 1)))
      ;; in the region
      (should (equal (cons start-of-region end-of-region)
                     (mastodon-tl--find-property-range 'test-property (1+ start-of-region))))
      ;; at end of region
      (should (equal (cons start-of-region end-of-region)
                     (mastodon-tl--find-property-range 'test-property (1- end-of-region)))))))

(ert-deftest mastodon-tl--find-property-range--successful-finding-whole-buffer ()
  "Should cope with a tag being set for the whole buffer."
  (with-temp-buffer
    (insert (propertize "Just some text with a the sought property" 'test-property 'set))

    ;; before the region
    (should (equal (cons (point-min) (point-max))
                   (mastodon-tl--find-property-range 'test-property 2)))))

(defun tl-tests--all-regions-with-property (property)
  "Returns a list with (start . end) regions where PROPERTY is set."
  (let (result
        region)
    (goto-char (point-min))
    (while (and (< (point) (point-max))
                (setq region (mastodon-tl--find-property-range property (point))))
      (push region result)
      (goto-char (min (point-max) (cdr region))))
    (nreverse result)))

(defun tl-tests--property-values-at (property ranges)
  "Returns a list with property values at the given ranges.

The property value for PROPERTY within a region is assumed to be
constant."
  (let (result)
    (dolist (range ranges (nreverse result))
      (push (get-text-property (car range) property) result))))

(ert-deftest mastodon-tl--update-timestamps-callback ()
  "Should update the 5 timestamps at a time as expected."
  (let ((now (current-time))
        markers)
    (cl-labels ((insert-timestamp (n)
                                  (insert (format "\nSome text before timestamp %s:" n))
                                  (insert (propertize
                                           (format "timestamp #%s" n)
                                           'timestamp (time-subtract now (seconds-to-time (* 60 n)))
                                           'display (format "unset %s" n)))
                                  (push (copy-marker (point)) markers)
                                  (insert " some more text.")))
      (with-temp-buffer
        (cl-dotimes (n 12) (insert-timestamp (+ n 2)))
        (setq markers (nreverse markers))
        
        (with-mock
          (mock (current-time) => now)
          (stub run-at-time => 'fake-timer)

          ;; make the initial call
          (mastodon-tl--update-timestamps-callback (current-buffer) nil)
          (should (equal '("2 minutes ago" "3 minutes ago" "4 minutes ago" "5 minutes ago" "6 minutes ago"
                           "unset 7" "unset 8" "unset 9" "unset 10" "unset 11" "unset 12" "unset 13")
                         (tl-tests--property-values-at 'display
                                                       (tl-tests--all-regions-with-property 'timestamp))))

          ;; fake the follow-up call
          (mastodon-tl--update-timestamps-callback (current-buffer) (nth 4 markers))
          (should (equal '("2 minutes ago" "3 minutes ago" "4 minutes ago" "5 minutes ago" "6 minutes ago"
                           "7 minutes ago" "8 minutes ago" "9 minutes ago" "10 minutes ago" "11 minutes ago"
                           "unset 12" "unset 13")
                         (tl-tests--property-values-at 'display
                                                       (tl-tests--all-regions-with-property 'timestamp))))
          (should (null (marker-position (nth 4 markers))))

          ;; fake the follow-up call
          (mastodon-tl--update-timestamps-callback (current-buffer) (nth 9 markers))
          (should (equal '("2 minutes ago" "3 minutes ago" "4 minutes ago" "5 minutes ago" "6 minutes ago"
                           "7 minutes ago" "8 minutes ago" "9 minutes ago" "10 minutes ago" "11 minutes ago"
                           "12 minutes ago" "13 minutes ago")
                         (tl-tests--property-values-at 'display
                                                       (tl-tests--all-regions-with-property 'timestamp))))
          (should (null (marker-position (nth 9 markers)))))))))
