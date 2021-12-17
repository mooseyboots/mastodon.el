;;; mastodon-search-test.el --- Tests for mastodon-search.el  -*- lexical-binding: nil -*-

(defconst mastodon-search--single-account-query
  '((id . "242971")
    (username . "mousebot")
    (acct . "mousebot")
    (display_name . ": ( ) { : | : & } ; :")
    (locked . t)
    (bot . :json-false)
    (discoverable . t)
    (group . :json-false)
    (created_at . "2020-04-14T00:00:00.000Z")
    (note . "<p>poetry, writing, dmt, desertion, trash, black metal, translation, hegel, language, autonomia....</p><p><a href=\"https://anarchive.mooo.com\" rel=\"nofollow noopener noreferrer\" target=\"_blank\"><span class=\"invisible\">https://</span><span class=\"\">anarchive.mooo.com</span><span class=\"invisible\"></span></a><br /><a href=\"https://pleasantlybabykid.tumblr.com/\" rel=\"nofollow noopener noreferrer\" target=\"_blank\"><span class=\"invisible\">https://</span><span class=\"\">pleasantlybabykid.tumblr.com/</span><span class=\"invisible\"></span></a><br />IG: <a href=\"https://bibliogram.snopyta.org/u/martianhiatus\" rel=\"nofollow noopener noreferrer\" target=\"_blank\"><span class=\"invisible\">https://</span><span class=\"ellipsis\">bibliogram.snopyta.org/u/marti</span><span class=\"invisible\">anhiatus</span></a><br />photos alt: <span class=\"h-card\"><a href=\"https://todon.eu/@goosebot\" class=\"u-url mention\">@<span>goosebot</span></a></span><br />git: <a href=\"https://git.blast.noho.st/mouse\" rel=\"nofollow noopener noreferrer\" target=\"_blank\"><span class=\"invisible\">https://</span><span class=\"\">git.blast.noho.st/mouse</span><span class=\"invisible\"></span></a></p><p>want to trade chapbooks or zines? hmu!</p><p>he/him or they/them</p>")
    (url . "https://todon.nl/@mousebot")
    (avatar . "https://todon.nl/system/accounts/avatars/000/242/971/original/0a5e801576af597b.jpg")
    (avatar_static . "https://todon.nl/system/accounts/avatars/000/242/971/original/0a5e801576af597b.jpg")
    (header . "https://todon.nl/system/accounts/headers/000/242/971/original/f85f7f1048237fd4.jpg")
    (header_static . "https://todon.nl/system/accounts/headers/000/242/971/original/f85f7f1048237fd4.jpg")
    (followers_count . 226)
    (following_count . 634)
    (statuses_count . 3807)
    (last_status_at . "2021-11-05")
    (emojis .
            [])
    (fields .
            [((name . "dark to")
              (value . "themselves")
              (verified_at))
             ((name . "its raining")
              (value . "plastic")
              (verified_at))
             ((name . "dis")
              (value . "integration")
              (verified_at))
             ((name . "ungleichzeitigkeit und")
              (value . "gleichzeitigkeit, philosophisch")
              (verified_at))]))
  "A sample mastodon account search result (parsed json)")

(defconst mastodon-search--test-single-tag
  '((name . "TeamBringBackVisibleScrollbars")
    (url . "https://todon.nl/tags/TeamBringBackVisibleScrollbars")
    (history . [((day . "1636156800") (uses . "0") (accounts . "0"))
                ((day . "1636070400") (uses . "0") (accounts . "0"))
                ((day . "1635984000") (uses . "0") (accounts . "0"))
                ((day . "1635897600") (uses . "0") (accounts . "0"))
                ((day . "1635811200") (uses . "0") (accounts . "0"))
                ((day . "1635724800") (uses . "0") (accounts . "0"))
                ((day . "1635638400") (uses . "0") (accounts . "0"))])))

(defconst mastodon-search--test-single-status
  '((id . "107230316503209282")
    (created_at . "2021-11-06T13:19:40.628Z")
    (in_reply_to_id)
    (in_reply_to_account_id)
    (sensitive . :json-false)
    (spoiler_text . "")
    (visibility . "direct")
    (language . "en")
    (uri . "https://todon.nl/users/mousebot/statuses/107230316503209282")
    (url . "https://todon.nl/@mousebot/107230316503209282")
    (replies_count . 0)
    (reblogs_count . 0)
    (favourites_count . 0)
    (favourited . :json-false)
    (reblogged . :json-false)
    (muted . :json-false)
    (bookmarked . :json-false)
    (content . "<p>This is a nice test toot, for testing purposes. Thank you.</p>")
    (reblog)
    (application
     (name . "mastodon.el")
     (website . "https://github.com/jdenen/mastodon.el"))
    (account
     (id . "242971")
     (username . "mousebot")
     (acct . "mousebot")
     (display_name . ": ( ) { : | : & } ; :")
     (locked . t)
     (bot . :json-false)
     (discoverable . t)
     (group . :json-false)
     (created_at . "2020-04-14T00:00:00.000Z")
     (note . "<p>poetry, writing, dmt, desertion, trash, black metal, translation, hegel, language, autonomia....</p><p><a href=\"https://anarchive.mooo.com\" rel=\"nofollow noopener noreferrer\" target=\"_blank\"><span class=\"invisible\">https://</span><span class=\"\">anarchive.mooo.com</span><span class=\"invisible\"></span></a><br /><a href=\"https://pleasantlybabykid.tumblr.com/\" rel=\"nofollow noopener noreferrer\" target=\"_blank\"><span class=\"invisible\">https://</span><span class=\"\">pleasantlybabykid.tumblr.com/</span><span class=\"invisible\"></span></a><br />IG: <a href=\"https://bibliogram.snopyta.org/u/martianhiatus\" rel=\"nofollow noopener noreferrer\" target=\"_blank\"><span class=\"invisible\">https://</span><span class=\"ellipsis\">bibliogram.snopyta.org/u/marti</span><span class=\"invisible\">anhiatus</span></a><br />photos alt: <span class=\"h-card\"><a href=\"https://todon.eu/@goosebot\" class=\"u-url mention\">@<span>goosebot</span></a></span><br />git: <a href=\"https://git.blast.noho.st/mouse\" rel=\"nofollow noopener noreferrer\" target=\"_blank\"><span class=\"invisible\">https://</span><span class=\"\">git.blast.noho.st/mouse</span><span class=\"invisible\"></span></a></p><p>want to trade chapbooks or zines? hmu!</p><p>he/him or they/them</p>")
     (url . "https://todon.nl/@mousebot")
     (avatar . "https://todon.nl/system/accounts/avatars/000/242/971/original/0a5e801576af597b.jpg")
     (avatar_static . "https://todon.nl/system/accounts/avatars/000/242/971/original/0a5e801576af597b.jpg")
     (header . "https://todon.nl/system/accounts/headers/000/242/971/original/f85f7f1048237fd4.jpg")
     (header_static . "https://todon.nl/system/accounts/headers/000/242/971/original/f85f7f1048237fd4.jpg")
     (followers_count . 226)
     (following_count . 634)
     (statuses_count . 3807)
     (last_status_at . "2021-11-05")
     (emojis . [])
     (fields . [((name . "dark to")
                 (value . "themselves")
                 (verified_at))
                ((name . "its raining")
                 (value . "plastic")
                 (verified_at))
                ((name . "dis")
                 (value . "integration")
                 (verified_at))
                ((name . "ungleichzeitigkeit und")
                 (value . "gleichzeitigkeit, philosophisch")
                 (verified_at))]))
    (media_attachments . [])
    (mentions . [((id . "242971")
                  (username . "mousebot")
                  (url . "https://todon.nl/@mousebot")
                  (acct . "mousebot"))])
    (tags . [])
    (emojis . [])
    (card)
    (poll)))

(ert-deftest mastodon-search--get-user-info-@ ()
  "Should build a list from a single account for company completion."
  (should
   (equal
    (mastodon-search--get-user-info-@ mastodon-search--single-account-query)
    '(": ( ) { : | : & } ; :" "@mousebot" "https://todon.nl/@mousebot"))))

(ert-deftest mastodon-search--get-user-info ()
  "Should build a list from a single account for company completion."
  (should
   (equal
    (mastodon-search--get-user-info mastodon-search--single-account-query)
    '(": ( ) { : | : & } ; :" "mousebot" "https://todon.nl/@mousebot"))))

(ert-deftest mastodon-search--get-hashtag-info ()
  "Should build a list of hashtag name and URL."
  (should
   (equal
    (mastodon-search--get-hashtag-info mastodon-search--test-single-tag)
    '("TeamBringBackVisibleScrollbars"
      "https://todon.nl/tags/TeamBringBackVisibleScrollbars"))))

(ert-deftest mastodon-search--get-status-info ()
  "Should return a list of ID, timestamp, content, and spoiler."
  (should
   (equal
    (mastodon-search--get-status-info mastodon-search--test-single-status)
    '("107230316503209282"
      "2021-11-06T13:19:40.628Z"
      ""
      "<p>This is a nice test toot, for testing purposes. Thank you.</p>"))))
