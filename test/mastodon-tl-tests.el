(require 'el-mock)

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
      (mastodon-tl--more-json "foo" 12345))))
