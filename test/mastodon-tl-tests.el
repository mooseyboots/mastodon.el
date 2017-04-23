(require 'el-mock)

(ert-deftest remove-html-1 ()
  "Should remove all <span> tags."
  (let ((input "<span class=\"h-card\">foobar</span> <span>foobaz</span>"))
    (should (string= (mastodon-tl--remove-html input) "foobar foobaz"))))

(ert-deftest remove-html-2 ()
  "Should replace <\p> tags with two new lines."
  (let ((input "foobar</p>"))
    (should (string= (mastodon-tl--remove-html input) "foobar\n\n"))))
