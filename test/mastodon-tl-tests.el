(require 'el-mock)
(load-file "../lisp/mastodon-tl.el")

(ert-deftest mastodon-tl:from-toot ()
  "Should return the value for KEY in a list."
  (should (string= (mastodon-tl--from-toot "foo" '(("foo" . "bar"))) "bar")))

(ert-deftest mastodon-tl:remove-html:remove-p-and-span ()
  "Should remove <p> and <span> tags that are not parsed by `html2text'."
  (let ((input "<p>foo<span>bar</span></p>"))
    (should (string= (mastodon-tl--remove-html input) "foobar\n"))))

(ert-deftest mastodon-tl:remove-html:remove-hcard-span ()
  "Should remove <span> tags with a class of 'h-card'."
  (let ((input "<span class=\"h-card\">foobar</span>"))
    (should (string= (mastodon-tl--remove-html input) "foobar"))))
