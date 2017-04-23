(require 'el-mock)

(ert-deftest cancel ()
  (with-mock
    (mock (kill-buffer-and-window))
    (should (eq nil (mastodon-toot--cancel)))))
