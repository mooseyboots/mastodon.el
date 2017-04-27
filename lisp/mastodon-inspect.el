;;; mastodon-inspect.el --- Client for Mastodon

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.6.0
;; Package-Requires: ((emacs "24.4"))
;; Homepage: https://github.com/jdenen/mastodon.el

;; This file is not part of GNU Emacs.

;; This file is part of mastodon.el.

;; mastodon.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mastodon.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mastodon.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some tools to help inspect / debug mastodon.el

;;; Code:

(require 'mastodon-tl nil t)

(defgroup mastodon-inspect nil
  "Tools to help inspect toots."
  :prefix "mastodon-inspect-"
  :group 'external)

(defun mastodon-inspect--dump-json-in-buffer (name json)
  "Buffer NAME is opened and JSON in printed into it."
  (switch-to-buffer-other-window name)
  (progn (setf print-level nil
            print-length nil)
         (insert (pp json t))
         (goto-char 1)
         (emacs-lisp-mode)
         (message "success")))

(defun mastodon-inspect--toot ()
  "Find next toot and dump its meta data into new buffer."
  (interactive)
  (mastodon-inspect--dump-json-in-buffer
   (concat "*mastodon-inspect-toot-"
           (int-to-string (mastodon-tl--property 'toot-id))
           "*")
  (mastodon-tl--property 'toot-json)))

(provide 'mastodon-inspect)
;;; mastodon-inspect.el ends here
