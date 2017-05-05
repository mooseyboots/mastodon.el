;;; mastodon-media.el --- Functions for inlining Mastodon media  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.6.2
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

;; mastodon-media.el provides functions for inlining media.

;; Known bug gnutls -12 when trying to access images on some systems.
;; It looks like their may be a version mismatch between the encryption
;; required by the server and client.

;;; Code:
(require 'mastodon-http  nil t)

(defgroup mastodon-media nil
  "Inline Mastadon media."
  :prefix "mastodon-media-"
  :group 'mastodon)

(defun mastodon-media--image-from-url (url)
  "Takes a URL and return an image."
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (let ((data (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (buffer-substring (point) (point-max)))))
          (insert "\n")
          (insert-image (create-image data nil t)))
      (kill-buffer buffer))))

(defun mastodon-media--select-next-media-line ()
  "Find coordinates of a line that contains `Media_Links::'

Returns the cons of (`start' . `end') points of that line or nil no
more media links were found."
  (let ((foundp (search-forward-regexp "Media_Link::" nil t)))
    (when foundp
      (let ((start (progn (move-beginning-of-line nil) (point)))
            (end (progn (move-end-of-line nil) (point))))
        (cons start end)))))

(defun mastodon-media--valid-link-p (link)
  "Checks to make sure that the missing string has

not been returned."
  (let ((missing "/files/small/missing.png"))
    (not (equal link missing))))

(defun mastodon-media--line-to-link (line-points)
  "Returns the url of the media link given at the given point.

`LINE-POINTS' is a cons of (`start' . `end') positions of the line with
the `Media_Link:: <url>' text."
  (replace-regexp-in-string "Media_Link:: " ""
                            (buffer-substring
                             (car line-points)
                             (cdr line-points))))

(defun mastodon-media--delete-line (line)
  "Deletes the current media line"
  (delete-region (car line) (cdr line)))

(defun mastodon-media--inline-images ()
  "Find all `Media_Links:' in the buffer replacing them with the referenced image."
  (interactive)
  (goto-char (point-min))
  (let (line-coordinates)
    (while (setq line-coordinates (mastodon-media--select-next-media-line))
      (let ((link (mastodon-media--line-to-link line-coordinates)))
        (when (mastodon-media--valid-link-p link)
          (mastodon-media--image-from-url link)
          (mastodon-media--delete-line line-coordinates))))))

(provide 'mastodon-media)
;;; mastodon-media.el ends here
