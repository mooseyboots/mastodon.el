;;; mastodon-tl.el --- HTTP request/response functions for mastodon.el

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
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

;;; Code:

(defun mastodon-media--image-from-url (url)
  "Takes a url and returns an image"
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
         (let ((data (with-current-buffer buffer
                       (goto-char (point-min))
                       (search-forward "\n\n")
                       (buffer-substring (point) (point-max)))))
	   (insert "\n")
           (insert-image (create-image data nil t)))
      (kill-buffer buffer))))

(defun mastodon-media--select-media-line(start)
  "Returns the list of line coordinates of a line that

contains `Media_Links::'"
  (when start (goto-char (point-min)))
  (search-forward-regexp "Media_Link::" nil nil nil)
  (let ((start (progn (move-beginning-of-line '()) (point)))
	(end (progn (move-end-of-line '()) (point))))
    (list start end)))

(defun mastodon-media--select-first-media-line()
  (mastodon-media--select-media-line 1))

(defun mastodon-media--check-missing(link)
  "Checks to make sure that the missing string has 

not been returned."
  (let((missing "/files/small/missing.png"))
    (not(equal link missing))))

(defun mastodon-media--select-next-media-line()
  (mastodon-media--select-media-line '()))

(defun mastodon-media--line-to-link(line)
    "Removes the identifier from the media line leaving

just a url"
  (replace-regexp-in-string "Media_Link:: " ""
			    (buffer-substring
			     (car line)
			     (cadr line))))

(defun mastodon-media--delete-line(line)
  "Deletes the current media line"
  (delete-region (car line) (cadr line)))

(defun mastodon-media--inline-images-aux ( not-first)
  "Recursivly goes through all of the `Media_Links:' in the buffer"
  (let* ((line (mastodon-media--select-media-line (not not-first)))
	(link (mastodon-media--line-to-link line)))
    (when (mastodon-media--check-missing link)
      (progn (mastodon-media--image-from-url link)
	     (mastodon-media--delete-line line))))
  (mastodon-inline-images-aux 1))

(defun mastodon-media--inline-images()
  "A wrapper for the `mastodon-media--inline-images-aux' that catches

errors thrown by reaching the end of the buffer"
  (interactive)
  (condition-case nil
      (progn (mastodon-media--inline-images-aux '()) t)
  (error nil)))

(provide 'mastodon-media)
;;; mastodon-media.el ends here
