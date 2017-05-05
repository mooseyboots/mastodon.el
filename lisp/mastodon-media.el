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
(require 'mastodon)

(defgroup mastodon-media nil
  "Inline Mastadon media."
  :prefix "mastodon-media-"
  :group 'mastodon)

(defvar mastodon-media-show-avatars-p
  (image-type-available-p 'imagemagick)
  "A boolean value stating whether to show avatars in timelines.")

(defun mastodon-media--process-image-response (status-plist marker image-options region-length image-url)
  "Callback function processing the url retrieve response for URL.

STATUS-PLIST is the usual plist of status events as per `url-retrieve'.
IMAGE-OPTIONS are the precomputed options to apply to the image.
MARKER is the marker to where the response should be visible.
REGION-LENGTH is the length of the region that should be replaced with the image.
IMAGE-URL is the URL that was retrieved.
"
  (let ((url-buffer (current-buffer))
        (is-error-response-p (eq :error (car status-plist))))
    (unwind-protect
        (let* ((data (unless is-error-response-p
                       (goto-char (point-min))
                       (search-forward "\n\n")
                       (buffer-substring (point) (point-max))))
               (image (when data
                        (apply #'create-image data (when image-options 'imagemagick)
                               t image-options))))
          (switch-to-buffer (marker-buffer marker))
          ;; Save narrowing in our buffer
          (let ((inhibit-read-only t))
            (save-restriction
              (widen)
              (put-text-property marker (+ marker region-length) 'media-state 'loaded)
              (put-text-property marker (+ marker region-length)
                                 'display (or
                                           image
                                           (format "Failed to load %s" image-url)))
              ;; We are done with the marker; release it:
              (set-marker marker nil)))
          (kill-buffer url-buffer)))))

(defun mastodon-media--load-image-from-url (url media-type start region-length)
  "Takes a URL and MEDIA-TYPE and return an image.

MEDIA-TYPE is a symbol and either 'avatar or 'media-link."
  ;; TODO: Cache the avatars
  (let ((image-options (when (image-type-available-p 'imagemagick)
                         (pcase media-type
                           ('avatar `(:height ,mastodon-avatar-height))
                           ('media-link `(:max-height ,mastodon-preview-max-height))))))
    (url-retrieve url
                  #'mastodon-media--process-image-response
                  (list (copy-marker start) image-options region-length url))))

(defun mastodon-media--select-next-media-line ()
  "Find coordinates of the next media to load.

Returns the list of (`start' . `end', `media-symbol') points of
that line and string found or nil no more media links were
found."
  (let ((next-pos (point)))
    (while (and (setq next-pos (next-single-property-change next-pos 'media-state))
                (or (not (eq 'needs-loading (get-text-property next-pos 'media-state)))
                    (null (get-text-property next-pos 'media-url))
                    (null (get-text-property next-pos 'media-type))))
      ;; do nothing - the loop will proceed
      )
    (when next-pos
      (pcase (get-text-property next-pos 'media-type)
        ;; Avatars are just one character in the buffer
        ('avatar (list next-pos (+ next-pos 1) 'avatar))
        ;; Media links are 5 character ("[img]")
        ('media-link (list next-pos (+ next-pos 5) 'media-link))))))

(defun mastodon-media--valid-link-p (link)
  "Checks to make sure that the missing string has

not been returned."
  (let ((missing "/files/small/missing.png"))
    (and link
         (not (equal link missing)))))

(defun mastodon-media--inline-images ()
  "Find all `Media_Links:' in the buffer replacing them with the referenced image."
  (interactive)
  (goto-char (point-min))
  (let (line-details)
    (while (setq line-details (mastodon-media--select-next-media-line))
      (let* ((start (car line-details))
             (end (cadr line-details))
             (media-type (caddr line-details))
             (image-url (get-text-property start 'media-url)))
        (if (not (mastodon-media--valid-link-p image-url))
            ;; mark it at least as not needing loading any more
            (put-text-property start end 'media-state 'invalid-url)
          ;; proceed to load this image asynchronously
          (put-text-property start end 'media-state 'loading)
          (mastodon-media--load-image-from-url image-url media-type start (- end start)))))))

(provide 'mastodon-media)
;;; mastodon-media.el ends here
