;;; mastodon-media.el --- Functions for inlining Mastodon media  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.9.0
;; Homepage: https://github.com/jdenen/mastodon.el
;; Package-Requires: ((emacs "24.4"))

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
(defvar url-show-status)

(defgroup mastodon-media nil
  "Inline Mastadon media."
  :prefix "mastodon-media-"
  :group 'mastodon)

(defcustom mastodon-media--avatar-height 30
  "Height of the user avatar images (if shown)."
  :group 'mastodon-media
  :type 'integer)

(defcustom mastodon-media--preview-max-height 250
  "Max height of any media attachment preview to be shown."
  :group 'mastodon-media
  :type 'integer)

(defvar mastodon-media--generic-avatar-data
  (base64-decode-string
   "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")
  "The PNG data for a generic 100x100 avatar")

(defvar mastodon-media--generic-broken-image-data
  (base64-decode-string
   "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")
  "The PNG data for a generic 200x200 'broken image' view")

(defun mastodon-media--process-image-response
    (status-plist marker image-options region-length)
  "Callback function processing the url retrieve response for URL.

STATUS-PLIST is the usual plist of status events as per `url-retrieve'.
IMAGE-OPTIONS are the precomputed options to apply to the image.
MARKER is the marker to where the response should be visible.
REGION-LENGTH is the length of the region that should be replaced with the image.
"
  (when (marker-buffer marker) ; only if the buffer hasn't been kill in the meantime
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
            (with-current-buffer (marker-buffer marker)
              ;; Save narrowing in our buffer
              (let ((inhibit-read-only t))
                (save-restriction
                  (widen)
                  (put-text-property marker
                                     (+ marker region-length) 'media-state 'loaded)
                  (when image
                    ;; We only set the image to display if we could load
                    ;; it; we already have set a default image when we
                    ;; added the tag.
                    (put-text-property marker (+ marker region-length)
                                       'display image))
                  ;; We are done with the marker; release it:
                  (set-marker marker nil)))
              (kill-buffer url-buffer)))))))

(defun mastodon-media--load-image-from-url (url media-type start region-length)
  "Takes a URL and MEDIA-TYPE and load the image asynchronously.

MEDIA-TYPE is a symbol and either 'avatar or 'media-link."
  ;; TODO: Cache the avatars
  (let ((image-options (when (image-type-available-p 'imagemagick)
                         (cond
                          ((eq media-type 'avatar)
                           `(:height ,mastodon-media--avatar-height))
                          ((eq media-type 'media-link)
                           `(:max-height ,mastodon-media--preview-max-height))))))
    (let ((buffer (current-buffer))
          (marker (copy-marker start))
	  ;; Keep url.el from spamming us with messages about connecting to hosts:
	  (url-show-status nil))
      (condition-case nil
          ;; catch any errors in url-retrieve so as to not abort
          ;; whatever called us
          (url-retrieve url
                        #'mastodon-media--process-image-response
                        (list marker image-options region-length))
        (error (with-current-buffer buffer
                 ;; TODO: Consider adding retries
                 (put-text-property marker
                                    (+ marker region-length)
                                    'media-state
                                    'loading-failed)
                 :loading-failed))))))

(defun mastodon-media--select-next-media-line (end-pos)
  "Find coordinates of the next media to load before END-POS.

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
    (when (and next-pos (< next-pos end-pos))
      (let ((media-type (get-text-property next-pos 'media-type)))
        (cond
         ;; Avatars are just one character in the buffer
         ((eq media-type 'avatar)
          (list next-pos (+ next-pos 1) 'avatar))
        ;; Media links are 5 character ("[img]")
         ((eq media-type 'media-link)
          (list next-pos (+ next-pos 5) 'media-link)))))))

(defun mastodon-media--valid-link-p (link)
  "Checks to make sure that the missing string has

not been returned."
  (and link
       (> (length link) 8)
       (or (string= "http://" (substring link 0 7))
           (string= "https://" (substring link 0 8)))))

(defun mastodon-media--inline-images (search-start search-end)
  "Find all `Media_Links:' in the range from SEARCH-START to SEARCH-END
replacing them with the referenced image."
  (save-excursion
    (goto-char search-start)
    (let (line-details)
      (while (setq line-details (mastodon-media--select-next-media-line
                                 search-end))
        (let* ((start (car line-details))
               (end (cadr line-details))
               (media-type (cadr (cdr line-details)))
               (image-url (get-text-property start 'media-url)))
          (if (not (mastodon-media--valid-link-p image-url))
              ;; mark it at least as not needing loading any more
              (put-text-property start end 'media-state 'invalid-url)
            ;; proceed to load this image asynchronously
            (put-text-property start end 'media-state 'loading)
            (mastodon-media--load-image-from-url
             image-url media-type start (- end start))))))))

(defun mastodon-media--get-avatar-rendering (avatar-url)
  "Returns the string to be written that renders the avatar at AVATAR-URL."
  ;; We use just an empty space as the textual representation.
  ;; This is what a user will see on a non-graphical display
  ;; where not showing an avatar at all is preferable.
  (let ((image-options (when (image-type-available-p 'imagemagick)
                         `(:height ,mastodon-media--avatar-height))))
    (concat
     (propertize " "
                 'media-url avatar-url
                 'media-state 'needs-loading
                 'media-type 'avatar
                 'display (apply #'create-image mastodon-media--generic-avatar-data
                                 (when image-options 'imagemagick)
                                 t image-options))
     " ")))

(defun mastodon-media--get-media-link-rendering (media-url &optional full-remote-url)
  "Returns the string to be written that renders the image at MEDIA-URL."
  (concat
   (propertize "[img]"
               'media-url media-url
               'media-state 'needs-loading
               'media-type 'media-link
               'display (create-image mastodon-media--generic-broken-image-data nil t)
               'mouse-face 'highlight
               'mastodon-tab-stop 'image ; for do-link-action-at-point
               'image-url full-remote-url ; for shr-browse-image
               'keymap mastodon-tl--shr-image-map-replacement
               'help-echo (concat "RET/i: load full image (prefix: copy URL), +/-: zoom, r: rotate, o: save preview")
               )
   " "))

(provide 'mastodon-media)
;;; mastodon-media.el ends here
