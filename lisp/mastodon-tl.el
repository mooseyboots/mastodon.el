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

;; mastodon-tl.el provides timeline functions.

;;; Code:

(require 'mastodon-http)

(defgroup mastodon-tl nil
  "Timelines in Mastodon."
  :prefix "mastodon-tl-"
  :group 'mastodon)

(defun mastodon-tl--get-federated-timeline ()
  "Opens federated timeline."
  (interactive)
  (mastodon-tl--get "public"))

(defun mastodon-tl--get-home-timeline ()
  "Opens home timeline."
  (interactive)
  (mastodon-tl--get "home"))

(defun mastodon-tl--get-local-timeline ()
  "Opens local timeline."
  (interactive)
  (mastodon-tl--get "public?local=true"))

(defun mastodon-tl--get-tag-timeline ()
  "Prompts for tag and opens its timeline."
  (interactive)
  (let ((tag (read-string "Tag: ")))
    (mastodon-tl--get (concat "tag/" tag))))

(defun mastodon-tl--goto-toot-pos (find-pos &optional pos)
  "Search for toot with FIND-POS. Optionally stat from POS."
  (let* ((npos (funcall find-pos
                        (or pos (point))
                        'toot-id
                        (current-buffer))))
    (when npos (if (not (get-text-property npos 'toot-id))
                   (mastodon-tl--goto-toot-pos find-pos npos)
                 (when npos (goto-char npos))))))

(defun mastodon-tl--goto-next-toot ()
  "Jump to next toot header."
  (interactive)
  (mastodon-tl--goto-toot-pos 'next-single-property-change))

(defun mastodon-tl--goto-prev-toot ()
  "Jump to last toot header."
  (interactive)
  (mastodon-tl--goto-toot-pos 'previous-single-property-change))

(defun mastodon-tl--timeline-name ()
  "Determine timeline from `buffer-name'."
  (replace-regexp-in-string "\*" ""
                            (replace-regexp-in-string "mastodon-" "" (buffer-name))))

(defun mastodon-tl--remove-html (toot)
  (let* ((t1 (replace-regexp-in-string "<\/p>" "\n\n" toot))
         (t2 (replace-regexp-in-string "<\/?span>" "" t1)))
    (replace-regexp-in-string "<span class=\"h-card\">" "" t2)))

(defun mastodon-tl--byline-author (toot)
  "Propertize author of TOOT."
  (let* ((account (cdr (assoc 'account toot)))
         (handle (cdr (assoc 'acct account)))
         (name (cdr (assoc 'display_name account))))
    (concat
     (propertize name 'face 'warning)
     " (@"
     handle
     ")")))

(defun mastodon-tl--byline-boosted (toot)
  "Add byline for boosted data from TOOT."
  (let ((reblog (cdr (assoc 'reblog toot))))
    (when reblog
      (concat
       " "
       (propertize "Boosted" 'face 'highlight)
       " "
       (mastodon-tl--byline-author reblog)))))

(defun mastodon-tl--byline (toot)
  (let ((id (cdr (assoc 'id toot))))
    (propertize
     (concat (propertize "\n | " 'face 'default)
             (mastodon-tl--byline-author toot)
             (mastodon-tl--byline-boosted toot)
             (propertize "\n  ------------" 'face 'default))
     'toot-id id)))

(defun mastodon-tl--content (toot)
  (let* ((reblog (cdr (assoc 'reblog toot)))
         (content (if reblog
                      (cdr (assoc 'content reblog))
                    (cdr (assoc 'content toot)))))
    (propertize (mastodon-tl--remove-html content)
                'face 'default)))

(defun mastodon-tl--toot (toot)
  (insert
   (concat
    (mastodon-tl--content toot)
    (mastodon-tl--byline toot)
    "\n\n")))

(defun mastodon-tl--timeline (toots)
  (mapcar 'mastodon-tl--toot toots)
  (html2text)
  (replace-regexp "\n\n\n" "\n" nil (point-min) (point-max)))

;; TODO
;; Look into the JSON returned here by Local
(defun mastodon-tl--updated-json (timeline id)
  "Return JSON for TIMELINE since ID."
  (let ((url (mastodon--api-for (concat "timelines/"
                                        timeline
                                        "?since_id="
                                        (number-to-string id)))))
    (mastodon-http--get-json url)))

(defun mastodon-tl--newest-id ()
  "Return toot-id from the top of the buffer."
  (goto-char (point-min))
  (mastodon-tl--goto-next-toot)
  (get-text-property (point) 'toot-id))

(defun mastodon-tl--update ()
  "Update timeline with new toots."
  (interactive)
  (let* ((tl (mastodon-tl--timeline-name))
         (id (mastodon-tl--newest-id))
         (json (mastodon-tl--updated-json tl id)))
    (when json
      (with-current-buffer (current-buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (mastodon-tl--timeline json))))))

(defun mastodon-tl--get (timeline)
  "Display TIMELINE in buffer."
  (let* ((url (mastodon--api-for (concat "timelines/" timeline)))
         (buffer (concat "*mastodon-" timeline "*"))
         (json (mastodon-http--get-json url)))
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      (mastodon-tl--timeline json))
    (mastodon-mode)))

(provide 'mastodon-tl)
;;; mastodon-tl.el ends here
