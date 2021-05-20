;;; mastodon-discover.el --- Use Mastodon.el with discover.el  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.9.0
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

;; This adds optional functionality that can be used if the dicover package
;; is present.
;;
;; See the README file for how to use this.

;;; Code:

(defun mastodon-discover ()
  "Plug Mastodon functionality into `discover'."
  (interactive)
  (when (require 'discover nil :noerror)
    (discover-add-context-menu
     :bind "?"
     :mode 'mastodon-mode
     :mode-hook 'mastodon-mode-hook
     :context-menu '(mastodon
                     (description "Mastodon feed viewer")
                     (actions
                      ("Toots"
                       ("A" "Author" mastodon-profile--get-toot-author)
                       ("b" "Boost" mastodon-toot--boost)
                       ("c" "Toggle content" mastodon-tl--toggle-spoiler-text-in-toot)
                       ("f" "Favourite" mastodon-toot--favourite)
                       ("n" "Next" mastodon-tl--goto-next-toot)
                       ("p" "Prev" mastodon-tl--goto-prev-toot)
                       ("TAB" "Next link item" mastodon-tl--next-tab-item)
                       ("S-TAB" "Prev link item" mastodon-tl--previous-tab-item)
                       ("t" "New toot" mastodon-toot)
                       ("r" "Reply" mastodon-toot--reply)
                       ("u" "Update" mastodon-tl--update)
                       ("P" "Users" mastodon-profile--show-user)
                       ("T" "Thread" mastodon-tl--thread))
                      ("Timelines"
                       ("#" "Tag" mastodon-tl--get-tag-timeline)
                       ("F" "Federated" mastodon-tl--get-federated-timeline)
                       ("H" "Home" mastodon-tl--get-home-timeline)
                       ("L" "Local" mastodon-tl--get-local-timeline)
                       ("N" "Notifications" mastodon-notifications--get))
                      ("Images"
                       ("RET/i" "Load full image in browser" 'shr-browse-image)
                       ("r" "rotate" 'image-rotate)
                       ("+" "zoom in" 'image-increase-size)
                       ("-" "zoom out" 'image-decrease-size)
                       ("u" "copy URL" 'shr-maybe-probe-and-copy-url))
                      ("Profile view"
                       ("o" "Show following" mastodon-profile--open-following)
                       ("O" "Show followers" mastodon-profile--open-followers))
                      ("Quit"
                       ("q" "Quit mastodon buffer. Leave window open." kill-this-buffer)
                       ("Q" "Quit mastodon buffer and kill window." kill-buffer-and-window)))))))

(provide 'mastodon-discover)
;;; mastodon-discover.el ends here
