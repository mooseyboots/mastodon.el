;;; mastodon-discover.el --- Use Mastodon.el with discover.el  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Maintainer: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 0.10.0
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://git.blast.noho.st/mouse/mastodon.el

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

(declare-function discover-add-context-menu "discover")

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
                       ("A" "View profile of author" mastodon-profile--get-toot-author)
                       ("b" "Boost" mastodon-toot--boost)
                       ("f" "Favourite" mastodon-toot--favourite)
                       ("c" "Toggle hidden text (CW)" mastodon-tl--toggle-spoiler-text-in-toot)
                       ("n" "Next" mastodon-tl--goto-next-toot)
                       ("p" "Prev" mastodon-tl--goto-prev-toot)
                       ("TAB" "Next link item" mastodon-tl--next-tab-item)
                       ("S-TAB" "Prev link item" mastodon-tl--previous-tab-item)
                       ("t" "New toot" mastodon-toot)
                       ("r" "Reply" mastodon-toot--reply)
                       ("C" "Copy toot URL" mastodon-toot--copy-toot-url)
                       ("d" "Delete (your) toot" mastodon-toot--delete-toot)
                       ("D" "Delete and redraft (your) toot" mastodon-toot--delete-toot)
                       ("i" "Pin/Unpin (your) toot" mastodon-toot--pin-toot-toggle)
                       ("P" "View user profile" mastodon-profile--show-user)
                       ("T" "View thread" mastodon-tl--thread)
                       ("v" "Vote on poll" mastodon-tl--poll-vote))
                      ("Timelines"
                       ("h" "View mode help/keybindings" describe-mode)
                       ("#" "Tag search" mastodon-tl--get-tag-timeline)
                       ("F" "Federated" mastodon-tl--get-federated-timeline)
                       ("H" "Home" mastodon-tl--get-home-timeline)
                       ("L" "Local" mastodon-tl--get-local-timeline)
                       ("N" "Notifications" mastodon-notifications--get)
                       ("u" "Update timeline" mastodon-tl--update)
                       ("S" "Search" mastodon-search--search-query)
                       ("C-S-P" "Jump to your profile" mastodon-profile--my-profile)
                       ("K" "View bookmarks" mastodon-profile--view-bookmarks))
                      ("Users"
                       ("W" "Follow" mastodon-tl--follow-user)
                       ("C-S-W" "Unfollow" mastodon-tl--unfollow-user)
                       ("M" "Mute" mastodon-tl--mute-user)
                       ("C-S-M" "Unmute" mastodon-tl--unmute-user)
                       ("B" "Block" mastodon-tl--block-user)
                       ("C-S-B" "Unblock" mastodon-tl--unblock-user))
                      ("Images"
                       ("RET/i" "Load full image in browser" 'shr-browse-image)
                       ("r" "rotate" 'image-rotate)
                       ("+" "zoom in" 'image-increase-size)
                       ("-" "zoom out" 'image-decrease-size)
                       ("u" "copy URL" 'shr-maybe-probe-and-copy-url))
                      ("Profile view"
                       ("o" "Show following" mastodon-profile--open-following)
                       ("O" "Show followers" mastodon-profile--open-followers)
                       
                       ("R" "View follow requests" mastodon-profile--view-follow-requests)
                       ("a" "Accept follow request" mastodon-profile--follow-request-accept)
                       ("j" "Reject follow request" mastodon-profile--follow-request-reject)
                       ("U" "Update your profile note" mastodon-profile--update-user-profile-note))
                      ("Quit"
                       ("q" "Quit mastodon and bury buffer." kill-this-buffer)
                       ("Q" "Quit mastodon buffer and kill window." kill-buffer-and-window)))))))

(provide 'mastodon-discover)
;;; mastodon-discover.el ends here
