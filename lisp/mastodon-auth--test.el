;;; mastodon-auth--test.el --- Tests for mastodon-auth  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Version: 0.9.0
;; Homepage: https://github.com/jdenen/mastodon.el
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This file is part of mastodon.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; mastodon-auth--test.el provides ERT tests for mastodon-auth.el

;;; Code:

(require 'ert)

(ert-deftest mastodon-auth--handle-token-response--good ()
  (should (string= "foo" (mastodon-auth--handle-token-response '(:access_token "foo" :token_type "Bearer" :scope "read write follow" :created_at 0)))))

(ert-deftest mastodon-auth--handle-token-response--unknown ()
  :expected-result :failed
  (mastodon-auth--handle-token-response '(:herp "derp")))

(ert-deftest mastodon-auth--handle-token-response--failure ()
  :expected-result :failed
  (mastodon-auth--handle-token-response '(:error "invalid_grant" :error_description "The provided authorization grant is invalid, expired, revoked, does not match the redirection URI used in the authorization request, or was issued to another client.")))

(provide 'mastodon-auth--test)
;;; mastodon-auth--test.el ends here
