;;; ghub-legacy.el --- Deprecated Ghub functions  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2025 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.ghub@jonas.bernoulli.dev>
;; Homepage: https://github.com/magit/ghub
;; Keywords: tools

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Deprecated Ghub functions.

;;; Code:

(require 'ghub-graphql)

(cl-defun ghub--graphql (graphql
                         &optional variables
                         &key username auth host forge
                         headers
                         callback errorback)
  (ghub--graphql-vacuum graphql variables callback nil
                        :username  username
                        :auth      auth
                        :host      host
                        :forge     forge
                        :headers   headers
                        :errorback errorback))

;;; _
(provide 'ghub-legacy)
;;; ghub-legacy.el ends here
