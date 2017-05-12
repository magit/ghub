;;; ghub.el --- minuscule client for the Github API  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/tarsius/ghub
;; Keywords: tools
;; Package-Requires: ((emacs "25"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a copy of the GPL see https://www.gnu.org/licenses/gpl.txt.

;;; Commentary:

;; A minuscule client for the Github API.

;; This library just provides the HTTP verbs.  Instead of wrapping
;; every resource, I recommend https://developer.github.com/v3.
;; Due to the lack of doc-strings, I also recommend having a quick
;; look at the source, which is quite trivial.

;; Initial configuration
;; ---------------------
;;
;;   $ git config github.user <username>
;;   $ emacs ~/.authinfo.gpg
;;   # -*- epa-file-encrypt-to: ("A.U.Thor@example.com") -*-
;;   machine api.github.com login <login> password <token>

;; Usage examples
;; --------------
;;
;; Getting details about a repository:
;;
;;   (ghub-get "/repos/tarsius/ghub")
;;
;; Listing names of all repositories of a user:
;;
;;   (--keep (cdr (assq 'name it))
;;           (let ((ghub-unpaginate t))
;;             (ghub-get "/users/tarsius/repos")))
;;
;; Making an unauthenticated request:
;;
;;   (let ((ghub-authenticate nil))
;;     (ghub-get "/orgs/magit/repos"))

;; Github Enterprise support
;; -------------------------
;;
;; Initial configuration:
;;
;;   $ git config example_com.user employee
;;   $ emacs ~/.authinfo.gpg
;;   # -*- epa-file-encrypt-to: ("employee@example.com") -*-
;;   machine example.com/api/v3 login employee password <token>
;;
;; Making a request:
;;
;;   (let ((ghub-base-url "https://example.com/api/v3"))
;;     (ghub-get "/users/employee/repos"))

;; Alternatives
;; ------------

;; If you like this, then you might also like `glab.el'; a minuscule
;; client for the Gitlab API.  See https://gitlab.com/tarsius/glab.

;; If you don't like this, then you might instead like `gh.el'; a big
;; client for the Github API.  See https://github.com/sigma/gh.el.

;; If you would like to use `ghub.el', but also want dedicated
;; functions for each API endpoint, then you can create those using
;; `apiwrap.el'.  See https://github.com/vermiculus/apiwrap.el.

;;; Code:

(require 'auth-source)
(require 'json)
(require 'subr-x)
(require 'url)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defvar ghub-base-url "https://api.github.com")
(defvar ghub-authenticate t)
(defvar ghub-token nil)
(defvar ghub-username nil)
(defvar ghub-unpaginate nil)

(defun ghub-get (resource &optional params data noerror)
  (ghub--request "GET" resource params data noerror))

(defun ghub-put (resource &optional params data noerror)
  (ghub--request "PUT" resource params data noerror))

(defun ghub-head (resource &optional params data noerror)
  (ghub--request "HEAD" resource params data noerror))

(defun ghub-post (resource &optional params data noerror)
  (ghub--request "POST" resource params data noerror))

(defun ghub-patch (resource &optional params data noerror)
  (ghub--request "PATCH" resource params data noerror))

(defun ghub-delete (resource &optional params data noerror)
  (ghub--request "DELETE" resource params data noerror))

(define-error 'ghub-error "Ghub Error")
(define-error 'ghub-auth-error "Auth Error" 'ghub-error)
(define-error 'ghub-http-error "HTTP Error" 'ghub-error)
(define-error 'ghub-301 "Moved Permanently" 'ghub-http-error)
(define-error 'ghub-400 "Bad Request" 'ghub-http-error)
(define-error 'ghub-404 "Not Found" 'ghub-http-error)
(define-error 'ghub-422 "Unprocessable Entity" 'ghub-http-error)

(defun ghub--request (method resource &optional params data noerror)
  (let* ((p (and params (concat "?" (ghub--url-encode-params params))))
         (d (and data   (json-encode-list data)))
         (url-request-extra-headers
          `(("Content-Type"  . "application/json")
            ,@(and ghub-authenticate
                   `(("Authorization" . ,(concat "token " (ghub--token)))))))
         (url-request-method method)
         (url-request-data d))
    (with-current-buffer
        (url-retrieve-synchronously (concat ghub-base-url resource p))
      (set-buffer-multibyte t)
      (let (link body)
        (goto-char (point-min))
        (save-restriction
          (narrow-to-region (point) url-http-end-of-headers)
          (and (setq link (mail-fetch-field "Link"))
               (setq link (car (rassoc (list "rel=\"next\"")
                                       (mapcar (lambda (elt) (split-string elt "; "))
                                               (split-string link ",")))))
               (string-match "[?&]page=\\([^&>]+\\)" link)
               (setq link (match-string 1 link))))
        (goto-char (1+ url-http-end-of-headers))
        (setq body (ghub--read-response))
        (unless (or noerror (= (/ url-http-response-status 100) 2))
          (pcase url-http-response-status
            (301 (signal 'ghub-301 (list method resource p d body)))
            (400 (signal 'ghub-400 (list method resource p d body)))
            (404 (signal 'ghub-404 (list method resource p d body)))
            (422 (signal 'ghub-422 (list method resource p d body)))
            (_   (signal 'ghub-http-error
                         (list url-http-response-status
                               method resource p d body)))))
        (if (and link ghub-unpaginate)
            (nconc body
                   (ghub--request method resource
                                  (cons (cons 'page link)
                                        (cl-delete 'page params :key #'car))
                                  data noerror))
          body)))))

(defun ghub--read-response ()
  (unless (eobp)
    (let ((json-object-type 'alist)
          (json-array-type  'list)
          (json-key-type    'symbol)
          (json-false       nil)
          (json-null        nil))
      (json-read))))

(defun ghub--url-encode-params (params)
  (mapconcat (pcase-lambda (`(,key . ,val))
               (concat (url-hexify-string (symbol-name key)) "="
                       (url-hexify-string val)))
             params "&"))

(defun ghub--token ()
  (or ghub-token
      (let ((secret
             (plist-get
              (car (auth-source-search
                    :max 1
                    :user (ghub--username)
                    :host (save-match-data
                            (string-match "\\`https?://" ghub-base-url)
                            (substring ghub-base-url (match-end 0)))))
              :secret)))
        (or (if (functionp secret)
                (funcall secret)
              secret)
            (signal 'ghub-auth-error "Token not found")))))

(defun ghub--username ()
  (or ghub-username
      (substring
       (shell-command-to-string
        (format "git config %s.user"
                (if (string-equal ghub-base-url "https://api.github.com")
                    "github"
                  (save-match-data
                    (and (string-match "\\`https?://\\([^/]+\\)" ghub-base-url)
                         (replace-regexp-in-string
                          "\\." "_" (match-string 1 ghub-base-url)))))))
       0 -1)))

(defun ghub-wait (resource)
  (with-local-quit
    (let ((for 0.5)
          (total 0))
      (while (not (ignore-errors (ghub-get resource)))
        (setq for (truncate (* 2 for)))
        (setq total (+ total for))
        (when (= for 128)
          (signal 'ghub-error
                  (format "Github is taking to long to create %s" resource)))
        (message "Waiting for %s (%ss)..." resource total)
        (sit-for for)))))

;;; ghub.el ends soon
(provide 'ghub)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ghub.el ends here
