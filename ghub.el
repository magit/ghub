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

;; Initial configuration
;; ---------------------
;;
;;   $ git config --global github.user <username>
;;   $ emacs ~/.authinfo.gpg
;;   # -*- epa-file-encrypt-to: ("A.U.Thor@example.com") -*-
;;   machine api.github.com login <login> password <token>
;;
;; To acquire a token, go to https://github.com/settings/tokens.  Note
;; that currently the same token is shared by all Emacs packages that
;; use `ghub.el'.

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
;;
;; Making a request using basic authentication:
;;
;;   (let ((ghub-authenticate 'basic))
;;     (ghub-get "/orgs/magit/repos"))

;; Github Enterprise support
;; -------------------------
;;
;; Initial configuration:
;;
;;   $ cd /path/to/repository
;;   $ git config gh_example_com.user employee
;;   $ emacs ~/.authinfo.gpg
;;   # -*- epa-file-encrypt-to: ("employee@example.com") -*-
;;   machine gh.example.com login employee password <token>
;;
;; Note that unlike for Github.com, which uses `github.user', the Git
;; variable used to store the username for an Enterprise instance is
;; named `HOST.user', where HOST is the host part of the `URI', with
;; dots replaced with underscores.
;;
;; Making a request:
;;
;;   (let ((ghub-base-url "https://gh.example.com/api/v3"))
;;     (ghub-get "/users/employee/repos"))

;; Alternatives
;; ------------

;; If you like this, then you might never-the-less prefer `ghub+.el';
;; a thick GitHub API client built on `ghub.el'.
;; See https://github.com/vermiculus/ghub-plus.

;; If you like this, then you might also like `glab.el'; a minuscule
;; client for the Gitlab API.  See https://gitlab.com/tarsius/glab.

;; If you don't like this, then you might instead like `gh.el'; a big
;; client for the Github API.  See https://github.com/sigma/gh.el.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-auth)

(eval-when-compile (require 'subr-x))

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defvar ghub-base-url "https://api.github.com")
(defvar ghub-authenticate t)
(defvar ghub-token nil)
(defvar ghub-username nil)
(defvar ghub-unpaginate nil)

(defun ghub-get (resource &optional params data noerror)
  "Make `GET' request for RESOURCE, optionally sending PARAMS and/or DATA.
Signal an error if the status code isn't in the 2xx class;
unless optional NOERROR is non-nil, in which case return nil."
  (ghub--request "GET" resource params data noerror))

(defun ghub-put (resource &optional params data noerror)
  "Make `PUT' request for RESOURCE, optionally sending PARAMS and/or DATA.
Signal an error if the status code isn't in the 2xx class;
unless optional NOERROR is non-nil, in which case return nil."
  (ghub--request "PUT" resource params data noerror))

(defun ghub-head (resource &optional params data noerror)
  "Make `HEAD' request for RESOURCE, optionally sending PARAMS and/or DATA.
Signal an error if the status code isn't in the 2xx class;
unless optional NOERROR is non-nil, in which case return nil."
  (ghub--request "HEAD" resource params data noerror))

(defun ghub-post (resource &optional params data noerror)
  "Make `POST' request for RESOURCE, optionally sending PARAMS and/or DATA.
Signal an error if the status code isn't in the 2xx class;
unless optional NOERROR is non-nil, in which case return nil."
  (ghub--request "POST" resource params data noerror))

(defun ghub-patch (resource &optional params data noerror)
  "Make `PATCH' request for RESOURCE, optionally sending PARAMS and/or DATA.
Signal an error if the status code isn't in the 2xx class;
unless optional NOERROR is non-nil, in which case return nil."
  (ghub--request "PATCH" resource params data noerror))

(defun ghub-delete (resource &optional params data noerror)
  "Make `DELETE' request for RESOURCE, optionally sending PARAMS and/or DATA.
Signal an error if the status code isn't in the 2xx class; unless
optional NOERROR is non-nil, in which case return nil."
  (ghub--request "DELETE" resource params data noerror))

(define-error 'ghub-error "Ghub Error")
(define-error 'ghub-auth-error "Auth Error" 'ghub-error)
(define-error 'ghub-http-error "HTTP Error" 'ghub-error)
(define-error 'ghub-301 "Moved Permanently" 'ghub-http-error)
(define-error 'ghub-400 "Bad Request" 'ghub-http-error)
(define-error 'ghub-404 "Not Found" 'ghub-http-error)
(define-error 'ghub-422 "Unprocessable Entity" 'ghub-http-error)

(defun ghub--request (method resource &optional params data noerror)
  "Make a request using METHOD for RESOURCE.
METHOD is a `HTTP' request method, a string.  If non-nil, send
PARAMS and/or DATA in the request.  Signal an error if the status
code isn't in the 2xx class; unless optional NOERROR is non-nil,
in which case return nil."
  (let* ((p (and params (concat "?" (ghub--url-encode-params params))))
         (d (and data   (json-encode-list data)))
         (url-request-extra-headers
          `(("Content-Type"  . "application/json")
            ,@(and ghub-authenticate
                   `(("Authorization"
                      . ,(if (eq ghub-authenticate 'basic)
                             (ghub--basic-auth)
                           (concat "token " (ghub--token))))))))
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

(defun ghub--basic-auth ()
  (let ((url (url-generic-parse-url ghub-base-url)))
    (setf (url-user url)
          (ghub--username))
    (url-basic-auth url t)))

(defun ghub--hostname ()
  (save-match-data
    (if (string-match "\\`https?://\\([^/]+\\)" ghub-base-url)
        (match-string 1 ghub-base-url)
      (signal 'ghub-auth-error '("Invalid value for ghub-base-url")))))

(defun ghub--token ()
  "Return the configured token.
Use `auth-source-search' to get the token for the user returned
by `ghub--username' and a host based on `ghub-base-url'.  When
`ghub-token' is non-nil, then return its value instead."
  (or ghub-token
      (let ((secret (plist-get (car (auth-source-search
                                     :max 1
                                     :user (ghub--username)
                                     :host (ghub--hostname)))
                               :secret)))
        (or (if (functionp secret)
                (funcall secret)
              secret)
            (signal 'ghub-auth-error '("Token not found"))))))

(defun ghub--username ()
  "Return the configured username.
For Github.com get the value of the Git variable `github.user'.
For Github enterprise instances, get the value of `HOST.user',
where HOST is the host part of the `URI', with dots replaced with
underscores.  E.g. `gh_example_com.user' for gh.example.com/api."
  (or ghub-username
      (substring
       (shell-command-to-string
        (format "git config %s.user"
                (if (string-equal ghub-base-url "https://api.github.com")
                    "github"
                  (subst-char-in-string ?. ?_ (ghub--hostname)))))
       0 -1)))

(defun ghub-wait (resource)
  "Busy-wait until RESOURCE becomes available."
  (with-local-quit
    (let ((for 0.5)
          (total 0))
      (while (not (ignore-errors (ghub-get resource)))
        (setq for (truncate (* 2 for)))
        (setq total (+ total for))
        (when (= for 128)
          (signal 'ghub-error
                  (list (format "Github is taking too long to create %s"
                                resource))))
        (message "Waiting for %s (%ss)..." resource total)
        (sit-for for)))))

;;; ghub.el ends soon
(provide 'ghub)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ghub.el ends here
