;;; ghub.el --- minuscule client library for the Github API  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/ghub
;; Keywords: tools
;; Package-Requires: ((emacs "24.4") (let-alist "1.0.5"))

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

;; Ghub is a library that provides basic support for using the Github API
;; from Emacs packages.  It abstracts access to API resources using only
;; a handful of functions that are not resource-specific.

;; Ghub handles the creation, storage and use of access tokens using a
;; setup wizard to make it easier for users to get started and to reduce
;; the support burden imposed on package maintainers.  It also comes with
;; a comprehensive manual to address the cases when things don't just
;; work as expected or in case you don't want to use the wizard.

;; Ghub is intentionally limited to only provide these two essential
;; features — basic request functions and guided setup — to avoid being
;; too opinionated, which would hinder wide adoption.  It is assumed that
;; wide adoption would make life easier for users and maintainers alike,
;; because then all packages that talk to the Github API could be
;; configured the same way.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'let-alist)
(require 'url)
(require 'url-auth)
(require 'url-http)

(eval-when-compile (require 'subr-x))

(defvar url-callback-arguments)
(defvar url-http-end-of-headers)
(defvar url-http-response-status)

;;; Settings

(defconst ghub-default-host "api.github.com")

(defvar ghub-github-token-scopes '(repo)
  "The Github API scopes that your private tools need.

The token that is created based on the value of this variable
is used when `ghub-request' (or one of its wrappers) is called
without providing a value for AUTH.  Packages should always
identify themselves using that argument, but when you use Ghub
directly in private tools, then that is not necessary and the
request is made on behalf of the `ghub' package itself, aka on
behalf of some private tool.

By default the only requested scope is `repo' because that is
sufficient as well as required for most common uses.  This and
other scopes are documented at https://magit.vc/goto/2e586d36.

If your private tools need other scopes, then you have to add
them here *before* creating the token.  Alternatively you can
edit the scopes of an existing token using the web interface
at https://github.com/settings/tokens.")

(defvar ghub-override-system-name nil
  "If non-nil, the string used to identify the local machine.
If this is nil, then the value returned by `system-name' is
used instead.")

;;; Request
;;;; API

(define-error 'ghub-error "Ghub/Url Error" 'error)
(define-error 'ghub-http-error "HTTP Error" 'ghub-error)

(defvar ghub-response-headers nil
  "The headers returned in response to the last request.
`ghub-request' returns the response body and stores the
response header in this variable.")

(cl-defun ghub-graphql (graphql &optional variables
                                &key username auth host
                                callback errorback)
  "Make a GraphQL request using GRAPHQL and VARIABLES.
Return the response as a json-like alist.  Even if the response
contains `errors', do not raise an error.  GRAPHQL is a GraphQL
string.  VARIABLES is a json-like alist.  The other arguments
behave like for `ghub-request' (which see)."
  (cl-assert (stringp graphql))
  (cl-assert (not (stringp variables)))
  (ghub-request "POST" "/graphql" nil :payload
                (json-encode `(("query" . ,graphql)
                               ,@(and variables `(("variables" ,@variables)))))
                :username username :auth auth :host host
                :callback callback :errorback errorback))

(cl-defun ghub-head (resource &optional params
                              &key query payload headers
                              unpaginate noerror reader
                              username auth host
                             callback errorback)
  "Make a `HEAD' request for RESOURCE, with optional query PARAMS.
Like calling `ghub-request' (which see) with \"HEAD\" as METHOD."
  (ghub-request "HEAD" resource params
                :query query :payload payload :headers headers
                :unpaginate unpaginate :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback))

(cl-defun ghub-get (resource &optional params
                             &key query payload headers
                             unpaginate noerror reader
                             username auth host
                             callback errorback)
  "Make a `GET' request for RESOURCE, with optional query PARAMS.
Like calling `ghub-request' (which see) with \"GET\" as METHOD."
  (ghub-request "GET" resource params
                :query query :payload payload :headers headers
                :unpaginate unpaginate :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback))

(cl-defun ghub-put (resource &optional params
                             &key query payload headers
                             unpaginate noerror reader
                             username auth host
                             callback errorback)
  "Make a `PUT' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"PUT\" as METHOD."
  (ghub-request "PUT" resource params
                :query query :payload payload :headers headers
                :unpaginate unpaginate :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback))

(cl-defun ghub-post (resource &optional params
                              &key query payload headers
                              unpaginate noerror reader
                              username auth host
                              callback errorback)
  "Make a `POST' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"POST\" as METHOD."
  (ghub-request "POST" resource params
                :query query :payload payload :headers headers
                :unpaginate unpaginate :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback))

(cl-defun ghub-patch (resource &optional params
                               &key query payload headers
                               unpaginate noerror reader
                               username auth host
                               callback errorback)
  "Make a `PATCH' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"PATCH\" as METHOD."
  (ghub-request "PATCH" resource params
                :query query :payload payload :headers headers
                :unpaginate unpaginate :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback))

(cl-defun ghub-delete (resource &optional params
                                &key query payload headers
                                unpaginate noerror reader
                                username auth host
                                callback errorback)
  "Make a `DELETE' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"DELETE\" as METHOD."
  (ghub-request "DELETE" resource params
                :query query :payload payload :headers headers
                :unpaginate unpaginate :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback))

(cl-defun ghub-request (method resource &optional params
                               &key query payload headers
                               unpaginate noerror reader
                               username auth host forge
                               callback errorback
                               url value error extra
                               ((:method method*)))
  "Make a request for RESOURCE and return the response body.

Also place the response header in `ghub-response-headers'.

METHOD is the http method, given as a string.
RESOURCE is the resource to access, given as a string beginning
  with a slash.

PARAMS, QUERY, PAYLOAD and HEADERS are alists used to specify
  data.  The Github API documentation is vague on how data has
  to be transmitted and for a particular resource usually just
  talks about \"parameters\".  Generally speaking when the METHOD
  is \"HEAD\" or \"GET\", then they have to be transmitted as a
  query, otherwise as a payload.
Use PARAMS to automatically transmit like QUERY or PAYLOAD would
  depending on METHOD.
Use QUERY to explicitly transmit data as a query.
Use PAYLOAD to explicitly transmit data as a payload.
  Instead of an alist, PAYLOAD may also be a string, in which
  case it gets encoded as UTF-8 but is otherwise transmitted as-is.
Use HEADERS for those rare resources that require that the data
  is transmitted as headers instead of as a query or payload.
  When that is the case, then the API documentation usually
  mentions it explicitly.

If UNPAGINATE is t, then make as many requests as necessary to
  get all values.  If UNPAGINATE is a natural number, then get
  at most that many pages.  For any other non-nil value raise
  an error.
If NOERROR is non-nil, then do not raise an error if the request
  fails and return nil instead.  If NOERROR is `return', then
  return the error payload instead of nil.
If READER is non-nil, then it is used to read and return from the
  response buffer.  The default is `ghub--read-json-payload'.
  For the very few resources that do not return json, you might
  want to use `ghub--decode-payload'.

If USERNAME is non-nil, then make a request on behalf of that
  user.  It is better to specify the user using the Git variable
  `github.user' for \"api.github.com\", or `github.HOST.user' if
  connecting to a Github Enterprise instance.

Each package that uses `ghub' should use its own token. If AUTH
  is nil, then the generic `ghub' token is used instead.  This
  is only acceptable for personal utilities.  A packages that
  is distributed to other users should always use this argument
  to identify itself, using a symbol matching its name.

  Package authors who find this inconvenient should write a
  wrapper around this function and possibly for the method
  specific functions also.

  Some symbols have a special meaning.  `none' means to make an
  unauthorized request.  `basic' means to make a password based
  request.  If the value is a string, then it is assumed to be
  a valid token or two-factor authentication code.  `basic' and
  strings are only intended for internal and debugging uses.

  If AUTH is a package symbol, then the scopes are specified
  using the variable `AUTH-github-token-scopes'.  It is an error
  if that is not specified.  See `ghub-github-token-scopes' for
  an example.

If HOST is non-nil, then connect to that Github instance.  This
  defaults to \"api.github.com\".  When a repository is connected
  to a Github Enterprise instance, then it is better to specify
  that using the Git variable `github.host' instead of using this
  argument.

If FORGE is `gitlab', then connect to Gitlab.com or, depending
  on HOST to another Gitlab instance.  This is only intended for
  internal use.  Instead of using this argument you should use
  function `glab-request' and other `glab-*' functions.

If CALLBACK and/or ERRORBACK is non-nil, then make one or more
  asynchronous requests and call CALLBACK or ERRORBACK when
  finished.  If an error occurred, then call ERRORBACK, or if
  that is nil, then CALLBACK.  When no error occurred then call
  CALLBACK.  When making asynchronous requests, then no errors
  are signaled, regardless of the value of NOERROR.

Both callbacks are called with four arguments.
  1. For CALLBACK, the combined value of the retrieved pages.
     For ERRORBACk, the error that occured when retrieving the
     last page.
  2. The headers of the last page as an alist.
  3. Status information provided by `url-retrieve'. Its `:error'
     property holds the same information as ERRORBACK's first
     argument.
  4. A plist containing arguments that have to be passed to
     `ghub-continue' (which see) to retrieve the next page.

The remaining arguments are intended for internal use only.  They
are provided by `ghub-continue' (which see) to fetch another page."
  (cl-assert (or (booleanp unpaginate) (natnump unpaginate)))
  (if url
      (setq method method*)
    ;; #35: Encode in case caller used (symbol-name 'GET).
    (setq method (encode-coding-string method 'utf-8))
    (unless (string-prefix-p "/" resource)
      (setq resource (concat "/" resource)))
    (unless host
      (setq host (ghub--host forge)))
    (unless (or username (stringp auth) (eq auth 'none))
      (setq username (ghub--username host forge)))
    (cond ((not params))
          ((member method '("GET" "HEAD"))
           (when query
             (error "PARAMS and QUERY are mutually exclusive for METHOD %S"
                    method))
           (setq query params))
          (t
           (when payload
             (error "PARAMS and PAYLOAD are mutually exclusive for METHOD %S"
                    method))
           (setq payload params)))
    (when payload
      (unless (stringp payload)
        (setq payload (json-encode-list payload)))
      (setq payload (encode-coding-string payload 'utf-8)))
    (when (or callback errorback)
      (setq noerror t))
    (setq url
          (concat "https://" host resource
                  (and query (concat "?" (ghub--url-encode-params query))))))
  (let ((url-request-extra-headers
         `(("Content-Type" . "application/json")
           ,@(and (not (eq auth 'none))
                  (ghub--auth host auth username forge))
           ,@headers))
        (url-request-method method)
        (url-request-data payload)
        (args (list :method     method
                    :headers    headers
                    :unpaginate unpaginate
                    :noerror    noerror
                    :reader     reader
                    :username   username
                    :auth       auth
                    :host       host
                    :forge      forge
                    :callback   callback
                    :errorback  errorback
                    :url        url
                    :value      value
                    :error      error
                    :extra      extra)))
    (if (or callback errorback)
        (url-retrieve url 'ghub--handle-response (list args))
      (with-current-buffer (url-retrieve-synchronously url)
        (ghub--handle-response (car url-callback-arguments) args)))))

(defun ghub-continue (args)
  "If there is a next page, then retrieve that.

This function is only intended to be called from callbacks.
If there is a next page, then retrieve that and return the
buffer that the result will be loaded into, or t if the
process has already completed.  Otherwise return nil.

Callbacks are called with four arguments (see `ghub-request').
The forth argument is a plist and its value should be used as
the argument to this function.  The plist contains most but
not all of the keyword arguments of `ghub-request' and a few
additional properties.

`:url' is the URL for the next page and is retrieved from the
  previous response header.  It replaces the optional RESOURCE,
  PARAMS and QUERY arguments.  This keyword argument is more
  important than the others in that it signals to `ghub-request'
  that it is being called recursively, in which case it ignores
  some of its the other arguments, using the ones describe here
  instead.
`:method' replaces `ghub-request's mandatory METHOD argument.
`:value' contains the value that was collected so far.
`:extra' can be used to pass additional information from one
  callback to its next incarnation.  This is the only property
  whose value callbacks may change.  Callback must not add any
  additional properties.  Setting `:extra' (and only that) in
  the initial call to `ghub-request' is allowed."
  (and (assq 'next (ghub-response-link-relations))
       (or (apply #'ghub-request nil nil nil args) t)))

(cl-defun ghub-wait (resource &optional duration &key username auth host)
  "Busy-wait up to DURATION seconds for RESOURCE to become available.

DURATION specifies how many seconds to wait at most.  It defaults
to 64 seconds.  The first attempt is made immediately, the second
after two seconds, and each subsequent attempt is made after
waiting as long again as we already waited between all preceding
attempts combined.

See `ghub-request' for information about the other arguments."
  (unless duration
    (setq duration 64))
  (with-local-quit
    (let ((total 0))
      (while (not (ghub-get resource nil
                            :noerror t
                            :username username
                            :auth auth
                            :host host))
        (message "Waited (%3ss of %ss) for %s..." total duration resource)
        (if (= total duration)
            (error "Github is taking too long to create %s" resource)
          (if (> total 0)
              (let ((wait (min total (- duration total))))
                (sit-for wait)
                (cl-incf total wait))
            (sit-for (setq total 2))))))))

(defun ghub-response-link-relations (&optional headers)
  "Return an alist of link relations in HEADERS.
If optional HEADERS is nil, then return those
in `ghub-response-headers'."
  (let ((rels (cdr (assoc "Link" (or headers ghub-response-headers)))))
    (and rels (mapcar (lambda (elt)
                        (pcase-let ((`(,url ,rel) (split-string elt "; ")))
                          (cons (intern (substring rel 5 -1))
                                (substring url 1 -1))))
                      (split-string rels ", ")))))

;;;; Internal

(defun ghub--handle-response (status args)
  (let ((buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer-multibyte t)
          (let* ((unpaginate (plist-get args :unpaginate))
                 (headers    (ghub--handle-response-headers status args))
                 (payload    (ghub--handle-response-payload args))
                 (payload    (ghub--handle-response-error status payload args))
                 (value      (nconc (plist-get args :value) payload))
                 (next       (cdr (assq 'next (ghub-response-link-relations
                                               headers)))))
            (when (numberp unpaginate)
              (cl-decf unpaginate))
            (plist-put args :url next)
            (plist-put args :value value)
            (plist-put args :unpaginate unpaginate)
            (or (and next
                     unpaginate
                     (or (eq unpaginate t)
                         (>  unpaginate 0))
                     (ghub-continue args))
                (let ((callback  (plist-get args :callback))
                      (errorback (plist-get args :errorback))
                      (err       (plist-get status :error)))
                  (if (or callback errorback)
                      (if (and err errorback)
                          (funcall errorback err headers status args)
                        (funcall callback  value headers status args))
                    value)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun ghub--handle-response-headers (status args)
  (goto-char (point-min))
  (let (headers)
    (while (re-search-forward "^\\([^:]*\\): \\(.+\\)"
                              url-http-end-of-headers t)
      (push (cons (match-string 1)
                  (match-string 2))
            headers))
    (setq headers (nreverse headers))
    (unless url-http-end-of-headers
      (error "BUG: missing headers %s" (plist-get status :error)))
    (goto-char (1+ url-http-end-of-headers))
    (if (plist-get args :callback)
        (setq-local ghub-response-headers headers)
      (setq-default ghub-response-headers headers))
    headers))

(defun ghub--handle-response-error (status payload args)
  (let ((noerror (plist-get args :noerror))
        (err (plist-get status :error)))
    (if err
        (if noerror
            (if (eq noerror 'return)
                payload
              (setcdr (last err) (list payload))
              nil)
          (pcase-let ((`(,symb . ,data) err))
            (if (eq symb 'error)
                (if (eq (car-safe data) 'http)
                    (signal 'ghub-http-error
                            (let ((code (car (cdr-safe data))))
                              (list code
                                    (nth 2 (assq code url-http-codes))
                                    payload)))
                  (signal 'ghub-error data))
              (signal symb data))))
      payload)))

(defun ghub--handle-response-payload (args)
  (funcall (or (plist-get args :reader)
               'ghub--read-json-payload)
           url-http-response-status))

(defun ghub--read-json-payload (status)
  (let ((raw (ghub--decode-payload)))
    (and raw
         (condition-case err
             (let ((json-object-type 'alist)
                   (json-array-type  'list)
                   (json-key-type    'symbol)
                   (json-false       nil)
                   (json-null        nil))
               (json-read-from-string raw))
           (json-readable-error
            (if (= status 500)
                nil
              (signal (car err) (cdr err))))))))

(defun ghub--decode-payload (&optional _status)
  (and (not (eobp))
       (decode-coding-string
        (buffer-substring-no-properties (point) (point-max))
        'utf-8)))

(defun ghub--url-encode-params (params)
  (mapconcat (lambda (param)
               (pcase-let ((`(,key . ,val) param))
                 (concat (url-hexify-string (symbol-name key)) "="
                         (if (integerp val)
                             (number-to-string val)
                           (url-hexify-string val)))))
             params "&"))

;;; Authentication
;;;; API

;;;###autoload
(defun ghub-create-token (host username package scopes &optional 2fa)
  "Create, store and return a new token.

HOST is the Github instance, usually \"api.github.com\".
USERNAME is the name of a user on that instance.
PACKAGE is the package that will use the token.
SCOPES are the scopes the token is given access to.
2FA is the two-factor authentication code, if any."
  (interactive
   (pcase-let ((`(,host ,username ,package)
                (ghub--read-triplet)))
     (list host username package
           (split-string
            (read-string
             "Scopes (separated by commas): "
             (mapconcat #'symbol-name
                        (symbol-value
                         (intern (format "%s-github-token-scopes" package)))
                        ","))
            "," t "[\s\t]+")
           (ghub--read-2fa-code))))
  (let ((user (ghub--ident username package)))
    (cl-destructuring-bind (_save token)
        (ghub--auth-source-get (list :save-function :secret)
          :create t :host host :user user
          :secret
          (cdr (assq 'token
                     (ghub-post
                      "/authorizations"
                      `((scopes . ,scopes)
                        (note   . ,(ghub--ident-github package)))
                      :username username :auth (or 2fa 'basic) :host host))))
      ;; If the Auth-Source cache contains the information that there
      ;; is no value, then setting the value does not invalidate that
      ;; now incorrect information.
      (auth-source-forget (list :host host :user user))
      token)))

;;;###autoload
(defun ghub-token-scopes (host username package)
  "Return and echo the scopes of the specified token.
This is intended for debugging purposes only.  The user
has to provide several values including their password."
  (interactive (ghub--read-triplet))
  (let ((scopes
         (cdr (assq 'scopes (ghub--get-token-plist host username package)))))
    (when (called-interactively-p 'any)
      ;; Also show the input values to make it easy for package
      ;; authors to verify that the user has done it correctly.
      (message "Scopes for %s@%s: %S"
               (ghub--ident username package)
               host scopes))
    scopes))

;;;; Internal

(defun ghub--auth (host auth &optional username forge)
  (unless username
    (setq username (ghub--username host)))
  (if (eq auth 'basic)
      (if (eq forge 'gitlab)
          (error "Gitlab does not support basic authentication")
        `(("Authorization" . ,(ghub--basic-auth host username))))
    `((,(cons (if (eq forge 'gitlab) "Private-Token" "Authorization")
              (concat
               (and (not (eq forge 'gitlab)) "token ")
               (encode-coding-string
                (cl-typecase auth
                  (string auth)
                  (null   (ghub--token host username 'ghub nil forge))
                  (symbol (ghub--token host username auth  nil forge))
                  (t (signal 'wrong-type-argument
                             `((or stringp symbolp) ,auth))))
                'utf-8))))
      ,@(and (stringp auth)
             (= (length auth) 6)
             `(("X-Github-OTP" . ,(encode-coding-string auth 'utf-8)))))))

(defun ghub--basic-auth (host username)
  (let ((url (url-generic-parse-url (concat "https://" host))))
    (setf (url-user url) username)
    (url-basic-auth url t)))

(defun ghub--token (host username package &optional nocreate forge)
  (let ((user (ghub--ident username package)))
    (or (ghub--auth-source-get :secret :host host :user user)
        (progn
          ;; Auth-Source caches the information that there is no
          ;; value, but in our case that is a situation that needs
          ;; fixing so we want to keep trying by invalidating that
          ;; information.  The (:max 1) is needed for Emacs releases
          ;; before 26.1.
          (auth-source-forget (list :max 1 :host host :user
          user))
          (and (not nocreate)
               (if (eq forge 'gitlab)
                   (error
                    (concat "Required Gitlab token does not exist.  See "
                            "https://magit.vc/manual/ghub/Gitlab-Support.html "
                            "for instructions."))
                 (ghub--confirm-create-token host username package)))))))

(defun ghub--host (&optional forge)
  (if (eq forge 'gitlab)
      (or (ignore-errors (car (process-lines "git" "config" "gitlab.host")))
          (bound-and-true-p glab-default-host))
    (or (ignore-errors (car (process-lines "git" "config" "github.host")))
        ghub-default-host)))

(defun ghub--username (host &optional forge)
  (let ((var (cond ((string-prefix-p "api.github.com" host) "github.user")
                   ((string-prefix-p "gitlab.com/api" host) "gitlab.user")
                   ((eq forge 'gitlab)     (format "gitlab.%s.user" host))
                   (t                      (format "github.%s.user" host)))))
    (condition-case nil
        (car (process-lines "git" "config" var))
      (error
       (let ((user (read-string
                    (format "Git variable `%s' is unset.  Set to: " var))))
         (or (and user (progn (call-process "git" nil nil nil
                                            "config" "--global" var user)
                              user))
             (user-error "Abort")))))))

(defun ghub--ident (username package)
  (format "%s^%s" username package))

(defun ghub--ident-github (package)
  (format "Emacs package %s @ %s"
          package
          (or ghub-override-system-name (system-name))))

(defun ghub--package-scopes (package)
  (let ((var (intern (format "%s-github-token-scopes" package))))
    (if (boundp var)
        (symbol-value var)
      (error "%s fails to define %s" package var))))

(defun ghub--confirm-create-token (host username package)
  (let* ((ident (ghub--ident-github package))
         (scopes (ghub--package-scopes package))
         (max-mini-window-height 40))
    (if (let ((message-log-max nil))
          (yes-or-no-p
           (format
            "Such a Github API token is not available:

  Host:    %s
  User:    %s
  Package: %s

  Scopes requested in `%s-github-token-scopes':\n%s
  Store on Github as:\n    %S
  Store locally according to option `auth-sources':\n    %S
%s
If in doubt, then abort and first view the section of the Ghub
documentation called \"Manually Creating and Storing a Token\".

Otherwise confirm and then provide your Github username and
password at the next two prompts.  Depending on the backend
you might have to provide a passphrase and confirm that you
really want to save the token.

Create and store such a token? "
            host username package package
            (mapconcat (lambda (scope) (format "    %s" scope)) scopes "\n")
            ident auth-sources
            (if (and (stringp (car auth-sources))
                     (not (string-suffix-p ".gpg" (car auth-sources))))
                (format "
WARNING: The token will be stored unencrypted in %S.
         If you don't want that, you have to abort and customize
         the `auth-sources' option.\n" (car auth-sources))
              ""))))
        (let ((2fa (ghub--read-2fa-code)))
          (when (ghub--get-token-id host username package 2fa)
            (if (yes-or-no-p
                 (format
                  "A token named %S\nalready exists on Github.  Replace it?"
                  ident))
                (progn
                  (when 2fa ; might already have expired
                    (setq 2fa (ghub--read-2fa-code
                               "Two-factor authentication code")))
                  (ghub--delete-token host username package 2fa))
              (user-error "Abort")))
          (ghub-create-token host username package scopes 2fa))
      (user-error "Abort"))))

(defun ghub--get-token-id (host username package &optional 2fa)
  (let ((ident (ghub--ident-github package)))
    (cl-some (lambda (x)
               (let-alist x
                 (and (equal .app.name ident) .id)))
             (ghub-get "/authorizations"
                       nil :username username :auth (or 2fa 'basic) :host host
                       :unpaginate t))))

(defun ghub--get-token-plist (host username package &optional 2fa)
  (ghub-get (format "/authorizations/%s"
                    (ghub--get-token-id host username package))
            nil :username username :auth (or 2fa 'basic) :host host))

(defun ghub--delete-token (host username package &optional 2fa)
  (ghub-delete (format "/authorizations/%s"
                       (ghub--get-token-id host username package))
               nil :username username :auth (or 2fa 'basic) :host host))

(defun ghub--read-triplet ()
  (let ((host (read-string "Host: " (ghub--host))))
    (list host
          (read-string "Username: " (ghub--username host))
          (intern (read-string "Package: " "ghub")))))

(defun ghub--read-2fa-code (&optional prompt)
  (let* ((prompt1 (or prompt "\
If, and only if, you have enabled two-factor authentication, then
provide the code now (otherwise just press `RET' with no input)"))
         (prompt2 "\
.\n\nPlease try again.  (Such codes are six digits long.)")
         (prompt prompt1)
         valid abort)
    (while (and (not valid)
                (not abort))
      (let ((input (read-string (concat prompt ": "))))
        (cond ((equal input "")
               (setq abort t))
              ((string-match-p "\\`[0-9]\\{6\\}\\'" input)
               (setq valid input))
              (t
               (setq prompt (concat prompt1 prompt2))))))
    valid))

(defun ghub--auth-source-get (key:s &rest spec)
  (declare (indent 1))
  (let ((plist (car (apply #'auth-source-search :max 1 spec))))
    (cl-flet ((value (k) (let ((v (plist-get plist k)))
                           (if (functionp v) (funcall v) v))))
      (if (listp key:s)
          (mapcar #'value key:s)
        (value key:s)))))

(advice-add 'auth-source-netrc-parse-next-interesting :around
            'auth-source-netrc-parse-next-interesting@save-match-data)
(defun auth-source-netrc-parse-next-interesting@save-match-data (fn)
  "Save match-data for the benefit of caller `auth-source-netrc-parse-one'.
Without wrapping this function in `save-match-data' the caller
won't see the secret from a line that is followed by a commented
line."
  (save-match-data (funcall fn)))

;;; ghub.el ends soon
(provide 'ghub)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ghub.el ends here
