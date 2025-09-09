;;; ghub.el --- Client libraries for Git forge APIs  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2025 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.ghub@jonas.bernoulli.dev>
;; Homepage: https://github.com/magit/ghub
;; Keywords: tools

;; Package-Version: 5.0.0
;; Package-Requires: (
;;     (emacs   "29.1")
;;     (compat  "30.1")
;;     (llama    "1.0")
;;     (treepy "0.1.2"))

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

;; Ghub provides basic support for using the APIs of various Git forges
;; from Emacs packages.  Originally it only supported the Github REST
;; API, but now it also supports the Github GraphQL API as well as the
;; REST APIs of Gitlab, Gitea, Gogs and Bitbucket.

;; Ghub abstracts access to API resources using only a handful of basic
;; functions such as `ghub-get'.  These are convenience wrappers around
;; `ghub-request'.

;; When accessing Github, then Ghub handles the creation and storage of
;; access tokens using a setup wizard to make it easier for users to get
;; started.  The tokens for other forges have to be created manually.

;; Ghub is intentionally limited to only provide these two essential
;; features — basic request functions and guided setup — to avoid being
;; too opinionated, which would hinder wide adoption.  It is assumed that
;; wide adoption would make life easier for users and maintainers alike,
;; because then all packages that talk to forge APIs could be configured
;; the same way.

;; Please consult the manual (info "ghub") for more information.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'compat)
(require 'gnutls)
(require 'let-alist)
(require 'llama)
(require 'url)
(require 'url-auth)
(require 'url-http)

(eval-when-compile (require 'subr-x))

(defvar url-callback-arguments)
(defvar url-http-end-of-headers)
(defvar url-http-extra-headers)
(defvar url-http-response-status)

;;; Settings

(defvar ghub-default-host-alist
  '((github    . "api.github.com")
    (gitlab    . "gitlab.com/api/v4")
    (gitea     . "localhost:3000/api/v1")
    (gogs      . "localhost:3000/api/v1")
    (bitbucket . "api.bitbucket.org/2.0"))
  "Alist of default hosts used when the respective `FORGE.host' is not set.")

(defvar ghub-github-token-scopes '(repo)
  "The Github API scopes that your private tools need.

You have to manually create or update the token at
https://github.com/settings/tokens.  This variable
only serves as documentation.")

(defvar ghub-insecure-hosts nil
  "List of hosts that use http instead of https.")

(defvar ghub-debug nil
  "Record additional debug information.")

;;; Request
;;;; Object

(cl-defstruct (ghub--req
               (:constructor ghub--make-req)
               (:copier nil))
  (url         nil :read-only nil)
  (forge       nil :read-only t)
  (silent      nil :read-only t)
  (method      nil :read-only t)
  (headers     nil :read-only t)
  (handler     nil :read-only t)
  (unpaginate  nil :read-only nil)
  (noerror     nil :read-only t)
  (reader      nil :read-only t)
  (buffer      nil :read-only t)
  (synchronous nil :read-only t)
  (callback    nil :read-only t)
  (errorback   nil :read-only t)
  (value       nil :read-only nil)
  (extra       nil :read-only nil))

(defalias 'ghub-req-extra #'ghub--req-extra)

;;;; API

(define-error 'ghub-error "Ghub/Url Error" 'error)
(define-error 'ghub-http-error "HTTP Error" 'ghub-error)

(defvar ghub-response-headers nil
  "The headers returned in response to the last request.
`ghub-request' returns the response body and stores the
response headers in this variable.")

(cl-defun ghub-head (resource
                     &optional params
                     &key query payload headers
                     silent unpaginate noerror reader
                     username auth host
                     callback errorback extra)
  "Make a `HEAD' request for RESOURCE, with optional query PARAMS.
Like calling `ghub-request' (which see) with \"HEAD\" as METHOD."
  (ghub-request "HEAD" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-get (resource
                    &optional params
                    &key query payload headers
                    silent unpaginate noerror reader
                    username auth host forge
                    callback errorback extra)
  "Make a `GET' request for RESOURCE, with optional query PARAMS.
Like calling `ghub-request' (which see) with \"GET\" as METHOD."
  (ghub-request "GET" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host :forge forge
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-put (resource
                    &optional params
                    &key query payload headers
                    silent unpaginate noerror reader
                    username auth host forge
                    callback errorback extra)
  "Make a `PUT' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"PUT\" as METHOD."
  (ghub-request "PUT" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host :forge forge
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-post (resource
                     &optional params
                     &key query payload headers
                     silent unpaginate noerror reader
                     username auth host forge
                     callback errorback extra)
  "Make a `POST' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"POST\" as METHOD."
  (ghub-request "POST" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host :forge forge
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-patch (resource
                      &optional params
                      &key query payload headers
                      silent unpaginate noerror reader
                      username auth host forge
                      callback errorback extra)
  "Make a `PATCH' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"PATCH\" as METHOD."
  (ghub-request "PATCH" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host :forge forge
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-delete (resource
                       &optional params
                       &key query payload headers
                       silent unpaginate noerror reader
                       username auth host forge
                       callback errorback extra)
  "Make a `DELETE' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"DELETE\" as METHOD."
  (ghub-request "DELETE" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host :forge forge
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-request ( method resource
                         &optional params
                         &key query payload headers
                         silent unpaginate noerror reader
                         username auth host forge
                         callback errorback value extra)
  "Make a request for RESOURCE and return the response body.

Also place the response headers in `ghub-response-headers'.

METHOD is the HTTP method, given as a string.
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

If SILENT is non-nil, then don't message progress reports and
  the like.

If UNPAGINATE is t, then make as many requests as necessary to
  get all values.  If UNPAGINATE is a natural number, then get
  at most that many pages.  For any other non-nil value raise
  an error.
If NOERROR is non-nil, then do not raise an error if the request
  fails and return nil instead.  If NOERROR is `return', then
  return the error payload instead of nil.
If READER is non-nil, then it is used to read and return from the
  response buffer.  The default is `ghub--read-json-payload'.
  For the very few resources that do not return JSON, you might
  want to use `ghub--decode-payload'.

If USERNAME is non-nil, then make a request on behalf of that
  user.  It is better to specify the user using the Git variable
  `github.user' for \"api.github.com\", or `github.HOST.user' if
  connecting to a Github Enterprise instance.

Each package that uses `ghub' should use its own token.  If AUTH
  is nil, then the generic `ghub' token is used instead.  This
  is only acceptable for personal utilities.  A packages that
  is distributed to other users should always use this argument
  to identify itself, using a symbol matching its name.

  Package authors who find this inconvenient should write a
  wrapper around this function and possibly for the
  method-specific functions as well.

  Some symbols have a special meaning.  `none' means to make an
  unauthorized request.  `basic' means to make a password based
  request.  If the value is a string, then it is assumed to be
  a valid token.  `basic' and an explicit token string are only
  intended for internal and debugging uses.

If HOST is non-nil, then connect to that Github instance.  This
  defaults to \"api.github.com\".  When a repository is connected
  to a Github Enterprise instance, then it is better to specify
  that using the Git variable `github.host' instead of using this
  argument.

If optional FORGE is nil, then it is assumed that HOST is a
  Github host.  When connecting to another forge type, then
  FORGE must be one of `gitlab', `gitea', `gogs' or `bitbucket'.

If CALLBACK and/or ERRORBACK is non-nil, then make one or more
  asynchronous requests and call CALLBACK or ERRORBACK when
  finished.  If no error occurred, then call CALLBACK, unless
  that is nil.

  If an error occurred, then call ERRORBACK, or if that is nil,
  then CALLBACK.  ERRORBACK can also be t, in which case an error
  is signaled instead.  NOERROR is ignored for all asynchronous
  requests.

Both callbacks are called with four arguments.
  1. For CALLBACK, the combined value of the retrieved pages.
     For ERRORBACK, the error that occurred when retrieving the
     last page.
  2. The headers of the last page as an alist.
  3. Status information provided by `url-retrieve'. Its `:error'
     property holds the same information as ERRORBACK's first
     argument.
  4. A `ghub--req' struct, which can be passed to `ghub-continue'
     (which see) to retrieve the next page, if any."
  (declare (indent defun))
  (cl-assert (or (booleanp unpaginate) (natnump unpaginate)))
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
  (when (or callback errorback)
    (setq noerror t))
  (ghub--retrieve
   (ghub--encode-payload payload)
   (ghub--make-req
    :url         (ghub--encode-url host resource query)
    :forge       forge
    :silent      silent
    :method      (encode-coding-string method 'utf-8) ;#35
    :headers     (ghub--headers headers host auth username forge)
    :handler     #'ghub--handle-response
    :unpaginate  unpaginate
    :noerror     noerror
    :reader      reader
    :buffer      (current-buffer)
    :synchronous (not (or callback errorback))
    :callback    callback
    :errorback   errorback
    :value       value
    :extra       extra)))

(defun ghub-continue (req)
  "If there is a next page, then retrieve that.

This function is only intended to be called from callbacks.  If
there is a next page, then retrieve that and return the buffer
that the result will be loaded into, or t if the process has
already completed.  If there is no next page, then return nil.

Callbacks are called with four arguments (see `ghub-request').
The forth argument is a `ghub--req' struct, intended to be passed
to this function.  A callback may use the struct's `extra' slot
to pass additional information to the callback that will be
called after the next request has finished.  Use the function
`ghub-req-extra' to get and set the value of this slot."
  (and (assq 'next (ghub-response-link-relations req))
       (or (ghub--retrieve nil req) t)))

(cl-defun ghub-wait (resource
                     &optional duration
                     &key username auth host forge)
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
      (while (not (ghub-request "GET" resource nil
                                :noerror t
                                :username username
                                :auth auth
                                :host host
                                :forge forge))
        (message "Waited (%3ss of %ss) for %s..." total duration resource)
        (if (= total duration)
            (error "%s is taking too long to create %s"
                   (if forge (capitalize (symbol-name forge)) "Github")
                   resource)
          (if (> total 0)
              (let ((wait (min total (- duration total))))
                (sit-for wait)
                (cl-incf total wait))
            (sit-for (setq total 2))))))))

(defun ghub-response-link-relations (req &optional headers payload)
  "Return an alist of link relations in HEADERS.
If optional HEADERS is nil, then return those that were
previously stored in the variable `ghub-response-headers'.

When accessing a Bitbucket instance then the link relations
are in PAYLOAD instead of HEADERS, making their API merely
RESTish and forcing this function to append those relations
to the value of `ghub-response-headers', for later use when
this function is called with nil for PAYLOAD."
  (if (eq (ghub--req-forge req) 'bitbucket)
      (if payload
          (let* ((page (seq-keep (##assq % payload)
                                 '(size page pagelen next previous)))
                 (headers (cons (cons 'link-alist page) headers)))
            (if (and req (or (ghub--req-callback req)
                             (ghub--req-errorback req)))
                (setq-local ghub-response-headers headers)
              (setq-default ghub-response-headers headers))
            page)
        (cdr (assq 'link-alist ghub-response-headers)))
    (and-let* ((headers (or headers ghub-response-headers))
               (rels (cdr (or (assoc "Link" headers)
                              (assoc "link" headers)))))
      (mapcar (lambda (elt)
                (pcase-let ((`(,url ,rel) (split-string elt "; ")))
                  (cons (intern (substring rel 5 -1))
                        (substring url 1 -1))))
              (split-string rels ", ?")))))

(cl-defun ghub-repository-id (owner name &key username auth host forge noerror)
  "Return the id of the specified repository.
Signal an error if the id cannot be determined."
  (or (pcase forge
        ((or 'nil 'github)
         (let-alist (ghub-graphql
                     '(query (repository [(owner $owner String!)
                                          (name  $name  String!)]
                                         id))
                     `((owner . ,owner)
                       (name  . ,name))
                     :username username :auth auth :host host)
           .data.repository.id))
        ('gitlab
         (number-to-string
          (alist-get
           'id (ghub-get (format "/projects/%s%%2F%s"
                                 (string-replace "/" "%2F" owner)
                                 name)
                         nil :forge 'gitlab
                         :username username :auth auth :host host))))
        ((or 'forgejo 'gitea 'gogs)
         (number-to-string
          (alist-get
           'id (ghub-get (format "/repos/%s/%s" owner name)
                         nil :forge forge
                         :username username :auth auth :host host))))
        ('bitbucket
         (substring
          (alist-get 'uuid
                     (ghub-get (format "/repositories/%s/%s" owner name)
                               nil :forge 'bitbucket
                               :username username :auth auth :host host))
          1 -1))
        (_ (error "ghub-repository-id: Forge type `%s' is unknown" forge)))
      (and (not noerror)
           (error "Repository %S does not exist on %S.\n%s%S?"
                  (concat owner "/" name)
                  (or host (ghub--host forge))
                  "Maybe it was renamed and you have to update "
                  "remote.<remote>.url"))))

;;;; Internal

(cl-defun ghub--retrieve (payload req)
  (pcase-let*
      (((cl-struct ghub--req headers method url handler silent synchronous) req)
       (url-request-extra-headers
        (if (functionp headers) (funcall headers) headers))
       (url-request-method method)
       (url-request-data payload)
       (url-show-status nil))
    (if synchronous
        (if-let ((buf (url-retrieve-synchronously url silent)))
            (with-current-buffer buf
              (funcall handler (car url-callback-arguments) req))
          (error "ghub--retrieve: No buffer returned"))
      (url-retrieve url handler (list req) silent))))

(defun ghub--handle-response (status req)
  (let ((buf (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer-multibyte t)
          (let* ((unpaginate (ghub--req-unpaginate req))
                 (headers (ghub--handle-response-headers status req))
                 (payload (ghub--handle-response-payload req))
                 (payload (ghub--handle-response-error status payload req))
                 (value   (ghub--handle-response-value payload req))
                 (prev    (ghub--req-url req))
                 (next    (cdr (assq 'next (ghub-response-link-relations
                                            req headers payload)))))
            (when (numberp unpaginate)
              (cl-decf unpaginate))
            (setf (ghub--req-url req)
                  (url-generic-parse-url next))
            (setf (ghub--req-unpaginate req) unpaginate)
            (or (and next
                     unpaginate
                     (or (eq unpaginate t)
                         (>  unpaginate 0))
                     (ghub-continue req))
                (let ((req-buf   (ghub--req-buffer req))
                      (callback  (ghub--req-callback req))
                      (errorback (ghub--req-errorback req))
                      (err       (plist-get status :error)))
                  (cond ((and err errorback)
                         (setf (ghub--req-url req) prev)
                         (funcall (if (eq errorback t)
                                      'ghub--errorback
                                    errorback)
                                  err headers status req))
                        (callback
                         (save-current-buffer
                           (when (buffer-live-p req-buf)
                             (set-buffer req-buf))
                           (funcall callback value headers status req)))
                        (t value))))))
      (when (and (buffer-live-p buf)
                 (not (buffer-local-value 'ghub-debug buf)))
        (kill-buffer buf)))))

(defun ghub--handle-response-headers (_status req)
  (let (headers)
    (when (memq url-http-end-of-headers '(nil 0))
      (unless url-debug (setq url-debug t))
      (error "BUG: Missing headers in response buffer %s" (current-buffer)))
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward "^\\([^:]*\\): \\(.+\\)"
                              url-http-end-of-headers t)
      (push (cons (match-string 1)
                  (match-string 2))
            headers))
    (setq headers (nreverse headers))
    (goto-char (1+ url-http-end-of-headers))
    (if (and req (or (ghub--req-callback req)
                     (ghub--req-errorback req)))
        (setq-local ghub-response-headers headers)
      (setq-default ghub-response-headers headers))
    headers))

(defun ghub--handle-response-error (status payload req)
  (if-let ((err (plist-get status :error)))
      (if-let ((noerror (ghub--req-noerror req)))
          (if (eq noerror 'return)
              payload
            (setcdr (last err) (list payload))
            nil)
        (ghub--signal-error err payload req))
    payload))

(defun ghub--signal-error (err &optional payload req)
  (pcase-let ((`(,symb . ,data) err))
    (if (eq symb 'error)
        (if (eq (car-safe data) 'http)
            (signal 'ghub-http-error
                    (let ((code (car (cdr-safe data))))
                      (list code
                            (nth 2 (assq code url-http-codes))
                            (and req (url-recreate-url (ghub--req-url req)))
                            payload)))
          (signal 'ghub-error data))
      (signal symb data))))

(defun ghub--errorback (err _headers _status req)
  (ghub--signal-error err (nth 3 err) req))

(defun ghub--handle-response-value (payload req)
  (setf (ghub--req-value req)
        (nconc (ghub--req-value req)
               (if-let ((nested (and (eq (ghub--req-forge req) 'bitbucket)
                                     (assq 'values payload))))
                   (cdr nested)
                 payload))))

(defun ghub--handle-response-payload (req)
  (funcall (or (ghub--req-reader req)
               'ghub--read-json-payload)
           url-http-response-status))

(defun ghub--read-json-payload (_status &optional json-type-args)
  (and-let* ((payload (ghub--decode-payload)))
    (ghub--assert-json-available)
    (condition-case nil
        (apply #'json-parse-string payload
               (or json-type-args
                   '( :object-type alist
                      :array-type list
                      :null-object nil
                      :false-object nil)))
      (json-parse-error
       (when ghub-debug
         (pop-to-buffer (current-buffer)))
       (setq-local ghub-debug t)
       `((message . ,(if (looking-at "<!DOCTYPE html>")
                         (if (re-search-forward
                              "<p>\\(?:<strong>\\)?\\([^<]+\\)" nil t)
                             (match-string 1)
                           "error description missing")
                       (string-trim (buffer-substring (point) (point-max)))))
         (documentation_url
          . "https://github.com/magit/ghub/wiki/Github-Errors"))))))

(defun ghub--decode-payload (&optional _status)
  (and (not (eobp))
       (decode-coding-string
        (buffer-substring-no-properties (point) (point-max))
        'utf-8)))

(defun ghub--encode-payload (payload)
  (cl-typecase payload
    (null nil)
    (string (encode-coding-string payload 'utf-8))
    (t (ghub--assert-json-available)
       (encode-coding-string
        (json-serialize payload
                        :null-object :null
                        :false-object nil)
        'utf-8))))

(defun ghub--encode-url (host resource &optional query)
  (url-generic-parse-url
   (concat (if (member host ghub-insecure-hosts) "http://" "https://")
           ;; Needed for some Github Enterprise instances.
           (cond
            ((and (equal resource "/graphql")
                  (string-suffix-p "/v3" host))
             (substring host 0 -3))
            ;; Needed for all Gitlab instances.
            ((and (equal resource "/api/graphql")
                  (string-suffix-p "/api/v4" host))
             (substring host 0 -7))
            (host))
           resource
           (and query (concat "?" (ghub--url-encode-params query))))))

(defun ghub--url-encode-params (params)
  (mapconcat (lambda (param)
               (pcase-let ((`(,key . ,val) param))
                 (concat (url-hexify-string (symbol-name key)) "="
                         (cl-typecase val
                           (integer (number-to-string val))
                           (boolean (if val "true" "false"))
                           (t (url-hexify-string val))))))
             params "&"))

(defun ghub--assert-json-available ()
  (unless (and (fboundp 'json-available-p)
               (json-available-p))
    (error "Ghub requires Emacs 29 --with-json or Emacs >= 30")))

;;; Authentication
;;;; API

;;;###autoload
(defun ghub-clear-caches ()
  "Clear all caches that might negatively affect Ghub.

If a library that is used by Ghub caches incorrect information
such as a mistyped password, then that can prevent Ghub from
asking the user for the correct information again.

Set `url-http-real-basic-auth-storage' to nil
and call `auth-source-forget+'."
  (interactive)
  (setq url-http-real-basic-auth-storage nil)
  (auth-source-forget+))

;;;; Internal

(defun ghub--headers (headers host auth username forge)
  (push (cons "Content-Type" "application/json") headers)
  (if (eq auth 'none)
      headers
    (unless (or username (stringp auth))
      (setq username (ghub--username host forge)))
    (lambda ()
      (if (eq auth 'basic)
          (cons (cons "Authorization" (ghub--basic-auth host username))
                headers)
        (cons (ghub--auth host auth username forge) headers)))))

(cl-defgeneric ghub--auth (host auth &optional username forge)
  (unless username
    (setq username (ghub--username host forge)))
  (if (eq auth 'basic)
      (pcase-exhaustive forge
        ((or 'nil 'forgejo 'gitea 'gogs 'bitbucket)
         (cons "Authorization" (ghub--basic-auth host username)))
        ((or 'github 'gitlab)
         (error "%s does not support basic authentication"
                (capitalize (symbol-name forge)))))
    (cons (pcase-exhaustive forge
            ((or 'nil 'forgejo 'github 'gitea 'gogs 'bitbucket) "Authorization")
            ('gitlab "Private-Token"))
          (if (eq forge 'bitbucket)
              ;; For some undocumented reason Bitbucket supports
              ;; values of the form "token <token>" only for GET
              ;; requests.  For PUT requests we have to use basic
              ;; authentication.  Note that the secret is a token
              ;; (aka "app password"), not the actual password.
              ;; The documentation fails to mention this little
              ;; detail.  See #97.
              (concat "Basic "
                      (base64-encode-string
                       (concat username ":"
                               (ghub--token host username auth nil forge))
                       t))
            (concat
             (and (not (eq forge 'gitlab)) "token ")
             (encode-coding-string
              (cl-typecase auth
                (string auth)
                (null   (ghub--token host username 'ghub nil forge))
                (symbol (ghub--token host username auth  nil forge))
                (t (signal 'wrong-type-argument
                           `((or stringp symbolp) ,auth))))
              'utf-8))))))

(defun ghub--basic-auth (host username)
  (let ((url (url-generic-parse-url
              (if (member host ghub-insecure-hosts) "http://" "https://"))))
    (setf (url-user url) username)
    (url-basic-auth url t)))

(defun ghub--token (host username package &optional nocreate forge)
  (let* ((user (ghub--ident username package))
         (token (or (ghub--auth-source-get :secret :host host :user user)
                    (and (string-match "\\`\\([^/]+\\)" host)
                         (ghub--auth-source-get :secret
                           :host (match-string 1 host)
                           :user user)))))
    (unless (or token nocreate)
      (error "\
Required %s token (%S for %s%S) does not exist.
See https://magit.vc/manual/ghub/Getting-Started.html
or (info \"(ghub)Getting Started\") for instructions."
             (capitalize (symbol-name (or forge 'github)))
             user
             (if (string-match "\\`\\([^/]+\\)" host)
                 (format "either %S or " (match-string 1 host))
               "")
             host))
    (if (functionp token) (funcall token) token)))

(cl-defmethod ghub--host (&optional forge)
  (let ((forge (or forge 'github)))
    (or (ghub--git-get (format "%s.host" forge))
        (alist-get forge ghub-default-host-alist))))

(cl-defmethod ghub--username (host &optional forge)
  (let* ((forge (or forge 'github))
         (host (or host (ghub--host forge)))
         (var (format "%s.%s.user" forge host)))
    (or (ghub--git-get var)
        (if-let ((_(equal host (alist-get forge ghub-default-host-alist)))
                 (default-var (format "%s.user" forge)))
            (or (ghub--git-get default-var)
                (user-error "%s; `%s' and `%s' are both unset"
                            "Cannot determine username" var default-var))
          (user-error "Cannot determine username; `%s' is unset" var)))))

(defun ghub--ident (username package)
  (format "%s^%s" username package))

(defun ghub--auth-source-get (keys &rest spec)
  (declare (indent 1))
  (if-let ((plist (car (apply #'auth-source-search
                              (append spec (list :max 1))))))
      (if (keywordp keys)
          (plist-get plist keys)
        (mapcar (##plist-get plist %) keys))
    ;; Auth-Source caches the information that there is no value, but in
    ;; our case that is a situation that needs fixing, so we want to keep
    ;; trying, by invalidating that information.
    (auth-source-forget spec)
    nil))

(defun ghub--git-get (var)
  (catch 'non-zero
    (car (process-lines-handling-status
          "git" (##unless (zerop %) (throw 'non-zero nil))
          ;; Beginning with Git v2.46 we could use "get".
          "config" "--get" var))))

;;; _
(provide 'ghub)
(require 'ghub-graphql)
(require 'ghub-legacy)
;;; ghub.el ends here
