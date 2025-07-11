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

(cl-defun ghub-graphql (graphql
                        &optional variables
                        &key username auth host forge
                        headers silent
                        callback errorback value extra)
  (cl-assert (not (stringp variables)))
  (cl-assert (or (stringp graphql)
                 (memq (car-safe graphql) '(query mutation))))
  (unless (stringp graphql)
    (setq graphql (gsexp-encode (ghub--graphql-prepare-query graphql))))
  (ghub-request "POST"
                (if (eq forge 'gitlab) "/api/graphql" "/graphql")
                nil
                :payload `((query . ,graphql)
                           ,@(and variables `((variables ,@variables))))
                :headers headers :silent silent
                :username username :auth auth :host host :forge forge
                :callback callback :errorback errorback
                :extra extra :value value))

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

(cl-defun ghub--graphql-vacuum
    ( query variables callback
      &optional until
      &key narrow username auth host forge
      headers paginate errorback noerror synchronous)
  (ghub-query query variables
    :until until :narrow narrow :headers headers :paginate paginate
    :callback callback
    :errorback errorback :noerror noerror :synchronous synchronous
    :username username :auth auth :host host :forge forge))

(defconst ghub-fetch-repository-sparse
  '(query
    (repository
     [(owner $owner String!)
      (name  $name  String!)]
     name
     id
     createdAt
     updatedAt
     nameWithOwner
     (parent nameWithOwner)
     description
     homepageUrl
     (defaultBranchRef name)
     isArchived
     isFork
     isLocked
     isMirror
     isPrivate
     hasDiscussionsEnabled
     hasIssuesEnabled
     hasWikiEnabled
     (licenseInfo name)
     (stargazers totalCount)
     (watchers totalCount))))

(defconst ghub-fetch-repository
  `(query
    (repository
     ,@(cdr (cadr ghub-fetch-repository-sparse))
     (assignableUsers [(:edges t)]
                      id
                      login
                      name)
     (discussionCategories [(:edges t)]
                           id
                           name
                           emoji
                           isAnswerable
                           description)
     (discussions    [(:edges t)
                      (:singular discussion number)
                      (orderBy ((field UPDATED_AT) (direction DESC)))]
                     id
                     databaseId
                     number
                     url
                     stateReason
                     ;; Discussions lack isReadByViewer.
                     (answer id)
                     (author login)
                     title
                     createdAt
                     updatedAt
                     closedAt
                     locked
                     (category id)
                     body
                     (comments  [(:edges t)]
                                id
                                databaseId
                                (author login)
                                createdAt
                                updatedAt
                                body
                                (replies [(:edges 20)]
                                         id
                                         databaseId
                                         (author login)
                                         createdAt
                                         updatedAt
                                         body))
                     (labels    [(:edges t)]
                                id))
     (issues         [(:edges t)
                      (:singular issue number)
                      (orderBy ((field UPDATED_AT) (direction DESC)))]
                     number
                     id
                     state
                     stateReason
                     isReadByViewer
                     (author login)
                     title
                     createdAt
                     updatedAt
                     closedAt
                     locked
                     (milestone id)
                     body
                     (assignees [(:edges t)]
                                id)
                     (comments  [(:edges t)]
                                id
                                databaseId
                                (author login)
                                createdAt
                                updatedAt
                                body)
                     (labels    [(:edges t)]
                                id))
     (labels         [(:edges t)
                      (:singular label id)]
                     id
                     name
                     color
                     description)
     (milestones     [(:edges t)
                      (:singular milestone id)]
                     id
                     number
                     title
                     createdAt
                     updatedAt
                     dueOn
                     closedAt
                     description)
     (pullRequests   [(:edges t)
                      (:singular pullRequest number)
                      (orderBy ((field UPDATED_AT) (direction DESC)))]
                     number
                     id
                     state
                     isReadByViewer
                     (author login)
                     title
                     createdAt
                     updatedAt
                     closedAt
                     mergedAt
                     isDraft
                     locked
                     maintainerCanModify
                     isCrossRepository
                     (milestone id)
                     body
                     (baseRef name
                              (repository nameWithOwner))
                     baseRefOid
                     (headRef name
                              (repository (owner login)
                                          nameWithOwner))
                     headRefOid
                     (assignees [(:edges t)]
                                id)
                     (reviewRequests [(:edges t)]
                                     (requestedReviewer "... on User { id }\n"))
                     (comments  [(:edges t)]
                                id
                                databaseId
                                (author login)
                                createdAt
                                updatedAt
                                body)
                     (labels    [(:edges t)]
                                id)))))

(defconst ghub-fetch-repository-review-threads
  '(query
    (repository
     [(owner $owner String!)
      (name  $name  String!)]
     (pullRequests   [(:edges t)
                      (:singular pullRequest number)
                      (orderBy ((field UPDATED_AT) (direction DESC)))]
                     number
                     baseRefOid
                     headRefOid
                     (reviewThreads [(:edges t)]
                                    id
                                    line
                                    originalLine
                                    diffSide
                                    (resolvedBy login)
                                    (comments [(:edges t)]
                                              id
                                              databaseId
                                              (author login)
                                              createdAt
                                              updatedAt
                                              body
                                              (replyTo databaseId)
                                              (originalCommit oid)
                                              path))))))

(cl-defun ghub-fetch-repository ( owner name callback
                                  &optional until
                                  &key username auth host forge
                                  headers paginate errorback sparse)
  "Asynchronously fetch forge data about the specified repository.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub-query (if sparse
                  ghub-fetch-repository-sparse
                ghub-fetch-repository)
              `((owner . ,owner)
                (name  . ,name))
              :until     until
              :narrow    '(repository)
              :username  username
              :auth      auth
              :host      host
              :forge     forge
              :headers   headers
              :paginate  paginate
              :callback  callback
              :errorback errorback))

(cl-defun ghub-fetch-discussion ( owner name number callback
                                  &optional until
                                  &key username auth host forge
                                  headers errorback)
  "Asynchronously fetch forge data about the specified discussion.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub-query (ghub--graphql-prepare-query
               ghub-fetch-repository
               `(repository discussions (discussion . ,number)))
              `((owner . ,owner)
                (name  . ,name))
              :until     until
              :narrow    '(repository discussion)
              :username  username
              :auth      auth
              :host      host
              :forge     forge
              :headers   headers
              :callback  callback
              :errorback errorback))

(cl-defun ghub-fetch-issue ( owner name number callback
                             &optional until
                             &key username auth host forge
                             headers paginate errorback)
  "Asynchronously fetch forge data about the specified issue.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub-query (ghub--graphql-narrow-query
               ghub-fetch-repository
               `(repository issues (issue . ,number)))
              `((owner . ,owner)
                (name  . ,name))
              :until     until
              :narrow    '(repository issue)
              :username  username
              :auth      auth
              :host      host
              :forge     forge
              :headers   headers
              :paginate  paginate
              :callback  callback
              :errorback errorback))

(cl-defun ghub-fetch-pullreq ( owner name number callback
                               &optional until
                               &key username auth host forge
                               headers paginate errorback)
  "Asynchronously fetch forge data about the specified pull-request.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub-query (ghub--graphql-narrow-query
               ghub-fetch-repository
               `(repository pullRequests (pullRequest . ,number)))
              `((owner . ,owner)
                (name  . ,name))
              :until     until
              :narrow    '(repository pullRequest)
              :username  username
              :auth      auth
              :host      host
              :forge     forge
              :headers   headers
              :paginate  paginate
              :callback  callback
              :errorback errorback))

(cl-defun ghub-fetch-review-threads ( owner name number callback
                                      &optional until
                                      &key username auth host forge
                                      headers paginate errorback)
  "Asynchronously fetch forge data about the review threads from a pull-request.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub-query (ghub--graphql-narrow-query
               ghub-fetch-repository-review-threads
               `(repository pullRequests (pullRequest . ,number)))
              `((owner . ,owner)
                (name  . ,name))
              :until     until
              :narrow    '(repository pullRequest)
              :username  username
              :auth      auth
              :host      host
              :forge     forge
              :headers   headers
              :paginate  paginate
              :callback  callback
              :errorback errorback))

;;; _
(provide 'ghub-legacy)
;;; ghub-legacy.el ends here
