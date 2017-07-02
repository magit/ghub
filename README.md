Minuscule client for the Github API
===================================

This library just provides the HTTP methods.
See https://developer.github.com/v3 for valid requests.

Initial configuration
---------------------

```shell
$ git config --global github.user <username>
$ emacs ~/.authinfo.gpg
```

```
# -*- epa-file-encrypt-to: ("A.U.Thor@example.com") -*-
machine api.github.com login <login> password <token>
```

To acquire a token, go to https://github.com/settings/tokens.  Note
that currently the same token is shared by all Emacs packages that
use `ghub.el`.

Usage examples
--------------

* Getting details about a repository:

  ```lisp
  (ghub-get "/repos/tarsius/ghub")
  ```

* Listing names of all repositories of a user:

  ```lisp
  (--keep (cdr (assq 'name it))
          (let ((ghub-unpaginate t))
             (ghub-get "/users/tarsius/repos")))
  ```

* Making an unauthenticated request:

  ```lisp
  (let ((ghub-authenticate nil))
    (ghub-get "/orgs/magit/repos"))
  ```

* Making a request using basic authentication:

  ```lisp
  (let ((ghub-authenticate 'basic))
    (ghub-get "/orgs/magit/repos"))
  ```


Github Enterprise support
-------------------------

* Initial configuration:

  ```shell
  $ cd /path/to/repository
  $ git config eg_example_com.user employee
  $ emacs ~/.authinfo.gpg
  ```

  ```
  # -*- epa-file-encrypt-to: ("employee@example.com") -*-
  machine gh.example.com login employee password <token>
  ```

Note that unlike for Github.com, which uses `github.user`, the Git
variable used to store the username for an Enterprise instance is
named `HOST.user`, where HOST is the host part of the `URI`, with
dots replaced with underscores.

* Making a request:

  ```lisp
  (let ((ghub-base-url "https://example.com/api/v3"))
    (ghub-get "/users/employee/repos"))
  ```

Alternatives
------------

If you like this, then you might never-the-less prefer `ghub+.el`;
a thick GitHub API client built on `ghub.el'.
See https://github.com/vermiculus/ghub-plus.

If you like this, then you might also like `glab.el`; a minuscule
client for the Gitlab API.  See https://gitlab.com/tarsius/glab.

If you don't like this, then you might instead like `gh.el`; a big
client for the Github API.  See https://github.com/sigma/gh.el.
