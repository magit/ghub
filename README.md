Minuscule client for the Github API
===================================

This library just provides the HTTP verbs.  Instead of wrapping
every resource, I recommend https://developer.github.com/v3.
Due to the lack of doc-strings, I also recommend having a quick
look at the source, which is quite trivial.

Initial configuration
---------------------

```shell
$ git config github.user <username>
$ emacs ~/.authinfo.gpg
```

```
# -*- epa-file-encrypt-to: ("A.U.Thor@example.com") -*-
machine github.com login <login> password <token>
```

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

Github Enterprise support
-------------------------

* Initial configuration:

  ```shell
  $ git config example_com.user employee
  $ emacs ~/.authinfo.gpg
  # -*- epa-file-encrypt-to: ("employee@example.com") -*-
  machine example.com login employee password <token>
  ```

* Making a request:

  ```lisp
  (let ((ghub-instance "example.com")
        (ghub-base-url "https://example.com/api/v3"))
    (ghub-get "/users/example/repos"))
  ```

Alternatives
------------

If you like this, then you might also like `glab.el`; a minuscule
client for the Gitlab API.  See https://gitlab.com/tarsius/glab.

If you don't like this, then you might instead like `gh.el`; a big
client for the Github API.  See https://github.com/sigma/gh.el.

If you would like to use `ghub.el`, but also want dedicated
functions for each API endpoint, then you can create those using
`apiwrap.el`.  See https://github.com/vermiculus/apiwrap.el.
