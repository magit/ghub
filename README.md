Minuscule client for the Github API
===================================

Initial configuration
---------------------

```shell
$ git config github.user <username>
$ emacs ~/.authinfo.gpg
```

```
# -*- epa-file-encrypt-to: ("A.U.Thor@example.com") -*-
machine api.github.com login <login> password <token>
```

Usage examples
--------------

* Get details about a repository:

  ```lisp
  (ghub-get "/repos/tarsius/ghub")
  ```

* List names of all repositories of a user:

  ```lisp
  (--keep (cdr (assq 'name it))
          (let ((ghub-unpaginate t))
             (ghub-get "/users/tarsius/repos")))
  ```

Links
-----

This library just provides the basic verbs.  Instead of wrapping
every resource, I recommend https://developer.github.com/v3.  Due
to the lack of doc-strings, I also recommend having a quick look
at the source, which is quite trivial.

If you like this, then you might also like `glab.el`; a minuscule
client for the Gitlab API.  See https://gitlab.com/tarsius/glab.

If you don't like this, then you might instead like `gh.el`; a big
client for the Github API.  See https://github.com/sigma/gh.el.
