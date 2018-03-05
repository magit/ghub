Ghub.el — Miniscule client library for the GitHub API
=====================================================

Ghub is a library that provides basic support for using the GitHub
REST (v3) and GraphQL (v4) APIs from Emacs packages.  It abstracts
access to API resources using only a handful of functions that are
not resource-specific.

It also handles the creation, storage, and use of access tokens by using a
setup wizard to make it easier for users to get started and to reduce
the support burden imposed on package maintainers and comes with
a comprehensive manual to address the cases when things don't just
work as expected.

Ghub is intentionally limited to only provide these two essential
features—basic request functions and guided setup—to avoid being
too opinionated, which would hinder wide adoption.  It is assumed that
wide adoption would make life easier for users and maintainers alike,
because then all packages that talk to the GitHub API would be
configured the same way.

Please consult the [manual][manual-ghub] for more information.

Glab.el — Miniscule client library for the GitLab API
=====================================================

Glab is a library that provides basic support for using the GitLab API
from Emacs packages.  It abstracts access to API resources using only
a handful of functions that are not resource-specific.

This library is implemented on top of Ghub.  Unlike Ghub, Glab does
not support the guided creation of tokens because GitLab lacks the
features that would be necessary to implement that.  Users have to
create tokens through the web interface.  Instructions can be found
[here][manual-glab].

[manual-ghub]: https://magit.vc/manual/ghub
[manual-glab]: https://magit.vc/manual/ghub/Gitlab-Support.html
