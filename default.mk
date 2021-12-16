TOP := $(dir $(lastword $(MAKEFILE_LIST)))

PKG   = ghub

ELS   = $(PKG).el
ELS  += gsexp.el
ELS  += $(PKG)-graphql.el
ELS  += glab.el
ELS  += gtea.el
ELS  += gogs.el
ELS  += buck.el
ELCS  = $(ELS:.el=.elc)

DEPS  = treepy

VERSION ?= $(shell test -e $(TOP).git && git describe --tags --abbrev=0 | cut -c2-)

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../../,$(DEPS))
LOAD_PATH  += -L .

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = -L ../../org/lisp
ORG_LOAD_PATH += -L ../../ox-texinfo+
endif

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

STATS_DIR ?= $(TOP)docs/stats
