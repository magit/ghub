-include ../config.mk
include ../default.mk

lisp: $(ELCS) autoloads check-declare

autoloads: $(PKG)-autoloads.el

ghub.elc:
gsexp.elc:
ghub-graphql.elc: ghub.elc gsexp.elc
ghub-legacy.elc: ghub.elc gsexp.elc ghub-graphql.elc

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --funcall batch-byte-compile $<

check-declare:
	@printf " Checking function declarations\n"
	@$(EMACS_BATCH) --eval "(check-declare-directory default-directory)"

CLEAN = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf " Cleaning lisp/*...\n"
	@rm -rf $(CLEAN)

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS_BATCH) --eval "\
(let ((inhibit-message t))\
  (loaddefs-generate\
   default-directory \"$@\" nil\
   (prin1-to-string\
    '(add-to-list 'load-path\
                  (or (and #$$ (directory-file-name (file-name-directory #$$)))\
                      (car load-path)))))\
   nil t)"
