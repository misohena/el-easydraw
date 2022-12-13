PWD         ?= $(shell pwd)

ELS          = $(wildcard $(PWD)/*.el)
ELCS         = $(ELS:.el=.elc)

EMACS       := emacs
FIND        := rm -f

EMACFLAGS   := --batch -q --no-site-file -L $(PWD)
EMACSCMD     = $(EMACS) $(EMACFLAGS)


.PHONY: all
all: clean compile

.PHONY: clean
clean:
	$(RM) $(ELCS)

%.elc:
	$(EMACSCMD) --eval "(byte-compile-file \"$(*).el\" 0)"

.PHONY: compile
compile: $(ELCS)

.PHONY: install
install: compile
	$(EMACSCMD) \
		--eval "(require 'package)" \
		--eval "(package-install-file \"$(PWD)\")"
