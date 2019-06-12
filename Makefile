prefix = /usr/local
bindir = $(prefix)/bin
libdir = $(prefix)/lib
sbcl = $(shell which sbcl)
src = src/*.asd src/*.lisp

-include config.mk

all: build/koas

build/koas: quicklisp/setup.lisp $(src)
	$(sbcl) --script make.lisp "$(libdir)/koas/"

quicklisp/install.lisp:
	mkdir -p quicklisp
	wget -O $@ "http://beta.quicklisp.org/quicklisp.lisp"

quicklisp/setup.lisp: quicklisp/install.lisp
	$(sbcl) --noinform --no-sysinit --no-userinit --non-interactive \
		--load asdf.conf \
		--load quicklisp/install.lisp \
		--eval '(quicklisp-quickstart:install :path "quicklisp/")'

config.mk:
	@echo "bindir = $(bindir)" > $@
	@echo "libdir = $(libdir)" >> $@
	@echo "sbcl = $(sbcl)" >> $@
	@cat $@

install:
	install -d -m 755 "$(bindir)" "$(libdir)/koas"
	install -m 755 build/koas "$(bindir)"
	install -m 644 build/src/koas.asd "$(libdir)/koas"
	install -m 644 build/src/koas--all-systems.fasl "$(libdir)/koas"

uninstall:
	rm -f -- "$(bindir)/koas"
	rm -fr -- "$(libdir)/koas"

clean:
	rm -fr build

distclean: clean
	rm -fr quicklisp
	rm -f config.mk

.PHONY: all install uninstall clean distclean
