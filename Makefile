bindir = /usr/local/bin
sbcl = sbcl
src = src/*.asd src/*.lisp

-include config.mk

koas: quicklisp/setup.lisp $(src) versio.txt
	$(sbcl) --script make.lisp

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
	@echo "sbcl = $(sbcl)" >> $@
	@cat $@

versio.txt:
	v=$$(git describe --always --dirty) || v=tuntematon; echo "$$v" > $@

install:
	install -d -m 755 "$(bindir)"
	install -m 755 koas "$(bindir)"

uninstall:
	rm -f -- "$(bindir)/koas"

clean:
	rm -f koas versio.txt
	rm -fr build

distclean: clean
	rm -fr quicklisp
	rm -f config.mk

.PHONY: install uninstall clean distclean
