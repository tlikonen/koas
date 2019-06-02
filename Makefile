sbcl = sbcl
bindir = $(HOME)/bin
src = koas.asd koas.lisp pathconv.lisp

koas: quicklisp/setup.lisp $(src)
	$(sbcl) --script make-quickload.lisp
	$(sbcl) --script make-image.lisp

quicklisp/install.lisp:
	mkdir -p quicklisp
	wget -O $@ "http://beta.quicklisp.org/quicklisp.lisp"

quicklisp/setup.lisp: quicklisp/install.lisp
	$(sbcl) --noinform --no-sysinit --no-userinit --non-interactive \
		--load quicklisp/install.lisp \
		--eval '(require "asdf")' \
		--eval '(asdf:disable-output-translations)' \
		--eval '(quicklisp-quickstart:install :path "quicklisp/")'

install:
	install -d -m 755 $(bindir)
	install -m 755 koas $(bindir)

clean:
	rm -f koas *.fasl

clean-all: clean
	rm -fr quicklisp

.PHONY: install clean clean-all
