sbcl = sbcl
bindir = $(HOME)/bin

src = koas.asd koas.lisp

koas: quicklisp/setup.lisp $(src)
	$(sbcl) --script make-image.lisp

quicklisp.lisp:
	wget -O $@ "http://beta.quicklisp.org/quicklisp.lisp"

quicklisp/setup.lisp: quicklisp.lisp
	$(sbcl) --noinform --no-sysinit --no-userinit --non-interactive \
		--load quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "quicklisp/")'

install:
	install -d -m 755 $(bindir)
	install -m 755 koas $(bindir)

clean:
	rm -f koas

clean-all: clean
	rm -fr quicklisp
	rm -f quicklisp.lisp

.PHONY: install clean clean-all
