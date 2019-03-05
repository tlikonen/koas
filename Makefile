sbcl = sbcl
bindir = $(HOME)/bin

src = koas.asd koas.lisp
src-ql = $(patsubst %,quicklisp/local-projects/%,$(src))

koas: quicklisp/setup.lisp $(src-ql)
	$(sbcl) --script make-image.lisp

$(src-ql): quicklisp/local-projects/%: %
	cp $< $@

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
	rm -f koas quicklisp/local-projects/*

clean-all: clean
	rm -fr quicklisp
	rm -f quicklisp.lisp

.PHONY: install clean clean-all
