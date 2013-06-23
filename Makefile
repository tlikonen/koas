bindir = $(HOME)/bin
IMAGE = koas
SYSTEM = koas
MAKEIMG = make-image.lisp
SBCL = sbcl --script
LISPFILES = koas.asd koas.lisp script-lib.lisp

$(IMAGE): $(MAKEIMG) $(LISPFILES)
	@$(SBCL) $(MAKEIMG) $(SYSTEM) $(IMAGE)

install: $(IMAGE)
	install -d -- $(bindir)
	install -m 755 -- $(IMAGE) $(bindir)

uninstall:
	rm -f -- $(bindir)/$(IMAGE)

clean:
	rm -f -- $(IMAGE)

clean-all: clean
	rm -fr -- quicklisp
	rm -f -- quicklisp.lisp

.PHONY: all clean clean-all install uninstall
