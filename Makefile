bindir = $(HOME)/bin
IMAGE = koas
SYSTEM = koas
MAKEIMG = make-image.lisp
SBCL = sbcl
LISPFILES = koas.asd koas.lisp script-lib.lisp

$(IMAGE): $(MAKEIMG) $(LISPFILES)
	@$(SBCL) --script $(MAKEIMG) $(SYSTEM) $(IMAGE)

install: $(IMAGE)
	install -d -- $(bindir)
	install -m 755 -- $(IMAGE) $(bindir)

uninstall:
	rm -f -- $(bindir)/$(IMAGE)

clean:
	rm -f -- $(IMAGE) system-index.txt

clean-all: clean
	rm -fr -- quicklisp

.PHONY: clean clean-all install uninstall
