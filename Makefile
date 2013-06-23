bindir = $(HOME)/bin
IMAGE = arviointi
SYSTEM = arviointi
MAKEIMG = make-image.lisp
SBCL = sbcl --script
LISPFILES = arviointi.asd arviointi.lisp script-lib.lisp

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
