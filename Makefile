bindir = $(HOME)/bin
IMAGE = arviointi
MAKEIMG = make-image.lisp
SBCL = sbcl --script
LISPFILES = arviointi.asd arviointi.lisp script-lib.lisp

# all: $(IMAGE)

$(IMAGE): $(MAKEIMG) $(ASD) $(LISPFILES)
	@$(SBCL) $(MAKEIMG) $(IMAGE)

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
