COQ_MAKEFILE = coq_makefile
COQTOP = coqtop

MAKEFILE_PLUGIN = Makefile.plugin
MAKEFILE_TEST = Makefile.test

.PHONY: plugin install test clean

plugin:
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f -

install:
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - install

test: plugin
	$(COQ_MAKEFILE) -f $(MAKEFILE_TEST) | make -f -
	$(COQTOP) -R src Dedukti -I test -l test/Test

clean:
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - clean
	$(COQ_MAKEFILE) -f $(MAKEFILE_TEST) | make -f - clean
	rm -rf test/*.dk

