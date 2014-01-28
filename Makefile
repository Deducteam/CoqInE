COQ_MAKEFILE = coq_makefile
COQTOP = coqtop
DKCHECK = dkcheck

MAKEFILE_PLUGIN = Makefile.plugin

.PHONY: plugin install test clean

plugin:
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f -

install:
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - install

test: plugin Coq.dko
	make -C test

Coq.dko: dedukti/Coq.dk
	$(DKCHECK) -e -unsafe dedukti/Coq.dk

clean:
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - clean
	make -C test clean
	rm -rf Coq.dko

