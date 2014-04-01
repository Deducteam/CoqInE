COQ_MAKEFILE = coq_makefile
COQTOP = coqtop
DKCHECK = dkcheck

MAKEFILE_PLUGIN = Makefile.plugin

.PHONY: plugin install test clean

plugin: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f -

install: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - install

test: plugin Coq.dko
	make -C test

Coq.dko: dedukti/Coq.dk
	$(DKCHECK) -e dedukti/Coq.dk

clean: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - clean
	make -C test clean
	rm -rf Coq.dko

