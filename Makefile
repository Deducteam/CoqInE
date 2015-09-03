COQ_MAKEFILE = coq_makefile
COQTOP = coqtop
DKCHECK = dkcheck

MAKEFILE_PLUGIN = Makefile.plugin

.PHONY: plugin install test clean

plugin: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f -

install: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - install

test: plugin 
	make -C test

dedukti/Coq.dko: dedukti/Coq.dk
	cd dedukti && $(DKCHECK) -e -nl Coq.dk

clean: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - clean
	make -C test clean
	rm -rf dedukti/Coq.dko

