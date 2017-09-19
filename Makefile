
VERBOSE=0

COQ_MAKEFILE = coq_makefile VERBOSE = $(VERBOSE)
COQTOP = coqtop
DKCHECK = dkcheck


MAKEFILE_PLUGIN = Makefile.plugin
MAKEFILE_GENERATED = Makefile.generated

.PHONY: plugin install test clean

plugin: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - all .merlin

install: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f -

test: plugin 
	make -C test

debug: plugin 
	make -C test debug

dedukti/Coq.dko: dedukti/Coq.dk
	cd dedukti && $(DKCHECK) -e -nl Coq.dk

clean: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - clean
	make -C test clean
	rm -rf dedukti/Coq.dko

