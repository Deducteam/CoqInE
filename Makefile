
VERBOSE?=

COQ_MAKEFILE = coq_makefile
COQTOP = coqtop

DKFOLDER = /home/gaspi/github/dedukti/acu
DKCHECK = $(DKFOLDER)/dkcheck.native
DKDEP = $(DKFOLDER)/dkdep.native

MAKEFILE_PLUGIN = Makefile.plugin
MAKEFILE_GENERATED = Makefile.generated

.PHONY: plugin install test clean

plugin: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - all .merlin VERBOSE=$(VERBOSE)

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

