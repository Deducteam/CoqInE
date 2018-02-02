
VERBOSE?=

COQ_MAKEFILE = coq_makefile
COQTOP = coqtop
DKCHECK = dkcheck

.PHONY: plugin install test clean

plugin: CoqMakefile
	make -f CoqMakefile VERBOSE=$(VERBOSE) -
	rm CoqMakefile

install: CoqMakefile
	make -f CoqMakefile -
	rm CoqMakefile

test: plugin 
	make -C test

debug: plugin 
	make -C test debug

dedukti/Coq.dko: dedukti/Coq.dk
	cd dedukti && $(DKCHECK) -e -nl Coq.dk

clean: CoqMakefile
	make -f CoqMakefile - clean
	make -C test clean
	rm -rf dedukti/Coq.dko
	rm -rf src/*.cm*
	rm -rf src/*.d
	rm -rf src/*.a
	rm -rf src/*.o
	rm -rf src/*.vo
	rm -rf src/*.a*
	rm -rf src/*.glob
	rm CoqMakefile

CoqMakefile: Make
	$(COQ_MAKEFILE) -f Make > CoqMakefile
