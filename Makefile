
VERBOSE?=

COQ_MAKEFILE = coq_makefile
COQTOP = coqtop

DKFOLDER = /home/gaspi/github/dedukti/acu
DKCHECK = $(DKFOLDER)/dkcheck.native
DKDEP = $(DKFOLDER)/dkdep.native

.PHONY: all plugin install test debug clean fullclean

all: .merlin plugin test

plugin: CoqMakefile
	make -f CoqMakefile VERBOSE=$(VERBOSE) - all

install: CoqMakefile
	make -f CoqMakefile - install

.merlin: CoqMakefile
	make -f CoqMakefile .merlin

test: plugin
	make -C test

debug: plugin 
	make -C debug

dedukti/Coq.dko: dedukti/Coq.dk
	cd dedukti && $(DKCHECK) -e -nl Coq.dk

clean: CoqMakefile
	make -f CoqMakefile - clean
	make -C test clean
	make -C debug clean
	rm -rf dedukti/Coq.dko
	rm CoqMakefile

fullclean: clean
	rm src/*.cmt
	rm src/*.cmti

CoqMakefile: Make
	$(COQ_MAKEFILE) -f Make > CoqMakefile
