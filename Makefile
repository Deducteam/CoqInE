VERBOSE?=

CAMLFLAGS="-bin-annot -annot"

COQ_MAKEFILE = coq_makefile
COQTOP = coqtop

.PHONY: all plugin install test debug clean

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

clean: CoqMakefile
	make -f CoqMakefile - clean
	make -C test clean
	make -C debug clean
	rm CoqMakefile
	rm src/*.cmt
	rm src/*.cmti
	rm src/*.annot

CoqMakefile: Make
	$(COQ_MAKEFILE) -f Make -o CoqMakefile
	echo "COQMF_CAMLFLAGS+=-annot -bin-annot -g" >> CoqMakefile.conf
