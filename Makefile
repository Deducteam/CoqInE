VERBOSE?=

COQ_MAKEFILE = coq_makefile
COQTOP = coqtop

DKFOLDER = /home/gferey/git/dedukti/master
DKCHECK = $(DKFOLDER)/dkcheck.native
DKDEP = $(DKFOLDER)/dkdep.native

CAMLFLAGS="-bin-annot -annot"

.PHONY: all plugin install test debug clean fullclean

all: .merlin plugin debug_default

plugin: CoqMakefile
	make -f CoqMakefile VERBOSE=$(VERBOSE) - all

install: CoqMakefile
	make -f CoqMakefile - install

uninstall: CoqMakefile
	make -f CoqMakefile - uninstall

.merlin: CoqMakefile
	make -f CoqMakefile .merlin

test: plugin
	make -C test

debug_default: plugin
	make -C debug clean
	cp encodings/0_original.dk debug/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"default\"\.' debug/Debug.v 
	make -C debug

debug_readable: plugin
	make -C debug clean
	cp encodings/0_original_short.dk debug/C.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"readable\"\.' debug/Debug.v 
	make -C debug

# Not implemented yet
debug_ac: plugin
	make -C debug clean
	cp encodings/0_ac.dk debug/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"AC\"\.' debug/Debug.v 
	make -C debug

debug_named: plugin
	make -C debug clean
	cp encodings/0_original.dk debug/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"named\"\.' debug/Debug.v 
	make -C debug

debug_poly: plugin
	make -C debug clean
	cp encodings/1b_cumul_const_short.dk debug/C.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"polymorph\"\.' debug/Debug.v 
	make -C debug

clean: CoqMakefile
	make -f CoqMakefile - clean
	make -C test clean
	make -C debug clean
	rm CoqMakefile

fullclean: clean
	rm src/*.cmt
	rm src/*.cmti
	rm src/*.annot

CoqMakefile: Make
	$(COQ_MAKEFILE) -f Make -o CoqMakefile
	echo "COQMF_CAMLFLAGS+=-annot -bin-annot -g" >> CoqMakefile.conf
