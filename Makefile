# Variables
COQ_MAKEFILE ?= coq_makefile
COQTOP       ?= coqtop
DKCHECK      ?= dkcheck
DKDEP        ?= dkdep
VERBOSE      ?=

CAMLFLAGS="-bin-annot -annot"

.PHONY: all plugin install uninstall test clean fullclean debug_default debug_readable debug_ac debug_named debug_poly

all: .merlin plugin debug_default

plugin: CoqMakefile
	make -f CoqMakefile VERBOSE=$(VERBOSE) - all

install: CoqMakefile plugin
	make -f CoqMakefile - install

uninstall: CoqMakefile
	make -f CoqMakefile - uninstall

.merlin: CoqMakefile
	make -f CoqMakefile .merlin

test: plugin
	make -C test clean
	sh encodings/gen.sh original
	cp encodings/_build/Coq.dk test/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"default\"\.' test/Test.v
	make -C test

debug_default: plugin
	make -C debug clean
	sh encodings/gen.sh original
	cp encodings/_build/Coq.dk debug/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"default\"\.' debug/Debug.v
	make -C debug

debug_readable: plugin
	make -C debug clean
	sh encodings/gen.sh original short
	cp encodings/_build/C.dk debug/C.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"readable\"\.' debug/Debug.v
	make -C debug

# Not implemented yet
debug_ac: plugin
	make -C debug clean
	sh encodings/gen.sh ac
	cp encodings/_build/Coq.dk debug/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"AC\"\.' debug/Debug.v 
	make -C debug

debug_named: plugin
	make -C debug clean
	sh encodings/gen.sh original
	cp encodings/_build/Coq.dk debug/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"named\"\.' debug/Debug.v 
	make -C debug

debug_poly: plugin
	make -C debug clean
	sh encodings/gen.sh constructors short
	cp encodings/_build/C.dk debug/C.dk
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
