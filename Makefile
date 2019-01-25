# Variables
COQ_MAKEFILE ?= coq_makefile
COQTOP       ?= coqtop
DKCHECK      ?= dkcheck
DKDEP        ?= dkdep
VERBOSE      ?=

CAMLFLAGS="-bin-annot -annot"

.PHONY: all plugin install uninstall test clean fullclean debug_default debug_readable debug_ac debug_named debug_poly

all: .merlin plugin debug_cast

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
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"original_cast\"\.' test/Test.v
	make -C test

debug_default: plugin
	make -C debug clean
	sh encodings/gen.sh original
	cp encodings/_build/Coq.dk debug/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"original\"\.' debug/Debug.v
	make -C debug

debug_readable: plugin
	make -C debug clean
	sh encodings/gen.sh original short
	cp encodings/_build/C.dk debug/C.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"readable original\"\.' debug/Debug.v
	make -C debug

debug_cast: plugin
	make -C debug clean
	sh encodings/gen.sh original_cast
	cp encodings/_build/Coq.dk debug/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"named original_cast\"\.' debug/Debug.v
	make -C debug

debug_named: plugin
	make -C debug clean
	sh encodings/gen.sh original
	cp encodings/_build/Coq.dk debug/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"named original\"\.' debug/Debug.v 
	make -C debug

debug_poly: plugin
	make -C debug clean
	sh encodings/gen.sh constructors short
	cp encodings/_build/C.dk debug/C.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"readable polymorph\"\.' debug/Debug.v 
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
