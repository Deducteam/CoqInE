# Variables
COQ_MAKEFILE ?= coq_makefile
COQTOP       ?= coqtop
DKCHECK      ?= dkcheck
DKDEP        ?= dkdep
VERBOSE      ?=

CAMLFLAGS="-bin-annot -annot"

RUNDIR=run
TESTDIR=$(RUNDIR)/test
GEOCOQDIR=$(RUNDIR)/geocoq
DEBUGDIR=$(RUNDIR)/debug

.PHONY: all plugin install uninstall clean fullclean

all: .merlin plugin debug_cast

plugin: CoqMakefile
	make -f CoqMakefile VERBOSE=$(VERBOSE) - all

install: CoqMakefile plugin
	make -f CoqMakefile - install

uninstall: CoqMakefile
	make -f CoqMakefile - uninstall

.merlin: CoqMakefile
	make -f CoqMakefile .merlin

clean: CoqMakefile
	make -f CoqMakefile - clean
	make -C $(TESTDIR)   clean
	make -C $(GEOCOQDIR) clean
	make -C $(DEBUGDIR)  clean
	rm CoqMakefile

fullclean: clean
	rm src/*.cmt
	rm src/*.cmti
	rm src/*.annot

CoqMakefile: Make
	$(COQ_MAKEFILE) -f Make -o CoqMakefile
	echo "COQMF_CAMLFLAGS+=-annot -bin-annot -g" >> CoqMakefile.conf



# Targets for several libraries to translate

.PHONY: test
test: plugin
	make -C $(TESTDIR) clean
	sh encodings/gen.sh original
	cp encodings/_build/Coq.dk $(TESTDIR)/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"original_cast\"\.' $(TESTDIR)/main.v
	make -C $(TESTDIR)

# This target requires geocoq. Set correct path in run/geocoq/Makefile.
.PHONY: geocoq
geocoq: plugin
	make -C $(GEOCOQDIR) clean
	sh encodings/gen.sh original_cast short
	cp encodings/_build/C.dk $(GEOCOQDIR)/C.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"readable original_cast\"\.' $(GEOCOQDIR)/main.v
	make -C $(GEOCOQDIR)

.PHONY: debug
debug: debug_cast

.PHONY: debug_default
debug_default: FLAGS:=original
debug_default: plugin
	make -C $(DEBUGDIR) clean
	sh encodings/gen.sh original
	cp encodings/_build/Coq.dk $(DEBUGDIR)/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"original\"\.' $(DEBUGDIR)/main.v
	make -C $(DEBUGDIR)

.PHONY: debug_readable
debug_readable: plugin
	make -C $(DEBUGDIR) clean
	sh encodings/gen.sh original short
	cp encodings/_build/C.dk $(DEBUGDIR)/C.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"readable original\"\.' $(DEBUGDIR)/main.v
	make -C $(DEBUGDIR)

.PHONY: debug_named_cast
debug_named_cast: plugin
	make -C $(DEBUGDIR) clean
	sh encodings/gen.sh original_cast
	cp encodings/_build/Coq.dk $(DEBUGDIR)/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"named original_cast\"\.' $(DEBUGDIR)/main.v
	make -C $(DEBUGDIR)

.PHONY: debug_cast
debug_cast: plugin
	make -C $(DEBUGDIR) clean
	sh encodings/gen.sh original_cast short
	cp encodings/_build/C.dk $(DEBUGDIR)/C.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"readable original_cast\"\.' $(DEBUGDIR)/main.v
	make -C $(DEBUGDIR)

.PHONY: debug_named
debug_named: plugin
	make -C $(DEBUGDIR) clean
	sh encodings/gen.sh original
	cp encodings/_build/Coq.dk $(DEBUGDIR)/Coq.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"named original\"\.' $(DEBUGDIR)/main.v 
	make -C $(DEBUGDIR)

.PHONY: debug_poly
debug_poly: plugin
	make -C $(DEBUGDIR) clean
	sh encodings/gen.sh constructors short
	cp encodings/_build/C.dk $(DEBUGDIR)/C.dk
	sed -i -e '/Encoding/c\Dedukti Set Encoding \"readable polymorph\"\.' $(DEBUGDIR)/main.v
	make -C $(DEBUGDIR)
