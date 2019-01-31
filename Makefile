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

COQ_VERSION   := $(shell $(COQTOP) -print-version)
CHECK_VERSION := $(shell $(COQTOP) -print-version | grep "8\.8\.*")

.PHONY: all plugin install uninstall clean fullclean

all: check-version .merlin plugin debug_cast

check-version:
ifeq ("$(CHECK_VERSION)","")
	$(warning "Incorrect Coq version !")
	$(warning "Found: $(COQ_VERSION).")
	$(warning "Expected: 8.8.x")
	$(error "To ignore this, use:  make CHECK_VERSION=ignore")
endif

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

ENCODING_FLAGS ?= original_cast # Configuration for the encoding generation
COQINE_FLAGS   ?= original_cast # Configuration for the translator

.PHONY: run
run: plugin
	sh encodings/gen.sh $(ENCODING_FLAGS)
	make -C $(RUNDIR) clean
	cp encodings/_build/*.dk $(RUNDIR)/
	sed -i -e "/Encoding/c\Dedukti Set Encoding \"$(COQINE_FLAGS)\"." $(RUNDIR)/main.v
	make -C $(RUNDIR)

.PHONY: test
test: RUNDIR:=$(TESTDIR)
test: run

.PHONY: debug
debug: RUNDIR:=$(DEBUGDIR)
debug: run

# This target requires geocoq. Set correct path in run/geocoq/Makefile.
.PHONY: geocoq
geocoq: RUNDIR:=$(GEOCOQDIR)
geocoq: run



.PHONY: template_geocoq
template_geocoq: ENCODING_FLAGS:=original_cast short
template_geocoq: COQINE_FLAGS:=readable template_cast
template_geocoq: geocoq

.PHONY: debug_default
debug_default: ENCODING_FLAGS:=original
debug_default: COQINE_FLAGS:=original
debug_default: debug

.PHONY: debug_readable
debug_readable: ENCODING_FLAGS:=original short
debug_readable: COQINE_FLAGS:=readable original
debug_readable: debug

.PHONY: debug_named_cast
debug_named_cast: ENCODING_FLAGS:=original_cast
debug_named_cast: COQINE_FLAGS:=named original_cast
debug_named_cast: debug

.PHONY: debug_cast
debug_cast: ENCODING_FLAGS:=original_cast short
debug_cast: COQINE_FLAGS:=readable original_cast
debug_cast: debug

.PHONY: debug_template
debug_template: ENCODING_FLAGS:=original_cast short
debug_template: COQINE_FLAGS:=readable template_cast
debug_template: debug

.PHONY: debug_named
debug_named: ENCODING_FLAGS:=original
debug_named: COQINE_FLAGS:=named original
debug_named: debug

.PHONY: debug_named
debug_named: ENCODING_FLAGS:=original
debug_named: COQINE_FLAGS:=named original
debug_named: debug

.PHONY: debug_poly
debug_poly: ENCODING_FLAGS:=constructors short
debug_poly: COQINE_FLAGS:=readable polymorph
debug_poly: debug
