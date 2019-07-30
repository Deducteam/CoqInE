# Variables
COQ_MAKEFILE ?= coq_makefile
COQTOP       ?= coqtop
DKCHECK      ?= dkcheck
DKDEP        ?= dkdep
VERBOSE      ?=

CAMLFLAGS="-bin-annot -annot"

RUNDIR=run
RUN_TESTS_DIR=$(RUNDIR)/test
RUN_GEOCOQ_DIR=$(RUNDIR)/geocoq
RUN_GEOCOQ_ORIG_DIR=$(RUNDIR)/geocoq_orig
RUN_DEBUG_DIR=$(RUNDIR)/debug
RUN_MATHCOMP_DIR=$(RUNDIR)/mathcomp

COQ_VERSION   := $(shell $(COQTOP) -print-version)
CHECK_VERSION := $(shell $(COQTOP) -print-version | grep "8\.8\.*")

.PHONY: all plugin install uninstall clean fullclean

all: check-version .merlin plugin test_fix

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
	make -C encodings - clean
	make -f CoqMakefile - clean
	make -C $(RUN_TESTS_DIR)       clean
	make -C $(RUN_DEBUG_DIR)       clean
	make -C $(RUN_GEOCOQ_DIR)      clean
	make -C $(RUN_GEOCOQ_ORIG_DIR) clean
	make -C $(RUN_MATHCOMP_DIR)    clean
	rm CoqMakefile

fullclean: clean
	rm src/*.cmt
	rm src/*.cmti
	rm src/*.annot

CoqMakefile: Make
	$(COQ_MAKEFILE) -f Make -o CoqMakefile
	echo "COQMF_CAMLFLAGS+=-annot -bin-annot -g" >> CoqMakefile.conf



# Targets for several libraries to translate

ENCODING ?= original_cast/Coq.dk # Configuration for the encoding generation
COQINE_FLAGS   ?= original_cast # Configuration for the translator

.PHONY: run
run: plugin
	make -C $(RUNDIR) clean
	make -C encodings clean _build/$(ENCODING).dk
	make -C encodings clean _build/$(ENCODING).config
	cp encodings/_build/$(ENCODING).dk     $(RUNDIR)/
	cp encodings/_build/$(ENCODING).config $(RUNDIR)/config.v
	sed -i -e "/Encoding/c\Dedukti Set Encoding \"$(COQINE_FLAGS)\"." $(RUNDIR)/main.v
	make -C $(RUNDIR)

.PHONY: test
test: RUNDIR:=$(RUN_TESTS_DIR)
test: run

.PHONY: debug
debug: RUNDIR:=$(RUN_DEBUG_DIR)
debug: run

.PHONY: mathcomp
mathcomp: RUNDIR:=$(RUN_MATHCOMP_DIR)
mathcomp: run


.PHONY: test_universo
test_universo: ENCODING:=predicates_eta/C
test_universo: COQINE_FLAGS:=universo readable
test_universo: test

.PHONY: test_fix
test_fix: ENCODING:=predicates_eta_fix/C
test_fix: COQINE_FLAGS:=universo readable fix
test_fix: test

.PHONY: test_poly
test_poly: ENCODING:=fullcodes_eta_fix/C
test_poly: COQINE_FLAGS:=polymorph readable fix
test_poly: test


.PHONY: debug_universo
debug_universo: ENCODING:=predicates/C
debug_universo: COQINE_FLAGS:=universo readable
debug_universo: debug

.PHONY: debug_fix
debug_fix: ENCODING:=predicates_eta_fix/C
debug_fix: COQINE_FLAGS:=universo readable fix
debug_fix: debug

.PHONY: debug_poly
debug_poly: ENCODING:=fullcodes_eta_fix/C
debug_poly: COQINE_FLAGS:=polymorph readable fix
debug_poly: debug

.PHONY: debug_default
debug_default: ENCODING:=original/Coq
debug_default: COQINE_FLAGS:=original
debug_default: debug

.PHONY: debug_readable
debug_readable: ENCODING:=original/C
debug_readable: COQINE_FLAGS:=original readable
debug_readable: debug

.PHONY: debug_named_cast
debug_named_cast: ENCODING:=original_cast/Coq
debug_named_cast: COQINE_FLAGS:=original_cast named
debug_named_cast: debug

.PHONY: debug_cast
debug_cast: ENCODING:=original_cast/C
debug_cast: COQINE_FLAGS:=original_cast readable
debug_cast: debug

.PHONY: debug_template
debug_template: ENCODING:=original_cast/C
debug_template: COQINE_FLAGS:=template_cast readable
debug_template: debug

.PHONY: debug_named
debug_named: ENCODING:=original/Coq
debug_named: COQINE_FLAGS:=original named
debug_named: debug


.PHONY: mathcomp_lift
mathcomp_lift: ENCODING:=lift_predicates/C
mathcomp_lift: COQINE_FLAGS:=lift_priv readable
mathcomp_lift: mathcomp

.PHONY: mathcomp_debug
mathcomp_debug: ENCODING:=predicates/C
mathcomp_debug: COQINE_FLAGS:=universo readable
mathcomp_debug: mathcomp


# These targets require GeoCoq. Set correct path in run/geocoq/Makefile.
.PHONY: geocoq_orig_universo
geocoq_orig_universo: ENCODING:= predicates/Coq
geocoq_orig_universo: COQINE_FLAGS  := universo
geocoq_orig_universo: RUNDIR        := $(RUN_GEOCOQ_ORIG_DIR)
geocoq_orig_universo: run

.PHONY: geocoq_orig
geocoq_orig: ENCODING:= predicates/C
geocoq_orig: COQINE_FLAGS  := universo readable
geocoq_orig: RUNDIR        := $(RUN_GEOCOQ_ORIG_DIR)
geocoq_orig: run


.PHONY: geocoq_universo
geocoq_universo: ENCODING:= predicates/Coq
geocoq_universo: COQINE_FLAGS  := universo
geocoq_universo: RUNDIR        := $(RUN_GEOCOQ_DIR)
geocoq_universo: run

.PHONY: geocoq
geocoq: ENCODING:= predicates/C
geocoq: COQINE_FLAGS  := universo readable
geocoq: RUNDIR        := $(RUN_GEOCOQ_DIR)
geocoq: run
