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

all: check-version .merlin plugin
	make test_fix
	make test_poly

long: all debug_fix debug_poly

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

.PHONY: orig_geocoq
orig_geocoq: RUNDIR:=$(RUN_GEOCOQ_ORIG_DIR)
orig_geocoq: COQINE_FLAGS:=polymorph
orig_geocoq: run

.PHONY: geocoq
geocoq: RUNDIR:=$(RUN_GEOCOQ_DIR)
geocoq: COQINE_FLAGS:=polymorph
geocoq: run



.PHONY: test_eta
test_eta: ENCODING:=predicates_eta/C
test_eta: COQINE_FLAGS:=polymorph
test_eta: test

.PHONY: test_fix
test_fix: ENCODING:=predicates_eta_fix/C
test_fix: COQINE_FLAGS:=polymorph
test_fix: test

.PHONY: test_poly
test_poly: ENCODING:=fullcodes_eta_fix/C
test_poly: COQINE_FLAGS:=polymorph
test_poly: test


.PHONY: debug_eta
debug_eta: ENCODING:=predicates_eta/C
debug_eta: COQINE_FLAGS:=polymorph
debug_eta: debug

.PHONY: debug_fix
debug_fix: ENCODING:=predicates_eta_fix/C
debug_fix: COQINE_FLAGS:=polymorph
debug_fix: debug

.PHONY: debug_poly
debug_poly: ENCODING:=fullcodes_eta_fix/C
debug_poly: COQINE_FLAGS:=polymorph
debug_poly: debug

.PHONY: debug_default
debug_default: ENCODING:=original/Coq
debug_default: COQINE_FLAGS:=
debug_default: debug

.PHONY: debug_readable
debug_readable: ENCODING:=original/C
debug_readable: COQINE_FLAGS:=
debug_readable: debug

.PHONY: debug_named_cast
debug_named_cast: ENCODING:=original_cast/Coq
debug_named_cast: COQINE_FLAGS:=
debug_named_cast: debug

.PHONY: debug_cast
debug_cast: ENCODING:=original_cast/C
debug_cast: COQINE_FLAGS:=
debug_cast: debug

.PHONY: debug_template
debug_template: ENCODING:=original_cast/C
debug_template: COQINE_FLAGS:=template
debug_template: debug

.PHONY: debug_named
debug_named: ENCODING:=original/Coq
debug_named: COQINE_FLAGS:=named
debug_named: debug


.PHONY: mathcomp_lift
mathcomp_lift: ENCODING:=lift_predicates/C
mathcomp_lift: COQINE_FLAGS:=
mathcomp_lift: mathcomp

.PHONY: mathcomp_debug
mathcomp_debug: ENCODING:=predicates/C
mathcomp_debug: COQINE_FLAGS:=polymorph
mathcomp_debug: mathcomp


# These targets require GeoCoq. Set correct path in run/geocoq/Makefile.
.PHONY: orig_geocoq_long
orig_geocoq_long: ENCODING:=predicates/Coq
orig_geocoq_long: orig_geocoq

.PHONY: orig_geocoq_short
orig_geocoq_short: ENCODING:=predicates/C
orig_geocoq_short: orig_geocoq


.PHONY: geocoq_long
geocoq_long: ENCODING:= predicates/Coq
geocoq_long: geocoq

.PHONY: geocoq_short
geocoq_short: ENCODING:= predicates/C
geocoq_short: geocoq
