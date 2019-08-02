# Variables
COQ_MAKEFILE ?= coq_makefile
COQTOP       ?= coqtop
DKCHECK      ?= dkcheck
DKDEP        ?= dkdep
VERBOSE      ?=

CAMLFLAGS="-bin-annot -annot"

RUNDIR=run
RUN_GEOCOQ_DIR=$(RUNDIR)/geocoq
RUN_GEOCOQ_ORIG_DIR=$(RUNDIR)/geocoq_orig
RUN_MAIN_DIR=$(RUNDIR)/main
RUN_MATHCOMP_DIR=$(RUNDIR)/mathcomp

COQ_VERSION   := $(shell $(COQTOP) -print-version)
CHECK_VERSION := $(shell $(COQTOP) -print-version | grep "8\.8\.*")

define MANUAL

---------------------------
     How to use Coqine
---------------------------


- make -C encodings
    Generates encodings for Coq in the encodings/_build folder then checks the
    generated files


- make test_fix
    Translates the file run/test/import.v and all it's dependencies into the
    run/test/out folder then checks the generated files
    The translation of import.v itself is run/test/out/Top__import.dk
    The encoding file generated is run/test/C.dk
    This translation relies on privates casts

- make test_poly
    Same as test_fix but the translation relies on private codes

- make debug_fix
    Translates the files in run/debug/Test and all their dependencies into the
    run/debug/out folder then checks the generated files
    This translation relies on privates casts

- make debug_poly
    Same as debug_fix but the translation relies on private codes

- make tests
    Run the four targets above successively
    This must work before pushing to the repo !!

- make geocoq_short
    GeoCoq needs to be installed. Set correct path in run/geocoq/Makefile.
    Fetches a subset of the GeoCoq library and translates it with all it's
    dependencies into the run/geocoq/out folder then checks the generated files
    This translation relies on privates casts

- make orig_geocoq_short
    Same as geocoq_short but translates the original proofs
    from Euclid's Elements in the run/geocoq_orig/out

endef
export MANUAL

.PHONY: all plugin install uninstall clean fullclean help tests test

all: check-version .merlin plugin help

help:
	@echo "$$MANUAL"

tests: check-version .merlin plugin
	make test_pred_fix
	make test_codes_fix
	make debug_pred_fix
	make debug_codes_fix
test: tests

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
	make -C $(RUN_MAIN_DIR)        clean
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
	echo "Dedukti Set Encoding \"$(COQINE_FLAGS)\"." >> $(RUNDIR)/config.v
	make -C $(RUNDIR) $(EXTRA_FLAGS)



.PHONY: _test
_test: RUNDIR:=$(RUN_MAIN_DIR)
_test: COQINE_FLAGS:=template
_test: EXTRA_FLAGS:=MAINFILE=main_test
_test: run

.PHONY: _debug
_debug: RUNDIR:=$(RUN_MAIN_DIR)
_debug: EXTRA_FLAGS:=MAINFILE=main_debug
_debug: run

.PHONY: _mathcomp
_mathcomp: RUNDIR:=$(RUN_MATHCOMP_DIR)
_mathcomp: run

.PHONY: _orig_geocoq
_orig_geocoq: RUNDIR:=$(RUN_GEOCOQ_ORIG_DIR)
_orig_geocoq: COQINE_FLAGS:=polymorph
_orig_geocoq: run

.PHONY: _geocoq
_geocoq: RUNDIR:=$(RUN_GEOCOQ_DIR)
_geocoq: COQINE_FLAGS:=polymorph
_geocoq: run



.PHONY: test_pred
test_pred: ENCODING:=predicates_eta/C
test_pred: _test

.PHONY: test_pred_fix
test_pred_fix: ENCODING:=predicates_eta_fix/C
test_pred_fix: _test

.PHONY: test_codes_fix
test_codes_fix: ENCODING:=fullcodes_eta_fix/C
test_codes_fix: _test


.PHONY: debug_pred
debug_pred: ENCODING:=predicates_eta/C
debug_pred: COQINE_FLAGS:=template
debug_pred: _debug

.PHONY: debug_pred_fix
debug_pred_fix: ENCODING:=predicates_eta_fix/C
debug_pred_fix: COQINE_FLAGS:=template
debug_pred_fix: _debug

.PHONY: debug_codes_fix
debug_codes_fix: ENCODING:=fullcodes_eta_fix/C
debug_codes_fix: COQINE_FLAGS:=template
debug_codes_fix: _debug

.PHONY: debug_pred_poly
debug_pred_poly: ENCODING:=predicates_eta_fix/C
debug_pred_poly: COQINE_FLAGS:=polymorph
debug_pred_poly: _debug

.PHONY: debug_codes_poly
debug_codes_poly: ENCODING:=fullcodes_eta_fix/C
debug_codes_poly: COQINE_FLAGS:=polymorph
debug_codes_poly: _debug

.PHONY: debug_default
debug_default: ENCODING:=original/C
debug_default: COQINE_FLAGS:=
debug_default: _debug

.PHONY: debug_cast
debug_cast: ENCODING:=original_cast/C
debug_cast: COQINE_FLAGS:=
debug_cast: _debug

.PHONY: debug_named
debug_named: ENCODING:=original/C
debug_named: COQINE_FLAGS:=named
debug_named: _debug



.PHONY: mathcomp_lift
mathcomp_lift: ENCODING:=lift_predicates/C
mathcomp_lift: COQINE_FLAGS:=
mathcomp_lift: _mathcomp

.PHONY: mathcomp_debug
mathcomp_debug: ENCODING:=predicates/C
mathcomp_debug: COQINE_FLAGS:=polymorph
mathcomp_debug: _mathcomp


# These targets require GeoCoq. Set correct path in run/geocoq/Makefile.
.PHONY: orig_geocoq_long
orig_geocoq_long: ENCODING:=predicates/Coq
orig_geocoq_long: _orig_geocoq

.PHONY: orig_geocoq_short
orig_geocoq_short: ENCODING:=predicates/C
orig_geocoq_short: _orig_geocoq


.PHONY: geocoq_long
geocoq_long: ENCODING:=predicates_eta_fix/Coq
geocoq_long: _geocoq

.PHONY: geocoq_short
geocoq_short: ENCODING:= predicates_eta_fix/C
geocoq_short: _geocoq
