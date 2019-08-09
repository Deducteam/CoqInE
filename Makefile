# Variables
COQ_MAKEFILE ?= coq_makefile
COQTOP       ?= coqtop
DKCHECK      ?= dkcheck
DKDEP        ?= dkdep
VERBOSE      ?=

CAMLFLAGS="-bin-annot -annot"

RUNDIR=run
RUN_MAIN_DIR=$(RUNDIR)/main
RUN_MATHCOMP_DIR=$(RUNDIR)/mathcomp

COQ_VERSION   := $(shell $(COQTOP) -print-version)
CHECK_VERSION := $(shell $(COQTOP) -print-version | grep "8\.8\.*")

define MANUAL

---------------------------
     How to use Coqine
---------------------------


- make -C encodings
    Generates encodings for Coq in the encodings/_build folder
    then checks the generated files

- make test_pred_fix
    Translates the file run/main/Test/Fixpoints.v and all it's dependencies
    into the run/main/out folder then checks the generated files.
    The translated file can be changed by editing run/main/main_test.v
    The encoding file generated is run/main/C.dk
    This translation relies on privates casts

- make test_codes_fix
    Same as test_fix but the translation relies on private codes

- make debug_pred_fix
    Translates non universe polymorphis files from run/main/Test and their
    dependencies into the run/main/out folder then checks the generated files
    This translation relies on privates casts

- make debug_codes_fix
    Same as debug_fix but the translation relies on private codes

- make poly_codes_fix
    Translates all files from run/main/Test and their dependencies
    into the run/main/out folder then checks the generated files
    This translation relies on private codes

- make tests
    Run all the above targets successively
    This must work before pushing to the repo !!

endef
export MANUAL

.PHONY: all plugin install uninstall clean fullclean help tests test

all: check-version .merlin plugin .coqrc help

help:
	@echo "$$MANUAL"

tests: check-version .merlin plugin
	make -C encodings
	make test_pred_fix
	make test_codes_fix
	make debug_pred_fix
	make debug_codes_fix
	make poly_codes_fix
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
	make -C $(RUN_MATHCOMP_DIR)    clean
	rm -f $(RUN_MAIN_DIR)/*.dk
	rm -f $(RUN_MATHCOMP_DIR)/*.dk
	rm -f $(RUN_MAIN_DIR)/config.v
	rm -f $(RUN_MATHCOMP_DIR)/config.v
	rm -f CoqMakefile
	rm -f .coqrc

fullclean: clean
	rm src/*.cmt
	rm src/*.cmti
	rm src/*.annot

CoqMakefile: Make
	$(COQ_MAKEFILE) -f Make -o CoqMakefile
	echo "COQMF_CAMLFLAGS+=-annot -bin-annot -g" >> CoqMakefile.conf

.coqrc: plugin
	echo "Add ML Path \"$(shell pwd)/src\"." > .coqrc

# Targets for several libraries to translate

ENCODING     ?= original_cast/Coq # Configuration for the encoding generation
COQINE_FLAGS ?= original_cast # Configuration for the translator

.PHONY: run
run: plugin .coqrc
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
_debug: COQINE_FLAGS:=template

_debug: EXTRA_FLAGS:=MAINFILE=main_debug
_debug: run

.PHONY: _poly
_poly: RUNDIR:=$(RUN_MAIN_DIR)
_poly: EXTRA_FLAGS:=MAINFILE=main_poly
_poly: COQINE_FLAGS:=polymorph
_poly: run

.PHONY: _mathcomp
_mathcomp: RUNDIR:=$(RUN_MATHCOMP_DIR)
_mathcomp: run



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
debug_pred: _debug

.PHONY: debug_pred_fix
debug_pred_fix: ENCODING:=predicates_eta_fix/C
debug_pred_fix: _debug

.PHONY: debug_codes_fix
debug_codes_fix: ENCODING:=fullcodes_eta_fix/C
debug_codes_fix: _debug


.PHONY: poly_pred_fix
poly_pred_fix: ENCODING:=predicates_eta_fix/C
poly_pred_fix: _poly

.PHONY: poly_codes_fix
poly_codes_fix: ENCODING:=fullcodes_eta_fix/C
poly_codes_fix: _poly


#.PHONY: debug_default
#debug_default: ENCODING:=original/C
#debug_default: COQINE_FLAGS:=
#debug_default: _debug

#.PHONY: debug_cast
#debug_cast: ENCODING:=original_cast/C
#debug_cast: COQINE_FLAGS:=
#debug_cast: _debug

#.PHONY: debug_named
#debug_named: ENCODING:=original/C
#debug_named: COQINE_FLAGS:=named
#debug_named: _debug



.PHONY: mathcomp_lift
mathcomp_lift: ENCODING:=lift_predicates/C
mathcomp_lift: COQINE_FLAGS:=
mathcomp_lift: _mathcomp

.PHONY: mathcomp_debug
mathcomp_debug: ENCODING:=predicates/C
mathcomp_debug: COQINE_FLAGS:=polymorph
mathcomp_debug: _mathcomp
