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
	make -C encodings clean _build/$(ENCODING)
	cp encodings/_build/$(ENCODING) $(RUNDIR)/
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
test_universo: ENCODING:=predicates_eta/C.dk
test_universo: COQINE_FLAGS:=readable universo
test_universo: test

.PHONY: test_fix
test_fix: ENCODING:=predicates_eta_fix/C.dk
test_fix: COQINE_FLAGS:=readable fix universo
test_fix: test

.PHONY: test_poly
test_poly: ENCODING:=fullcodes_eta_fix/C.dk
test_poly: COQINE_FLAGS:=readable fix polymorph
test_poly: test


.PHONY: debug_universo
debug_universo: ENCODING:=predicates/C.dk
debug_universo: COQINE_FLAGS:=readable universo
debug_universo: debug

.PHONY: debug_fix
debug_fix: ENCODING:=predicates_eta_fix/C.dk
debug_fix: COQINE_FLAGS:=readable fix polymorph
debug_fix: debug

.PHONY: debug_default
debug_default: ENCODING:=original/Coq.dk
debug_default: COQINE_FLAGS:=original
debug_default: debug

.PHONY: debug_readable
debug_readable: ENCODING:=original/C.dk
debug_readable: COQINE_FLAGS:=readable original
debug_readable: debug

.PHONY: debug_named_cast
debug_named_cast: ENCODING:=original_cast/Coq.dk
debug_named_cast: COQINE_FLAGS:=named original_cast
debug_named_cast: debug

.PHONY: debug_cast
debug_cast: ENCODING:=original_cast/C.dk
debug_cast: COQINE_FLAGS:=readable original_cast
debug_cast: debug

.PHONY: debug_template
debug_template: ENCODING:=original_cast/C.dk
debug_template: COQINE_FLAGS:=readable template_cast
debug_template: debug

.PHONY: debug_named
debug_named: ENCODING:=original/Coq.dk
debug_named: COQINE_FLAGS:=named original
debug_named: debug

.PHONY: debug_poly
debug_poly: ENCODING:=constructors/C.dk
debug_poly: COQINE_FLAGS:=readable polymorph
debug_poly: debug

.PHONY: lift_mathcomp
lift_mathcomp: ENCODING:=lift_predicates/C.dk
lift_mathcomp: COQINE_FLAGS:=readable lift_priv
lift_mathcomp: mathcomp

.PHONY: debug_mathcomp
debug_mathcomp: ENCODING:=predicates/C.dk
debug_mathcomp: COQINE_FLAGS:=readable universo
debug_mathcomp: mathcomp


# These targets require GeoCoq. Set correct path in run/geocoq/Makefile.
.PHONY: geocoq_orig_universo
geocoq_orig_universo: ENCODING:= predicates/Coq.dk
geocoq_orig_universo: COQINE_FLAGS  := universo
geocoq_orig_universo: RUNDIR        := $(RUN_GEOCOQ_ORIG_DIR)
geocoq_orig_universo: run

.PHONY: geocoq_orig
geocoq_orig: ENCODING:= predicates/C.dk
geocoq_orig: COQINE_FLAGS  := readable universo
geocoq_orig: RUNDIR        := $(RUN_GEOCOQ_ORIG_DIR)
geocoq_orig: run


.PHONY: geocoq_universo
geocoq_universo: ENCODING:= predicates/Coq.dk
geocoq_universo: COQINE_FLAGS  := universo
geocoq_universo: RUNDIR        := $(RUN_GEOCOQ_DIR)
geocoq_universo: run

.PHONY: geocoq
geocoq: ENCODING:= predicates/C.dk
geocoq: COQINE_FLAGS  := readable universo
geocoq: RUNDIR        := $(RUN_GEOCOQ_DIR)
geocoq: run
