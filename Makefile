# Variables
COQ_MAKEFILE ?= coq_makefile
COQTOP       ?= coqtop
DKCHECK      ?= dkcheck
DKDEP        ?= dkdep
VERBOSE      ?=

CAMLFLAGS="-bin-annot -annot"

RUNDIR=run

COQ_VERSION   := $(shell $(COQTOP) -print-version)
CHECK_VERSION := $(shell $(COQTOP) -print-version | grep "8\.8\.*")

define MANUAL

---------------------------------------
            How to use Coqine
---------------------------------------


- make -C encodings
    Generates encodings for Coq in the encodings/_build folder
    then checks the generated files

- make test_pred_fix
    Translates the file run/main/Test/Fixpoints.v and all its dependencies
    from Init into the run/main/out folder then checks the generated files.
    The translated file can be changed by editing run/main/main_test.v
    The encoding file generated is run/main/C.dk
    This translation relies on privates casts

- make test_codes_fix
    Same as test_pred_fix but the translation relies on private codes

- make test_tcodes_fix
    Same as test_codes_fix but the translation relies on private version
    of template inductive types.

- make debug_pred_fix
    Translates non universe polymorphis files from run/main/Test and their
    dependencies into the run/main/out folder then checks the generated files.
    The translated file can be changed by editing run/main/main_debug.v
    The encoding file generated is run/main/C.dk
    This translation relies on privates casts

- make debug_codes_fix
    Same as debug_fix but the translation relies on private codes

- make fullcodes_poly_templ
    Translates all files from run/main/Test and their dependencies including universe
    polymorphism into the run/main/out folder then checks the generated files
    This translation relies on private codes, private version of template inductives
    and constraints for true polymorphism.

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
	make test_tcodes_fix
	make debug_pred_fix
	make debug_codes_fix
	make upoly_logipedia
	make logipedia
	make fullcodes_poly_templ
#	make poly_codes_poly
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

	make -C $(RUNDIR)/main            clean
	make -C $(RUNDIR)/mathcomp        clean
	make -C $(RUNDIR)/logipedia       clean
	make -C $(RUNDIR)/upoly_logipedia clean

	rm -f $(RUNDIR)/main/*.dk
	rm -f $(RUNDIR)/mathcomp/*.dk
	rm -f $(RUNDIR)/logipedia/*.dk
	rm -f $(RUNDIR)/upoly_logipedia/*.dk

	rm -f $(RUNDIR)/main/config.v
	rm -f $(RUNDIR)/mathcomp/config.v
	rm -f $(RUNDIR)/logipedia/config.v
	rm -f $(RUNDIR)/upoly_logipedia/config.v
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

.PHONY: polymorph_config
polymorph_config:
	echo "Dedukti Set Param \"templ_polymorphism\" \"true\"." >> $(RUNDIR)/config.v
	echo "Dedukti Set Param \"polymorphism\"       \"true\"." >> $(RUNDIR)/config.v
	echo "Dedukti Set Param \"constraints\"        \"true\"." >> $(RUNDIR)/config.v

.PHONY: template_config
template_config:
	echo "Dedukti Set Param \"templ_polymorphism\" \"true\"." >> $(RUNDIR)/config.v

.PHONY: float_config
float_config:
	echo "Dedukti Set Param \"float_univ\" \"true\"." >> $(RUNDIR)/config.v
	echo "Dedukti Set Param \"universe_file\" \"U\"." >> $(RUNDIR)/config.v

.PHONY: named_config
named_config:
	echo "Dedukti Set Param \"named_univ\" \"true\"." >> $(RUNDIR)/config.v

.PHONY: cast_config
cast_config:
	echo "Dedukti Set Param \"use_cast\" \"true\"." >> $(RUNDIR)/config.v


# generate : target  ,  path  ,  encoding  ,  C/Coq  ,  polymorph  ,  extra flags
define generate
.PHONY: $1
$1: plugin .coqrc
	@echo ""
	@echo "------------------------------------------"
	@echo "  Building Target: $1"
	@echo "------------------------------------------"
	make -C encodings clean _build/$3/$4.dk
	cp encodings/_build/$3/$4.dk $2/$4.dk
	make -C encodings clean _build/$3/$4.config
	cp encodings/_build/$3/$4.config $2/config.v
ifeq ($5, polymorph)
	echo "Dedukti Set Param \"tpolymorphism\" \"true\"." >> $2/config.v
	echo "Dedukti Set Param \"upolymorphism\" \"true\"." >> $2/config.v
	echo "Dedukti Set Param \"constraints\"   \"true\"." >> $2/config.v
else ifeq ($5, cpolymorph)
	echo "Dedukti Set Param \"tpolymorphism\" \"true\"." >> $2/config.v
	echo "Dedukti Set Param \"tpoly_code\" \"true\"." >> $2/config.v
	echo "Dedukti Set Param \"upolymorphism\" \"true\"." >> $2/config.v
	echo "Dedukti Set Param \"constraints\"   \"true\"." >> $2/config.v
else ifeq ($5, template)
	echo "Dedukti Set Param \"tpolymorphism\" \"true\"." >> $2/config.v
else ifeq ($5, ctemplate)
	echo "Dedukti Set Param \"tpolymorphism\" \"true\"." >> $2/config.v
	echo "Dedukti Set Param \"tpoly_code\" \"true\"." >> $2/config.v
else ifeq ($5, float)
	echo "Dedukti Set Param \"float_univ\" \"true\"." >> $2/config.v
	echo "Dedukti Set Param \"universe_file\" \"U\"." >> $2/config.v
else ifeq ($5, named)
	echo "Dedukti Set Param \"named_univ\" \"true\"." >> $2/config.v
else ifeq ($5, cast)
	echo "Dedukti Set Param \"use_cast\" \"true\"." >> $2/config.v
endif
	echo "Dedukti Set Param \"encoding_name\" \"$5\"." >> $2/config.v
	make -C $2 clean
	make -C $2 $6

endef

$(eval $(call generate,test_pred,run/main,predicates_eta,C,template,MAINFILE=main_test))

$(eval $(call generate,test_pred_fix ,run/main,predicates_eta_fix,C,template,MAINFILE=main_test))
$(eval $(call generate,test_codes_fix,run/main,fullcodes_eta_fix,C,template,MAINFILE=main_test))
$(eval $(call generate,test_tcodes_fix,run/main,fullcodes_templ,C,ctemplate,MAINFILE=main_test))

$(eval $(call generate,debug_pred,run/main,predicates_eta,C,template,MAINFILE=main_debug))
$(eval $(call generate,debug_pred_fix,run/main,predicates_eta_fix,C,template,MAINFILE=main_debug))

$(eval $(call generate,debug_codes_fix,run/main,fullcodes_eta_fix,C,template,MAINFILE=main_debug))

$(eval $(call generate,poly_pred_fix,run/main,predicates_eta_fix,C,polymorph,MAINFILE=main_poly))
$(eval $(call generate,poly_codes_fix,run/main,fullcodes_eta_fix,C,polymorph,MAINFILE=main_poly))
$(eval $(call generate,poly_codes_poly,run/main,fullcodes_poly,C,polymorph,MAINFILE=main_poly))

$(eval $(call generate,fullcodes_poly_templ,run/main,fullcodes_poly_templ,C,cpolymorph,MAINFILE=main_poly))
$(eval $(call generate,fullcodes_poly_cstr,run/main,fullcodes_poly_cstr,C,cpolymorph,MAINFILE=main_poly))
$(eval $(call generate,fullcodes_poly_cstr2,run/main,fullcodes_poly_cstr2,C,cpolymorph,MAINFILE=main_poly))
$(eval $(call generate,fullcodes_poly_templ_linear,run/main,fullcodes_poly_templ_linear,C,cpolymorph,MAINFILE=main_poly))

$(eval $(call generate,mathcomp,run/mathcomp,fullcodes_eta_fix,C,template,))
$(eval $(call generate,mathcomp_lift,run/mathcomp,lift_predicates,C,cast,))
$(eval $(call generate,mathcomp_debug,run/mathcomp,predicates,C,polymorph,))

#$(eval $(call generate,orig,run/main,original,C,,MAINFILE=main_test))
#$(eval $(call generate,orig_cast,run/main,original_cast,C,cast,MAINFILE=main_test))
#$(eval $(call generate,orig_named,run/main,original,C,named,MAINFILE=main_test))



# Manual targets for Logipedia exports

.PHONY: logipedia
logipedia: plugin .coqrc
	@echo ""
	@echo "------------------------------------------"
	@echo "  Building Target: Logipedia"
	@echo "------------------------------------------"
	make -C encodings clean _build/predicates_eta_fix/Coq.dk
	cp encodings/_build/predicates_eta_fix/Coq.dk run/logipedia/ctpicef.dk
	make -C encodings clean _build/predicates_eta_fix/Coq.config
	cp encodings/_build/predicates_eta_fix/Coq.config run/logipedia/config.v
	echo "Dedukti Set Param \"tpolymorphism\" \"true\"."         >> run/logipedia/config.v
	echo "Dedukti Set Param \"named_univ\"    \"true\"."         >> run/logipedia/config.v
	echo "Dedukti Set Param \"universe_file\" \"U\"."            >> run/logipedia/config.v
	echo "Dedukti Set Param \"encoding_name\" \"template_cic\"." >> run/logipedia/config.v
	echo "Dedukti Set Param \"encoding_file\" \"ctpicef\"."      >> run/logipedia/config.v
	make -C run/logipedia clean
	make -C run/logipedia
	rm -rf std ctpicef.dk
	mkdir std
	cp run/logipedia/out/Coq*.dk std/
	cp run/logipedia/ctpicef.dk ./
	tar cj ctpicef.dk std > ctpicef.tar.bz2
	rm -rf std ctpicef.dk

.PHONY: upoly_logipedia
upoly_logipedia: plugin .coqrc
	@echo ""
	@echo "------------------------------------------"
	@echo "  Building Target: U. Poly. Logipedia"
	@echo "------------------------------------------"
	make -C encodings clean _build/fullcodes_poly_templ/Coq.dk
	cp encodings/_build/fullcodes_poly_templ/Coq.dk run/upoly_logipedia/cupicef.dk
	make -C encodings clean _build/fullcodes_poly_templ/Coq.config
	cp encodings/_build/fullcodes_poly_templ/Coq.config run/upoly_logipedia/config.v
	echo "Dedukti Set Param \"tpolymorphism\" \"true\"."         >> run/upoly_logipedia/config.v
	echo "Dedukti Set Param \"tpoly_code\"    \"true\"."         >> run/upoly_logipedia/config.v
	echo "Dedukti Set Param \"upolymorphism\" \"true\"."         >> run/upoly_logipedia/config.v
	echo "Dedukti Set Param \"constraints\"   \"true\"."         >> run/upoly_logipedia/config.v
	echo "Dedukti Set Param \"encoding_name\" \"template_cic\"." >> run/upoly_logipedia/config.v
	echo "Dedukti Set Param \"encoding_file\" \"cupicef\"."      >> run/upoly_logipedia/config.v
	make -C run/upoly_logipedia clean
	make -C run/upoly_logipedia
	rm -rf std cupicef.dk
	mkdir std
	cp run/upoly_logipedia/out/Coq*.dk std/
	cp run/upoly_logipedia/cupicef.dk ./
	tar cj cupicef.dk std > cupicef.tar.bz2
	rm -rf std cupicef.dk
