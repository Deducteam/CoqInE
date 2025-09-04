DKCHECK?=dk check
BUILD=_build

all:

check:

$(BUILD):
	@rm -rf $(BUILD)
	@mkdir $(BUILD)

%.config: %.dk
	@echo "Dedukti Set Param \"encoding_file\" \"$(@F)\"." | sed --expression="s/.config//" > $@
	@sed -n 's/^\(.*\)Coqine *\([^ ]*\) *= *\([^ ]*\)\(.*\)/Dedukti Set Param "\2" "\3"./p' $^ >> $@

define targ
$(BUILD)/$1: $(BUILD)
	@rm -rf $(BUILD)/$1
	@mkdir $(BUILD)/$1

$(BUILD)/$1/Coq.dk: $(BUILD) $(BUILD)/$1
	@cat interfaces/$2.dk private/$3.dk theories/$4.dk fixpoint/$5.dk tests/$6.dk > $(BUILD)/$1/Coq.dk 2>/dev/null || true

$(BUILD)/$1/C.dk: $(BUILD) $(BUILD)/$1 $(BUILD)/$1/Coq.dk
	@cp $(BUILD)/$1/Coq.dk $(BUILD)/$1/C.dk
	@sed -i 's/Coqine *\([^ ]*\)/Coqine_\1/g' $(BUILD)/$1/C.dk
	@sed -i 's/\(^\|[^_A-Za-z0-9]\)Sort/\1S/g;   \
             s/\(^\|[^_A-Za-z0-9]\)Term/\1T/g;   \
             s/\(^\|[^_A-Za-z0-9]\)Univ/\1U/g;   \
             s/\(^\|[^_A-Za-z0-9]\)univ/\1u/g;   \
             s/\(^\|[^_A-Za-z0-9]\)Decode/\1_D/g;\
             s/\(^\|[^_A-Za-z0-9]\)uncode/\1_u/g;\
             s/\(^\|[^_A-Za-z0-9]\)code/\1_c/g'   $(BUILD)/$1/C.dk
	@sed -i 's/Coqine_\([^ ]*\)/Coqine \1/g' $(BUILD)/$1/C.dk
	@echo "(; Coqine readable = true ;)" >> $(BUILD)/$1/C.dk

$(BUILD)/$1/C.dko: $(BUILD)/$1/C.dk
	@$(DKCHECK) $7 -e $(BUILD)/$1/C.dk

$(BUILD)/$1/Coq.dko: $(BUILD)/$1/Coq.dk
	@$(DKCHECK) $7 -e $(BUILD)/$1/Coq.dk

# Aliases
$1: $(BUILD)/$1/Coq.dk
$1_short: $(BUILD)/$1/C.dk
$1_check: $(BUILD)/$1/C.dko $(BUILD)/$1/Coq.dko

all: $(BUILD)/$1/Coq.dko
all: $(BUILD)/$1/C.dko
all: $(BUILD)/$1/Coq.config
all: $(BUILD)/$1/C.config
endef

$(eval $(call targ,original,original,original,original,,))
$(eval $(call targ,original_cast,original_cast,original_cast,original_cast,,))

$(eval $(call targ,predicates,predicates,predicates,predicates,,))
$(eval $(call targ,predicates_eta,predicates,predicates_eta,predicates,,))
$(eval $(call targ,predicates_eta_fix,predicates,predicates_eta,predicates,predicates,))
$(eval $(call targ,predicates_eta_fix_test,predicates,predicates_eta,predicates,predicates,test_fp_predicates))

$(eval $(call targ,fpredicates_eta,predicates,fpredicates_eta,fpredicates,,))
$(eval $(call targ,fpredicates_eta_fix,predicates,fpredicates_eta,fpredicates,fpredicates,))
$(eval $(call targ,fpredicates_eta_fix_test,predicates,fpredicates_eta,fpredicates,fpredicates,test_fp_fpredicates))

$(eval $(call targ,lift_predicates,lift-predicates,lift-predicates,lift-predicates,,))

$(eval $(call targ,functional,functional,functional,functional,,))
$(eval $(call targ,constructors,constructors,functional,functional,,))
$(eval $(call targ,full_constructors,full_constructors,functional,functional,,))

$(eval $(call targ,deepcodes,predicates,deepcodes,predicates,,))
$(eval $(call targ,deepcodes_test1,predicates,deepcodes,test1,,))
$(eval $(call targ,deepcodes_test2,predicates,deepcodes,predicates,,test2))

$(eval $(call targ,fullcodes,predicates,fullcodes,predicates,,))
$(eval $(call targ,fullcodes_test1,predicates,fullcodes,test1,,))
$(eval $(call targ,fullcodes_test2,predicates,fullcodes,predicates,,test2))
$(eval $(call targ,fullcodes_test3,predicates,fullcodes,predicates,,test3))

$(eval $(call targ,fullcodes_eta,predicates,fullcodes_eta,predicates,,))
$(eval $(call targ,fullcodes_eta_test2,predicates,fullcodes_eta,predicates,,test2))
$(eval $(call targ,fullcodes_eta_test3,predicates,fullcodes_eta,predicates,,test3))

$(eval $(call targ,fullcodes_eta_fix,predicates,fullcodes_eta,predicates,fullcodes,))
$(eval $(call targ,fullcodes_eta_fix_test2,predicates,fullcodes_eta,predicates,fullcodes,test2))
$(eval $(call targ,fullcodes_eta_fix_test3,predicates,fullcodes_eta,predicates,fullcodes,test3))

$(eval $(call targ,fullcodes_eta_fix2,predicates,fullcodes_eta,predicates,fullcodes_multis,))

$(eval $(call targ,fullcodes_templ,predicates,fullcodes_templ,predicates,fullcodes_templ,))

$(eval $(call targ,fullcodes_poly,predicates_ac,fullcodes_poly,predicates_ac,fullcodes_poly,test_poly,))

$(eval $(call targ,fullcodes_poly_templ,predicates_ac,fullcodes_poly_templ,predicates_ac,fullcodes_poly_templ,))

$(eval $(call targ,fullcodes_poly_cstr,predicates_ac_cstr,fullcodes_poly_cstr,predicates_ac,fullcodes_poly,test_poly,))

$(eval $(call targ,fullcodes_poly_templ_linear,predicates_ac_cstr2,fullcodes_poly_templ_linear,predicates_ac,fullcodes_poly_templ,test_poly,))

.PHONY: clean
clean:
	rm -rf $(BUILD)
