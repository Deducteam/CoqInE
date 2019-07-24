DKCHECK?=dkcheck
BUILD=_build

all:

check:

$(BUILD):
	@rm -rf $(BUILD)
	@mkdir $(BUILD)

define targ
$(BUILD)/$1: $(BUILD)
	@rm -rf $(BUILD)/$1
	@mkdir $(BUILD)/$1

$(BUILD)/$1/Coq.dk: $(BUILD) $(BUILD)/$1
	@cat interfaces/$2.dk private/$3.dk theories/$4.dk fixpoint/$5.dk tests/$6.dk > $(BUILD)/$1/Coq.dk 2>/dev/null || true

$(BUILD)/$1/C.dk: $(BUILD) $(BUILD)/$1 $(BUILD)/$1/Coq.dk
	@cp $(BUILD)/$1/Coq.dk $(BUILD)/$1/C.dk
	@sed -i 's/Sort/S/g' $(BUILD)/$1/C.dk
	@sed -i 's/Term/T/g' $(BUILD)/$1/C.dk
	@sed -i 's/Univ/U/g' $(BUILD)/$1/C.dk
	@sed -i 's/univ/u/g' $(BUILD)/$1/C.dk
	@sed -i 's/Decode/_D/g' $(BUILD)/$1/C.dk
	@sed -i 's/uncode/_u/g' $(BUILD)/$1/C.dk
	@sed -i 's/code/_c/g' $(BUILD)/$1/C.dk

$1: $(BUILD)/$1/Coq.dk

$1_short: $(BUILD)/$1/C.dk

$(BUILD)/$1/C.dko: $(BUILD)/$1/C.dk
	$(DKCHECK) -e $(BUILD)/$1/C.dk

$(BUILD)/$1/Coq.dko: $(BUILD)/$1/Coq.dk
	$(DKCHECK) -e $(BUILD)/$1/Coq.dk

all: $(BUILD)/$1/Coq.dko
all: $(BUILD)/$1/C.dko
endef

$(eval $(call targ,original,original,original,original,,))
$(eval $(call targ,original_cast,original_cast,original_cast,original_cast,,))

$(eval $(call targ,predicates,predicates,predicates,predicates,,))
$(eval $(call targ,predicates_eta,predicates,predicates_eta,predicates,,))
$(eval $(call targ,predicates_eta_fix,predicates,predicates_eta,predicates,generic,))
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


.PHONY: check
check:
	$(foreach file, $(wildcard $(BUILD)/**/*.dk), $(DKCHECK) $(file) ;)

.PHONY: clean
clean:
	rm -rf $(BUILD)