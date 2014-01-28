COQ_MAKEFILE = coq_makefile
COQTOP = coqtop
DKCHECK = dkcheck

MAKEFILE_TEST = Makefile.test

.PHONY: test clean

test: Coq.dko
	$(COQ_MAKEFILE) -f $(MAKEFILE_TEST) | make -f -
	$(COQTOP) -batch -nois -R ../src Dedukti -l Test
	$(DKCHECK) -autodep *.dk

Coq.dko: ../dedukti/Coq.dk
	$(DKCHECK) -e -unsafe ../dedukti/Coq.dk

clean:
	$(COQ_MAKEFILE) -f $(MAKEFILE_TEST) | make -f - clean
	rm -rf *.dk *.dko

