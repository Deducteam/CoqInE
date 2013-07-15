COQ_MAKEFILE = coq_makefile

PLUGIN_NAME = Dedukti
PLUGIN_MAKEFILE = Makefile.plugin
SRC_FILES = src/export.ml4 src/Dedukti.v
TEST_FILES = test/Test.v

.PHONY: plugin install test clean

plugin: $(PLUGIN_MAKEFILE)
	make -f $(PLUGIN_MAKEFILE)

install: $(PLUGIN_MAKEFILE)
	make -f $(PLUGIN_MAKEFILE) install

test: plugin
	coqc -R src $(PLUGIN_NAME) $(TEST_FILES)

clean: $(PLUGIN_MAKEFILE)
	make -f $(PLUGIN_MAKEFILE) clean
	rm -f $(PLUGIN_MAKEFILE)

$(PLUGIN_MAKEFILE): Makefile $(SRC_FILES)
	$(COQ_MAKEFILE) -R src $(PLUGIN_NAME) $(SRC_FILES) -o $(PLUGIN_MAKEFILE)

