COQ_MAKEFILE = coq_makefile

MAKEFILE_PLUGIN = Makefile.plugin
MAKEFILE_TEST = Makefile.test

.PHONY: plugin install test clean

plugin: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f -

install: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - install

test: plugin $(MAKEFILE_TEST)
	$(COQ_MAKEFILE) -f $(MAKEFILE_TEST) | make -f -

clean: $(MAKEFILE_PLUGIN) $(MAKEFILE_TEST)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - clean
	$(COQ_MAKEFILE) -f $(MAKEFILE_TEST) | make -f - clean

