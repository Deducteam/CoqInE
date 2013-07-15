COQ_MAKEFILE = coq_makefile

MAKEFILE_PLUGIN = Makefile.plugin

.PHONY: plugin install clean

plugin: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f -

install: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - install

clean: $(MAKEFILE_PLUGIN)
	$(COQ_MAKEFILE) -f $(MAKEFILE_PLUGIN) | make -f - clean

