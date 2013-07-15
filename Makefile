COQ_MAKEFILE = coq_makefile

MAKEFILE_INPUT = Makefile.input
MAKEFILE_PLUGIN = Makefile.plugin

.PHONY: plugin install clean

plugin: $(MAKEFILE_PLUGIN)
	make -f $(MAKEFILE_PLUGIN)

install: $(MAKEFILE_PLUGIN)
	make -f $(MAKEFILE_PLUGIN) install

clean: $(MAKEFILE_PLUGIN)
	make -f $(MAKEFILE_PLUGIN) clean
	rm -f $(MAKEFILE_PLUGIN)

$(MAKEFILE_PLUGIN): $(MAKEFILE_INPUT)
	$(COQ_MAKEFILE) -f $(MAKEFILE_INPUT) -o $(MAKEFILE_PLUGIN)

