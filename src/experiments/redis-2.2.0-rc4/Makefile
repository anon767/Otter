# Top level makefile, the real shit is at src/Makefile

TARGETS=32bit noopt test

all:
	cd src && $(MAKE) $@

install: dummy
	cd src && $(MAKE) $@

clean:
	cd src && $(MAKE) $@
	cd deps/hiredis && $(MAKE) $@
	cd deps/linenoise && $(MAKE) $@

$(TARGETS):
	cd src && $(MAKE) $@

src/help.h:
	@./utils/generate-command-help.rb > $@

dummy:

otter:
	$(MAKE) -f otter.makefile

otter-clean:
	$(MAKE) -f otter.makefile clean
