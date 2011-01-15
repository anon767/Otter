# See http://www.embecosm.com/appnotes/ean9/ean9-howto-newlib-1.0.pdf,
# especially chapter 9, for instructions on how to extend newlib to a
# new target.

NEWLIB_1_19_0 := $(dir $(CURDIR))
OTTER_TRUNK := $(realpath $(NEWLIB_1_19_0)/..)
BUILD := $(NEWLIB_1_19_0)/build

# $(NEWLIB_1_19_0)/configure is not included here because it seems
# especially picky. We'll just take the one we were given, rather than
# trying to auto-generate it ourselves.
CONFIG_FILES := $(addprefix $(NEWLIB_1_19_0)/newlib/libc/machine/, configure otter/configure)
MAKE_FILES := $(NEWLIB_1_19_0)/newlib/libc/machine/otter/Makefile.in

all: libc.c newlib.c

build_complete: otter-cc otter-ar $(BUILD)/Makefile
	make -C $(BUILD) && touch build_complete

libc.c: build_complete
	tar xf $(BUILD)/otter/newlib/libc/libc.a libc.a.cil.a
	mv libc.a.cil.a libc.c
	@touch libc.c # Update modification time to prevent re-running this recipe

newlib.c: build_complete
	tar xf $(BUILD)/otter/newlib/libc.a libc.a.cil.a
	mv libc.a.cil.a newlib.c
	@touch newlib.c # Update modification time to prevent re-running this recipe

otter-ar: otter-ar.in
	sed 's:@OTTER_TRUNK@:$(OTTER_TRUNK):' otter-ar.in > otter-ar
	chmod +x otter-ar

otter-cc: otter-cc.in
	sed 's:@OTTER_TRUNK@:$(OTTER_TRUNK):g' otter-cc.in > otter-cc
	chmod +x otter-cc

$(BUILD)/Makefile: otter-cc otter-ar $(CONFIG_FILES) $(MAKE_FILES) $(NEWLIB_1_19_0)/newlib/configure.host
	mkdir -p $(BUILD)
	cd $(BUILD) && \
	$(NEWLIB_1_19_0)/configure --target=otter --with-newlib --disable-multilib \
		CC_FOR_TARGET=$(NEWLIB_1_19_0)/otter/otter-cc \
		AR_FOR_TARGET=$(NEWLIB_1_19_0)/otter/otter-ar \
		RANLIB_FOR_TARGET=true

.PHONY: clean distclean
clean:
	rm -f otter-ar otter-cc build_complete libc.c newlib.c
	if [[ -f $(BUILD)/Makefile ]]; then make -C $(BUILD) clean; fi

distclean: clean
	rm -f $(CONFIG_FILES) $(CONFIG_FILES:configure=aclocal.m4) $(MAKE_FILES)
	if [[ -f $(BUILD)/Makefile ]]; then make -C $(BUILD) distclean; fi

# I modeled these rules after Otter's top-level makefile. Thanks, Yit.
.PRECIOUS : %/aclocal.m4 %/configure %/Makefile

%/aclocal.m4: $(NEWLIB_1_19_0)/newlib/acinclude.m4
	cd $* && aclocal -I $(NEWLIB_1_19_0)/newlib

%/configure : %/configure.in %/aclocal.m4
	cd $* && autoconf

%/Makefile.in : %/Makefile.am %/configure
	cd $* && automake --cygnus Makefile
