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

OTTER_EXES := otter-cc otter-ar otter-libc-cc
MERGED_FILES := libm.c libc.c

all: $(MERGED_FILES)

libc.c: $(BUILD)/otter/newlib/libc/libc.a
libm.c: $(BUILD)/otter/newlib/libm.a
$(MERGED_FILES):
	tar xmf $< $(@:.c=.a).cil.a
	mv $(@:.c=.a).cil.a $@

$(BUILD)/otter/newlib/%.a: $(BUILD)/Makefile
	make -C $(BUILD)

otter-%: otter-%.in
	sed 's:@OTTER_TRUNK@:$(OTTER_TRUNK):g' $< > $@
	chmod +x $@

$(BUILD)/Makefile: $(OTTER_EXES) $(CONFIG_FILES) $(MAKE_FILES) $(NEWLIB_1_19_0)/newlib/configure.host
	mkdir -p $(BUILD)
	cd $(BUILD) && \
	$(NEWLIB_1_19_0)/configure --target=otter --with-newlib --disable-multilib \
		CC_FOR_TARGET=$(NEWLIB_1_19_0)/otter/otter-cc \
		AR_FOR_TARGET=$(NEWLIB_1_19_0)/otter/otter-ar \
		RANLIB_FOR_TARGET=true

.PHONY: clean distclean
clean:
	if [[ -f $(BUILD)/Makefile ]]; then make -C $(BUILD) clean; fi
	rm -f $(OTTER_EXES) $(MERGED_FILES)

distclean: clean
	if [[ -f $(BUILD)/Makefile ]]; then make -C $(BUILD) distclean; fi
	rm -f $(CONFIG_FILES) $(CONFIG_FILES:configure=aclocal.m4) $(MAKE_FILES)

# I modeled these rules after Otter's top-level makefile. Thanks, Yit.
.PRECIOUS : %/aclocal.m4 %/configure %/Makefile

%/aclocal.m4: $(NEWLIB_1_19_0)/newlib/acinclude.m4
	cd $* && aclocal -I $(NEWLIB_1_19_0)/newlib

%/configure : %/configure.in %/aclocal.m4
	cd $* && autoconf

%/Makefile.in : %/Makefile.am %/configure
	cd $* && automake --cygnus Makefile
