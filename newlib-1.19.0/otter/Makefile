# See http://www.embecosm.com/appnotes/ean9/ean9-howto-newlib-1.0.pdf,
# especially chapter 9, for instructions on how to extend newlib to a
# new target.

SUFFIX = 
NEWLIB_1_19_0 := $(dir $(CURDIR))
OTTER_TRUNK := $(realpath $(NEWLIB_1_19_0)/..)
BUILD := $(NEWLIB_1_19_0)/build$(SUFFIX)
BIN_DIR := $(NEWLIB_1_19_0)/otter/bin$(SUFFIX)
LIB_DIR := $(NEWLIB_1_19_0)/otter/lib$(SUFFIX)

# $(NEWLIB_1_19_0)/configure is not included here because it seems
# especially picky. We'll just take the one we were given, rather than
# trying to auto-generate it ourselves.
CONFIG_FILES := $(addprefix $(NEWLIB_1_19_0)/newlib/libc/machine/, configure otter/configure)
MAKE_FILES := $(NEWLIB_1_19_0)/newlib/libc/machine/otter/Makefile.in

OTTER_EXES := $(addprefix $(BIN_DIR)/, otter-cc otter-ar otter-ranlib otter-mv otter-libc-cc)
MERGED_FILES := libm.a libc.a

all: $(MERGED_FILES)

libc.a: $(BUILD)/otter/newlib/libc/libc.a
libm.a: $(BUILD)/otter/newlib/libm.a

$(MERGED_FILES): 
	mkdir -p $(LIB_DIR)
	$(BIN_DIR)/otter-mv $< $@
	mv $@ $(LIB_DIR)/$@

$(BUILD)/otter/newlib/%.a: $(BUILD)/Makefile
	make -C $(BUILD) all-target-newlib

$(BIN_DIR)/otter-%: $(BIN_DIR)/otter-%.in
	sed 's:@OTTER_TRUNK@:$(OTTER_TRUNK):g' $< > $@
	chmod +x $@

$(BUILD)/Makefile: $(OTTER_EXES) $(CONFIG_FILES) $(MAKE_FILES) $(NEWLIB_1_19_0)/newlib/configure.host
	mkdir -p $(BUILD)
	cd $(BUILD) && \
	$(NEWLIB_1_19_0)/configure --target=otter --with-newlib --disable-multilib \
		CC_FOR_TARGET=$(BIN_DIR)/otter-cc \
		AR_FOR_TARGET=$(BIN_DIR)/otter-ar \
		RANLIB_FOR_TARGET=$(BIN_DIR)/otter-ranlib

.PHONY: clean distclean
clean:
	rm -rf $(OTTER_EXES) $(LIB_DIR)

distclean: clean
	rm -rf $(BUILD) $(CONFIG_FILES) $(CONFIG_FILES:configure=aclocal.m4) $(CONFIG_FILES:configure=autom4te.cache) $(MAKE_FILES)

# I modeled these rules after Otter's top-level makefile. Thanks, Yit.
.PRECIOUS : %/aclocal.m4 %/configure %/Makefile

%/aclocal.m4: $(NEWLIB_1_19_0)/newlib/acinclude.m4
	cd $* && aclocal -I $(NEWLIB_1_19_0)/newlib

%/configure : %/configure.in %/aclocal.m4
	cd $* && autoconf

%/Makefile.in : %/Makefile.am %/configure
	cd $* && automake --cygnus Makefile
