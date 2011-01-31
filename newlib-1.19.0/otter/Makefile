# There are two targets that are meant as 'entry points': cilly (the
# default) and native. Each target creates a ../build* directory
# containing the output from building newlib, a lib* directory
# containing libc.a and libm.a (and, for cilly, also libotter{sys,posix}.a),
# and a *-with-libc script which functions as a compiler that makes
# use of the generated libraries.

TARGETS := cilly native

.PHONY: $(TARGETS) all

ifeq ($(MAKECMDGOALS),native)
  SUFFIX:=.native
endif

all: cilly


# See http://www.embecosm.com/appnotes/ean9/ean9-howto-newlib-1.0.pdf,
# especially chapter 9, for instructions on how to extend newlib to a
# new target.


# Setting some directory names

NEWLIB_1_19_0 := $(dir $(CURDIR))
OTTER_TRUNK := $(realpath $(NEWLIB_1_19_0)/..)
# Making these next two variables recursively (i.e., lazily) expanded
# makes 'clean' a bit easier
BUILD_DIR = $(NEWLIB_1_19_0)/build$(SUFFIX)
LIB_DIR = $(CURDIR)/lib$(SUFFIX)


# Specifying some configurations files we depend on

# $(NEWLIB_1_19_0)/configure is not included here because it seems
# especially picky. We'll just take the one we were given, rather than
# trying to auto-generate it ourselves.
CONFIG_FILES := $(addprefix $(NEWLIB_1_19_0)/newlib/libc/machine/, configure otter/configure)
MAKE_FILES := $(NEWLIB_1_19_0)/newlib/libc/machine/otter/Makefile.in


# Defining some command-line options

OPTIONS_FOR_BUILDING_LIBC := -nostdinc -nostdlib -Wp,-undef -isystem $(CURDIR)/include
OPTIONS_FOR_USING_LIBC := $(OPTIONS_FOR_BUILDING_LIBC) \
  -isystem $(CURDIR)/posix/include \
  -isystem $(CURDIR)/syscalls/include \
  -isystem $(BUILD_DIR)/otter/newlib/targ-include \
  -isystem $(NEWLIB_1_19_0)/newlib/libc/include \
  -L$(LIB_DIR) \
  -lc 

CILLY_CMD := CILLY_DONT_COMPILE_AFTER_MERGE= '$(OTTER_TRUNK)/cil/bin/cilly' --merge --keepmerged

# Setting target-specific variables for building with CIL and building natively

cilly: MV=tar xfO '$(1)' '$(notdir $(2)).cil.a' > '$(2)'
cilly: OTTER_AR:=$(CURDIR)/otter-ar
cilly: OTTER_CC:=$(CILLY_CMD)
cilly: OTTER_RANLIB:=true

native: MV=mv '$(1)' '$(2)'
native: OTTER_AR:=ar
native: OTTER_CC:=gcc -m32
native: OTTER_RANLIB:=ranlib


# Each target depends on the corresponding merged files.

MERGED_FILES := $(addprefix $(LIB_DIR)/,libm.a libc.a)

$(TARGETS): $(MERGED_FILES)
# Also build the *-with-libc script. For cilly, also build libotter{sys,posix}.a
native: gcc-with-libc
cilly: cilly-with-libc libottersys.a libotterposix.a


# Creating the scripts

OTTER_EXES := otter-ar cilly-with-libc gcc-with-libc

otter-ar: otter-ar.in
	sed 's:@OTTER_TRUNK@:$(OTTER_TRUNK):g' $< > $@
	chmod +x $@

gcc-with-libc:
	echo gcc -m32 /usr/lib/crt1.o $(OPTIONS_FOR_USING_LIBC) \"$$\@\" > $@
	chmod +x $@

cilly-with-libc:
	echo $(CILLY_CMD) $(OPTIONS_FOR_USING_LIBC) -lottersys -lotterposix \"$$\@\" > $@
	chmod +x $@


# Building Otter's system calls and POSIX functions

SYSCALL_OBJS := \
	stdlib/_malloc_r.o \
	sys/time/gettimeofday.o \
	otter/otter_fs.o \
	otter/otter_fs_util.o
SYSCALL_OBJS := $(addprefix syscalls/,$(SYSCALL_OBJS))
libottersys.a: OBJ_FILES := $(SYSCALL_OBJS)
libottersys.a: $(SYSCALL_OBJS)

POSIX_OBJS := \
	fcntl/open.o \
	fcntl/__otter_fcntl.o \
	unistd/close.o \
	unistd/read.o \
	unistd/write.o \
	unistd/lseek.o \
	unistd/__otter_unistd.o \
	sys/stat/fstat.o \
	sys/stat/lstat.o \
	sys/stat/__otter_stat.o \
	sys/socket/__otter_socket.o 
POSIX_OBJS := $(addprefix posix/,$(POSIX_OBJS))
libotterposix.a: OBJ_FILES := $(POSIX_OBJS)
libotterposix.a: $(POSIX_OBJS)

# Make 'make' delete the .o files when it's done
.INTERMEDIATE: $(SYSCALL_OBJS) $(POSIX_OBJS)

libotter%.a: CC:='$(CURDIR)/cilly-with-libc' -I syscalls/include
libotter%.a: cilly-with-libc
	$(CILLY_CMD) --mode=AR cru $(LIB_DIR)/$@ $(OBJ_FILES)


# Building newlib

$(BUILD_DIR)/otter/newlib/%.a: $(BUILD_DIR)/Makefile
	make -C $(BUILD_DIR) all-target-newlib

$(BUILD_DIR)/Makefile: otter-ar $(CONFIG_FILES) $(MAKE_FILES) $(NEWLIB_1_19_0)/newlib/configure.host
	mkdir -p $(BUILD_DIR)
	cd $(BUILD_DIR) && \
	$(NEWLIB_1_19_0)/configure --target=otter --with-newlib --disable-multilib \
		CC_FOR_TARGET='$(OTTER_CC) $(OPTIONS_FOR_BUILDING_LIBC)' \
		AR_FOR_TARGET='$(OTTER_AR)' \
		RANLIB_FOR_TARGET='$(OTTER_RANLIB)'


# Extracting the merged files from the built files (which contain both a CIL archive and an ar archive)

$(LIB_DIR)/libc.a: $(BUILD_DIR)/otter/newlib/libc/libc.a
$(LIB_DIR)/libm.a: $(BUILD_DIR)/otter/newlib/libm.a
$(MERGED_FILES):
	mkdir -p $(LIB_DIR)
	$(call MV,$<,$@)

# Listing all functions in newlib, libottersys and libotterposix
list: 
	 $(OTTER_TRUNK)/otter/otter.pl --merge --dofindFns -Llib -lc -lm -lottersys -lotterposix > library_functions

# Cleaning up

.PHONY: clean distclean clean-% distclean-%

%-native: SUFFIX:=.native

clean-%:
	if [[ -f $(BUILD_DIR)/Makefile ]]; then make -C $(BUILD_DIR) clean; fi
	rm -rf $(LIB_DIR)

distclean-%:
	if [[ -f $(BUILD_DIR)/Makefile ]]; then make -C $(BUILD_DIR) distclean || rm -rf $(BUILD_DIR); fi

CLEAN_CMD :=rm -f $(OTTER_EXES) $(SYSCALL_OBJS) $(POSIX_OBJS)

clean: $(addprefix clean-,$(TARGETS))
	$(CLEAN_CMD)

distclean: $(addprefix distclean-,$(TARGETS))
	$(CLEAN_CMD)
	rm -rf $(CONFIG_FILES) $(CONFIG_FILES:configure=aclocal.m4) $(CONFIG_FILES:configure=autom4te.cache) $(MAKE_FILES) $(LIB_DIR) $(LIB_DIR).native


# Running autotools. I modeled these rules after Otter's top-level makefile. Thanks, Yit.

.PRECIOUS : %/aclocal.m4 %/configure %/Makefile

%/aclocal.m4: $(NEWLIB_1_19_0)/newlib/acinclude.m4
	cd $* && aclocal -I $(NEWLIB_1_19_0)/newlib

%/configure : %/configure.in %/aclocal.m4
	cd $* && autoconf

%/Makefile.in : %/Makefile.am %/configure
	cd $* && automake --cygnus Makefile
