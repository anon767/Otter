klee_gcc = $$(which klee-gcc)

coreutils := $(PWD)/../..
coreutils_src := $(coreutils)/src
coreutils_lib := $(coreutils)/lib
coreutils_files := $(coreutils_src)/*.c $(coreutils_src)/*.h $(coreutils_lib)/*.c $(coreutils_lib)/*.h

build_llvm := $(PWD)/build.llvm
build_gcov := $(PWD)/build.gcov

all: llvm-make

# llvm

$(build_llvm)/Makefile: 
	mkdir -p $(build_llvm)
	cd $(build_llvm) && $(coreutils)/configure --disable-nls CFLAGS="-g"

llvm-make: $(build_llvm)/Makefile $(coreutils_files)
	$(MAKE) -C $(build_llvm) CC="$(klee_gcc)" SUBDIRS="lib src"


# gcov

gcov_cflags = -g -fprofile-arcs -ftest-coverage

$(build_gcov)/Makefile: 
	mkdir -p $(build_gcov)
	cd $(build_gcov) && $(coreutils)/configure --disable-nls CFLAGS="$(gcov_cflags)"

gcov-make: $(build_gcov)/Makefile $(coreutils_files)
	$(MAKE) -C $(build_gcov) SUBDIRS="lib src"

