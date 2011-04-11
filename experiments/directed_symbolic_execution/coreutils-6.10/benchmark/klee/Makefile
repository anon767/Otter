klee_gcc = $$(which klee-gcc)

coreutils := $(PWD)/../..
coreutils_src := $(coreutils)/src
coreutils_lib := $(coreutils)/lib
coreutils_files := $(coreutils_src)/*.c $(coreutils_src)/*.h $(coreutils_lib)/*.c $(coreutils_lib)/*.h

build_llvm := $(PWD)/build.llvm
build_gcov := $(PWD)/build.gcov

otter_poi_src := $(PWD)/../__otter_poi.c

all: llvm-make

# llvm

$(build_llvm)/__otter_poi.o: $(otter_poi_src)
	cd $(build_llvm) && $(klee_gcc) $(otter_poi_src) -c

$(build_llvm)/Makefile: 
	mkdir -p $(build_llvm)
	cd $(build_llvm) && $(coreutils)/configure --disable-nls CFLAGS="-g"

llvm-make: $(build_llvm)/Makefile $(coreutils_files) $(build_llvm)/__otter_poi.o
	$(MAKE) -C $(build_llvm) CC="$(klee_gcc) $(build_llvm)/__otter_poi.o" SUBDIRS="lib src"


# gcov

gcov_cflags = -g -fprofile-arcs -ftest-coverage

$(build_gcov)/__otter_poi.o: $(otter_poi_src)
	cd $(build_gcov) && gcc $(otter_poi_src) -c $(gcov_cflags)

$(build_gcov)/Makefile: 
	mkdir -p $(build_gcov)
	cd $(build_gcov) && $(coreutils)/configure --disable-nls CFLAGS="$(gcov_cflags)"

gcov-make: $(build_gcov)/Makefile $(coreutils_files) $(build_gcov)/__otter_poi.o
	$(MAKE) -C $(build_gcov) CC="gcc $(build_gcov)/__otter_poi.o" SUBDIRS="lib src"

