klee_gcc = $$(which klee-gcc)

coreutils_src_dir := $(PWD)/../../src
coreutils_lib_dir := $(PWD)/../../lib
coreutils_files := $(coreutils_src_dir)/*.c $(coreutils_src_dir)/*.h $(coreutils_lib_dir)/*.c $(coreutils_lib_dir)/*.h

llvm_dir := $(PWD)/obj-llvm
gcov_dir := $(PWD)/obj-gcov

otter_poi_src := $(PWD)/../__otter_poi.c

all: llvm-tests

# llvm

$(llvm_dir)/__otter_poi.o: $(otter_poi_src)
	cd $(llvm_dir) && $(klee_gcc) $(otter_poi_src) -c

llvm-configure: ../../configure
	cd $(llvm_dir) && ../../../configure --disable-nls CFLAGS="-g"

$(llvm_dir)/Makefile: llvm-configure

llvm-make: $(llvm_dir)/Makefile $(coreutils_files) $(llvm_dir)/__otter_poi.o
	cd $(llvm_dir) && make CC="$(klee_gcc) $(llvm_dir)/__otter_poi.o" SUBDIRS="lib src"

$(llvm_dir)/src/*.bc: llvm-make

llvm-tests: $(llvm_dir)/src/*.bc
	setup_klee_llvm_tests.sh $(llvm_dir)/src $(llvm_dir)/benchmark $(PWD)/ktest-monitor.sh

# gcov

gcov_cflags = -g -fprofile-arcs -ftest-coverage

$(gcov_dir)/__otter_poi.o: $(otter_poi_src)
	cd $(gcov_dir) && gcc $(otter_poi_src) -c $(gcov_cflags)

gcov-configure: ../../configure
	cd $(gcov_dir) && ../../../configure --disable-nls CFLAGS="$(gcov_cflags)"

$(gcov_dir)/Makefile: gcov-configure

gcov-make: $(gcov_dir)/Makefile $(coreutils_files) $(gcov_dir)/__otter_poi.o
	cd $(gcov_dir) && make CC="gcc $(gcov_dir)/__otter_poi.o" SUBDIRS="lib src"

