trunk=$(PWD)/../../../..
coreutils=$(PWD)/..
build=$(PWD)/build.cil
newlib=$(trunk)/newlib-1.19.0/otter

gcc_with_libc_configure=$(newlib)/gcc-with-libc-configure.py

cilly_with_libc=$(newlib)/cilly-with-libc
cilly_ar=$(trunk)/cil/bin/cilly --merge --mode=AR
libcoreutils_functions=$(build)/libcoreutils_functions

all: make list 

make: $(build)/Makefile 
	$(MAKE) -C $(build) CC="$(cilly_with_libc)" AR="$(cilly_ar)" RANLIB="echo cilly_ranlib: " SUBDIRS="lib src"

# coreutils must be built in its source tree because the line-targets benchmark specify files relative to that directory
# *The above statement might have been violated*
$(build)/Makefile:
	mkdir -p  $(build)
	cd $(build) && $(coreutils)/configure SKIP_FTRUNCATE_CHECK=yes CC="$(gcc_with_libc_configure)" LD=ld --host=i386

# Generate a list of functions defined in lib/.
# They are excluded from coreutils' coverage.
list: make
	$(trunk)/otter/otter.pl --merge --dofindFns -L$(build)/lib -lcoreutils  > $(libcoreutils_functions)

clean:
	$(RM) $(build)/src/*_comb.c
	$(RM) $(libcoreutils_functions)
	$(MAKE) -C $(build) clean

distclean:
	$(RM) -r $(build)


# A ultimate target that builds everything (otter, newlib and coreutils)
universe:
	svn up $(trunk)
	make -C $(trunk)
	-make -C $(newlib) distclean
	-rm $(newlib)/../build* # Sometimes these directories are not removed by the distclean above
	make -C $(newlib)
	make -C $(newlib) native
	make -C $(newlib) list
	-make distclean
	make

