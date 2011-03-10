trunk=$(PWD)/../../..
coreutils=$(PWD)/..
newlib=$(trunk)/newlib-1.19.0/otter

gcc_with_libc_configure=$(newlib)/gcc-with-libc-configure.py

cilly_with_libc=$(newlib)/cilly-with-libc
cilly_ar=$(trunk)/cil/bin/cilly --merge --mode=AR
runotter=$(coreutils)/benchmark/runotter
runbackotter=$(coreutils)/benchmark/runbackotter
libcoreutils_functions=$(coreutils)/benchmark/libcoreutils_functions

all: make list runotter runbackotter

make: $(coreutils)/Makefile
	$(MAKE) -C $(coreutils) CC="$(cilly_with_libc)" AR="$(cilly_ar)" RANLIB="echo cilly_ranlib: " SUBDIRS="lib src"

# coreutils must be built in its source tree because the line-targets benchmark specify files relative to that directory
$(coreutils)/Makefile:
	cd $(coreutils) && ./configure SKIP_FTRUNCATE_CHECK=yes CC="$(gcc_with_libc_configure)" LD=ld --host=i386

# Generate a list of functions defined in lib/.
# They are excluded from coreutils' coverage.
list: make
	$(trunk)/otter/otter.pl --merge --dofindFns -L$(coreutils)/lib -lcoreutils  > $(libcoreutils_functions)

# Generate a script to run Otter on coreutils' programs.
runotter: runotter.in
	cat runotter.in \
		| sed "s|@OTTER_TRUNK@|\"$(trunk)\"|" \
		| sed "s|@OTTER_FLAVOR@|--doexecute|" \
		> $(runotter) && chmod +x $(runotter)

# Generate a script to run BackOtter on coreutils' programs.
runbackotter: runotter.in
	cat runotter.in \
		| sed "s|@OTTER_TRUNK@|\"$(trunk)\"|" \
		| sed "s|@OTTER_FLAVOR@|--dobackotter|" \
		> $(runbackotter) && chmod +x $(runbackotter)

clean:
	$(RM) $(coreutils)/src/*_comb.c
	$(RM) $(coreutils)/benchmark/{libcoreutils_functions,runotter,runbackotter}
	$(MAKE) -C $(coreutils) clean

distclean:
	$(RM) $(coreutils)/src/*_comb.c
	$(RM) $(coreutils)/benchmark/{libcoreutils_functions,runotter,runbackotter}
	$(MAKE) -C $(coreutils) distclean


# A ultimate target that builds everything (otter, newlib and coreutils)
universe:
	svn up $(trunk)
	make -C $(trunk)
	make -C $(newlib) distclean
	-rm $(newlib)/../build* # Sometimes these directories are not removed by the distclean above
	make -C $(newlib) 
	make -C $(newlib) native
	make -C $(newlib) list
	make distclean
	make

