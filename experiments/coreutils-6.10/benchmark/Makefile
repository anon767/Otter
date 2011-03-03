trunk=$(PWD)/../../..
coreutils=$(PWD)/..
newlib=$(trunk)/newlib-1.19.0/otter

gcc_with_libc_configure=$(newlib)/gcc-with-libc-configure.py

cilly_with_libc=$(newlib)/cilly-with-libc
cilly_ar=$(trunk)/cil/bin/cilly --merge --mode=AR
runotter=$(coreutils)/benchmark/runotter
runbackotter=$(coreutils)/benchmark/runbackotter
libcoreutils_functions=$(coreutils)/benchmark/libcoreutils_functions

procedure=configure make list runotter runbackotter

all: $(procedure)

.PHONY: $(procedure)

configure:
	cd $(coreutils) && ./configure SKIP_FTRUNCATE_CHECK=yes CC="$(gcc_with_libc_configure)" LD=ld --host=i386

make: 
	cd $(coreutils) && make CC="$(cilly_with_libc)" AR="$(cilly_ar)" RANLIB="echo cilly_ranlib: "

# Generate a list of functions defined in lib/.
# They are excluded from coreutils' coverage.
list:
	$(trunk)/otter/otter.pl --merge --dofindFns -L../lib -lcoreutils  > $(libcoreutils_functions)

# Generate a script to run Otter on coreutils' programs.
runotter:
	cat runotter.in \
		| sed "s|@OTTER_TRUNK@|\"$(trunk)\"|" \
		| sed "s|@OTTER_FLAVOR@|--doexecute|" \
		> $(runotter) && chmod +x $(runotter)

# Generate a script to run BackOtter on coreutils' programs.
runbackotter:
	cat runotter.in \
		| sed "s|@OTTER_TRUNK@|\"$(trunk)\"|" \
		| sed "s|@OTTER_FLAVOR@|--dobackotter|" \
		> $(runbackotter) && chmod +x $(runbackotter)

clean:
	rm $(coreutils)/src/*_comb.c
	rm $(coreutils)/benchmark/{libcoreutils_functions,runotter,runbackotter}
	cd $(coreutils) && make clean

distclean: clean
	cd $(coreutils) && make distclean

