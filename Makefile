
SUBDIRS=cilqual cil ocamlstp stp camlidl
EXTRALIBDIRS = $(addprefix $(CURDIR)/,camlidl/runtime stp/lib ocamlstp)


all : cil


cil : make//cil
cil : MAKEGOALS=
test-cil : make//cil
test-cil : MAKEGOALS=ounit
make//cil : CONFIGURE_FLAGS=EXTRALIBDIRS='$(EXTRALIBDIRS)'
make//cil : ocamlstp


cilqual : make//cilqual
cilqual : MAKEGOALS=
test-cilqual : make//cilqual
test-cilqual : MAKEGOALS=test
make//cilqual : CONFIGURE_FLAGS=EXTRALIBDIRS='$(EXTRALIBDIRS)' --with-cil='$(CURDIR)/cil'
make//cilqual : cil


ocamlstp : make//ocamlstp
make//ocamlstp : MAKEGOALS=CAMLIDL='../camlidl/compiler/camlidl' \
	                      LIBDIRS='../camlidl/runtime ../stp/lib' \
					      INCDIRS='../camlidl/runtime ../stp/c_interface'
make//ocamlstp : stp camlidl


stp : make//stp
make//stp : MAKEGOALS=
make//stp : CONFIGURE_FLAGS=--with-prefix=.


camlidl : make//camlidl


clean :
	$(foreach foo,$(SUBDIRS),$(MAKE) -C $(foo) clean;)


.PRECIOUS : %/Makefile
%/Makefile : %/Makefile.in %/configure Makefile
	cd $(@D) && ./configure $(CONFIGURE_FLAGS)

%/Makefile : %/configure Makefile
	cd $(@D) && ./configure $(CONFIGURE_FLAGS)

.PRECIOUS : %/configure
%/configure : %/configure.ac
	cd $(@D) && autoreconf

make//% : %/Makefile
	$(MAKE) -C $(@F) $(MAKEGOALS)
