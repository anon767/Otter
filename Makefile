
SUBDIRS=cilqual cil ocamlstp stp camlidl syck ocamlsyck
EXTRALIBDIRS = $(addprefix $(CURDIR)/,camlidl/runtime stp/lib ocamlstp ocamlsyck/yaml)


all : cil


cil : make//cil
cil : MAKEGOALS=
test-cil : make//cil
test-cil : MAKEGOALS=ounit
make//cil : CONFIGURE_FLAGS=EXTRALIBDIRS='$(EXTRALIBDIRS)' CC='gcc -m32'
make//cil : ocamlstp ocamlsyck


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


ocamlsyck : SYCKLIB=$(CURDIR)/syck/lib
ocamlsyck : CONFIGURE_FLAGS=LDFLAGS=-L$(SYCKLIB) CPPFLAGS=-I$(SYCKLIB)
ocamlsyck : make//ocamlsyck
	cd ocamlsyck && $(CONFIGURE_FLAGS) ./compile_cma
make//ocamlsyck : MAKEGOALS=
make//ocamlsyck : SYCKLIB=$(CURDIR)/syck/lib
make//ocamlsyck : CONFIGURE_FLAGS=LDFLAGS=-L$(SYCKLIB) CPPFLAGS=-I$(SYCKLIB)
make//ocamlsyck : syck 


syck : make//syck
make//syck : MAKEGOALS=
#make//syck : MAKEGOALS=all check


camlidl : make//camlidl


clean :
	$(foreach foo,$(SUBDIRS),$(MAKE) -C $(foo) clean;) true

distclean :
	$(foreach foo,$(SUBDIRS),$(MAKE) -C $(foo) clean distclean;) true

.PRECIOUS : %/Makefile
%/Makefile : %/Makefile.in %/configure Makefile
	cd $* && ./configure $(CONFIGURE_FLAGS) $(CONFIGURE_EXTRAFLAGS)

%/Makefile : %/configure Makefile
	cd $* && ./configure $(CONFIGURE_FLAGS) $(CONFIGURE_EXTRAFLAGS)

.PRECIOUS : %/configure
%/configure : %/configure.ac
	cd $* && autoreconf

%/configure : %/configure.in
	cd $* && autoreconf

make//% : %/Makefile
	$(MAKE) -C $* $(MAKEGOALS)
