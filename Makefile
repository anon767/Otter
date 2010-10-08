
SUBDIRS=cilqual cil ocamlstp stp camlidl ocaml-base-noparser ocamlgraph

EXTRALIBDIRS=$(addprefix $(CURDIR)/,camlidl/runtime stp/lib ocamlstp)
EXTRAOCAMLPATH=$(CURDIR)

CTAGS_FILE=tags
CTAGS_SOURCE_PATHS=otter/src cil/src ocaml-base-noparser/lib ocamlstp


# augment configuration from Makefile.local, if it exists
-include Makefile.local


all : otter


cil : make//cil
cil : MAKEGOALS=
make//cil : \
	CONFIGURE_FLAGS= \
		EXTRALIBDIRS='$(EXTRALIBDIRS)' \
		EXTRAOCAMLPATH='$(EXTRAOCAMLPATH)' \
		CC='$(CC) -m32'


otter : make//otter
otter : MAKEGOALS=
libs-otter : make//otter
libs-otter : MAKEGOALS=libs
test-otter : make//otter
test-otter : MAKEGOALS=test
doc-otter : make//otter
doc-otter : MAKEGOALS=doc
dot-otter : make//otter
dot-otter : MAKEGOALS=dot
make//otter : \
	CONFIGURE_FLAGS= \
		EXTRALIBDIRS='$(EXTRALIBDIRS)' \
		EXTRAOCAMLPATH='$(EXTRAOCAMLPATH)' \
		--with-cil='$(CURDIR)/cil'
make//otter : cil ocamlstp ocaml-base-noparser ocamlgraph


cilqual : make//cilqual
cilqual : MAKEGOALS=
test-cilqual : make//cilqual
test-cilqual : MAKEGOALS=test
debug-cilqual : make//cilqual
debug-cilqual : MAKEGOALS=debug
experiments-cilqual : make//cilqual
experiments-cilqual : MAKEGOALS=experiments
make//cilqual : \
	CONFIGURE_FLAGS=\
		EXTRALIBDIRS='$(EXTRALIBDIRS)' \
		EXTRAOCAMLPATH='$(EXTRAOCAMLPATH)' \
		--with-cil='$(CURDIR)/cil' \
		--with-otter='$(CURDIR)/otter'
make//cilqual : libs-otter ocamlgraph


ocamlstp : make//ocamlstp
make//ocamlstp : \
	MAKEGOALS= \
		CAMLIDL='$(CURDIR)/camlidl/compiler/camlidl' \
		LIBDIRS='$(CURDIR)/camlidl/runtime $(CURDIR)/stp/lib' \
		INCDIRS='$(CURDIR)/camlidl/runtime $(CURDIR)/stp/c_interface'
make//ocamlstp : stp camlidl


stp : make//stp
make//stp : MAKEGOALS=
make//stp : CONFIGURE_FLAGS=--with-prefix=.


camlidl : make//camlidl
make//camlidl : MAKEGOALS=


ocaml-base-noparser : make//ocaml-base-noparser
make//ocaml-base-noparser : MAKEGOALS=


ocamlgraph : make//ocamlgraph
make//ocamlgraph : MAKEGOALS=


ctags :
	$(RM) $(CTAGS_FILE) && \
	find $(CTAGS_SOURCE_PATHS) -iname "*.ml" -exec ctags -a -f $(CTAGS_FILE) {} \;

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
