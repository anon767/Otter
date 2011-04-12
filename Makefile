
SUBDIRS=cilqual cil stp ocamlgraph

EXTRALIBDIRS=$(addprefix $(CURDIR)/,stp/src/OcamlSTP)
EXTRAOCAMLPATH=$(CURDIR):$(CURDIR)/stp/src

CTAGS_FILE=tags
CTAGS_SOURCE_PATHS=otter/src otter/benchmark otter/test cil/src


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
benchmark-otter : make//otter
benchmark-otter : MAKEGOALS=benchmark-otter
doc-otter : make//otter
doc-otter : MAKEGOALS=doc
dot-otter : make//otter
dot-otter : MAKEGOALS=dot
make//otter : \
	CONFIGURE_FLAGS= \
		EXTRALIBDIRS='$(EXTRALIBDIRS)' \
		EXTRAOCAMLPATH='$(EXTRAOCAMLPATH)' \
		--with-cil='$(CURDIR)/cil'
make//otter : cil stp-ocamlstp ocamlgraph delimcc


cilqual : make//cilqual
cilqual : MAKEGOALS=
test-cilqual : make//cilqual
test-cilqual : MAKEGOALS=test
debug-cilqual : make//cilqual
debug-cilqual : MAKEGOALS=debug
experiments-cilqual : make//cilqual
experiments-cilqual : MAKEGOALS=experiments
make//cilqual : \
	CONFIGURE_FLAGS= \
		EXTRALIBDIRS='$(EXTRALIBDIRS)' \
		EXTRAOCAMLPATH='$(EXTRAOCAMLPATH)' \
		--with-cil='$(CURDIR)/cil' \
		--with-otter='$(CURDIR)/otter'
make//cilqual : libs-otter ocamlgraph


stp-ocamlstp : make//stp
stp-ocamlstp : MAKEGOALS=OcamlSTP
test-stp-ocamlstp : make//stp
test-stp-ocamlstp : MAKEGOALS=test-OcamlSTP
# STP's configure script is non-standard
stp/Makefile : stp/scripts/Makefile.in stp/scripts/configure
	cd stp && scripts/configure --with-fpic


ocamlgraph : make//ocamlgraph
make//ocamlgraph : MAKEGOALS=
make//ocamlgraph : CONFIGURE_FLAGS=


delimcc : make//delimcc
make//delimcc : MAKEGOALS=all opt


ctags :
	$(RM) $(CTAGS_FILE) && \
	find $(CTAGS_SOURCE_PATHS) -iname "*.ml" -exec ctags -a -f $(CTAGS_FILE) {} \;

clean :
	$(foreach foo,$(SUBDIRS),$(MAKE) -C $(foo) clean;) true

distclean :
	$(foreach foo,$(SUBDIRS),$(MAKE) -C $(foo) clean distclean;) true

.PRECIOUS : %/Makefile %/config.h
%/Makefile %/config.h : %/Makefile.in %/config.h.in %/configure Makefile
	cd $* && ./configure $(CONFIGURE_FLAGS) $(CONFIGURE_EXTRAFLAGS) && touch config.h

%/Makefile : %/Makefile.in %/configure Makefile
	cd $* && ./configure $(CONFIGURE_FLAGS) $(CONFIGURE_EXTRAFLAGS)

%/Makefile : %/configure Makefile
	cd $* && ./configure $(CONFIGURE_FLAGS) $(CONFIGURE_EXTRAFLAGS)

.PRECIOUS : %/configure
%/configure : %/configure.ac
	cd $* && autoreconf

%/configure : %/configure.in
	cd $* && autoreconf

make//% : %/Makefile %/config.h
	$(MAKE) -C $* $(MAKEGOALS)

make//% : %/Makefile
	$(MAKE) -C $* $(MAKEGOALS)
