TOP=$(PWD)/../..
CILQUALDIR=$(TOP)/cilqual

PRELUDE=$(PWD)/null_prelude.c

CILQUAL=$(CILQUALDIR)/cilqual.pl
CILQUAL_FLAGS=--merge --keepmerged --stats
CILQUAL.merge="env CILLY_DONT_COMPILE_AFTER_MERGE=1 $(CILQUAL) $(CILQUAL_FLAGS) $(CILQUAL_EXTRAFLAGS)"

all : muh-2.2a_comb.c

muh-2.2a_comb.c : muh-2.2a/src/muh_comb.c
	ln -s $< $@

muh-2.2a/src/muh_comb.c : make//muh-2.2a
make//muh-2.2a : MAKEGOALS=CC=$(CILQUAL.merge)
make//muh-2.2a : CILQUAL_FLAGS+=--cilqual-save-dot=$(PWD)/muh.dot -include $(PRELUDE)
make//muh-2.2a : cilqual clean//muh-2.2a
clean//muh-2.2a : $(PRELUDE)
	$(MAKE) -C muh-2.2a clean
	$(RM) muh-2.2a_comb.c

clean : clean//muh-2.2a

cilqual :
	$(MAKE) -C $(TOP) cilqual

.PRECIOUS : %/Makefile cilqual
%/Makefile : %/Makefile.in %/configure Makefile
	cd $(@D) && ./configure $(CONFIGURE_FLAGS) $(CONFIGURE_EXTRAFLAGS)

%/Makefile : %/configure Makefile
	cd $(@D) && ./configure $(CONFIGURE_FLAGS) $(CONFIGURE_EXTRAFLAGS)

.PRECIOUS : %/configure
%/configure : %/configure.ac
	cd $(@D) && autoreconf

%/configure : %/configure.in
	cd $(@D) && autoreconf

make//% : %/Makefile
	-$(MAKE) -C $(@F) $(MAKEGOALS)
