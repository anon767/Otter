TRUNK=../../..
INCLUDEDIR=-nostdinc -fno-builtin -Werror-implicit-function-declaration \
		   -I $(TRUNK)/includes/usr/include  \
		   -I $(TRUNK)/includes/usr/local/gcc-4.0.1/lib/gcc/i686-pc-linux-gnu/4.0.1/include
LIBCDIR=$(TRUNK)/libc
MOCKEDDIR=$(TRUNK)/mockedFns
CILLY=CILLY_DONT_COMPILE_AFTER_MERGE= $(TRUNK)/cil/bin/cilly --merge --warnall --disallowDuplication -D_FILE_OFFSET_BITS=64


CILTEMPSDIR=CILTEMPSDIR
CILEXTRAFILE=___extra_files

run: print_tokens_comb.c
	$(CILLY) $(INCLUDEDIR) print_tokens_comb.c --doexecute $(XARG)

lib_comb.c: 
	mkdir -p $(CILTEMPSDIR)
	$(CILLY) --useLogicalOperators --save-temps=$(CILTEMPSDIR) \
		-I $(LIBCDIR) -I $(MOCKEDDIR) \
		$(LIBCDIR)/*.c  $(MOCKEDDIR)/*.c \
		--out=lib_comb.c $(INCLUDEDIR)
	rm -rf $(CILEXTRAFILE) $(CILTEMPSDIR)

print_tokens_comb.c: lib_comb.c print_tokens_sym.c
	mkdir -p $(CILTEMPSDIR)
	$(CILLY) --useLogicalOperators --save-temps=$(CILTEMPSDIR) \
		-I $(LIBCDIR) -I $(MOCKEDDIR) \
		lib_comb.c print_tokens_sym.c \
		 --out=print_tokens_comb.c $(INCLUDEDIR)
	rm -rf $(CILEXTRAFILE) $(CILTEMPSDIR)

clean:
	rm -rf lib_comb.c print_tokens_comb.c $(CILEXTRAFILE) $(CILTEMPSDIR)
