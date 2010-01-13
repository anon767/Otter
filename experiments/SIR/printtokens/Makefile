TRUNK=../../..
INCLUDEDIR=-nostdinc -fno-builtin -Werror-implicit-function-declaration \
		   -I $(TRUNK)/includes/usr/include  \
		   -I $(TRUNK)/includes/usr/local/gcc-4.0.1/lib/gcc/i686-pc-linux-gnu/4.0.1/include
LIBCDIR=$(TRUNK)/libc
MOCKEDDIR=$(TRUNK)/mockedFns
CILLY=CILLY_DONT_COMPILE_AFTER_MERGE= $(TRUNK)/cil/bin/cilly --merge --warnall --disallowDuplication -D_FILE_OFFSET_BITS=64


CILTEMPSDIR=CILTEMPSDIR
CILEXTRAFILE=___extra_files

all:
	echo "(No default)"

#
# DAIKON TEST
#

# change the dir in your system
DAIKON_DIR=~/UMD/daikon/
DAIKON_OUTPUT=daikon-output
DAIKON_PPT_LIST_FILE=ppt-list-file

print_tokens_kvasir.exe: print_tokens_original.c
	gcc -gdwarf-2 -O0 print_tokens_original.c -o print_tokens_kvasir.exe

run_kvasir_dump-ppt-file: print_tokens_kvasir.exe
	$(DAIKON_DIR)/bin/kvasir-dtrace --dump-ppt-file=$(DAIKON_PPT_LIST_FILE) ./print_tokens_kvasir.exe

run_kvasir: print_tokens_kvasir.exe
	mkdir -p $(DAIKON_OUTPUT)
	for f in `ls inputs/*`; \
		do \
			echo $$f ; \
			~/UMD/daikon/bin/kvasir-dtrace \
				--with-dyncomp \
				--ppt-list-file=$(DAIKON_PPT_LIST_FILE) \
				--dtrace-file=daikon-output/`basename $$f`.dtrace \
				--decls-file=daikon-output/`basename $$f`.decls \
				./print_tokens_kvasir.exe $$f ; \
		done


#
# SYMBOLIC TEST
#

run_symbolic: print_tokens_comb.c
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

#
# CLEAN
#

clean:
	rm -rf lib_comb.c print_tokens_comb.c print_tokens_kvasir.exe 
	rm -rf $(CILEXTRAFILE) $(CILTEMPSDIR)

distclean: clean
	rm -rf $(DAIKON_OUTPUT) $(DAIKON_PPT_LIST_FILE)


