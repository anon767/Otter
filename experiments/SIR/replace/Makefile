all:
	echo "(No default)"

# SOURCE and EXECUTABLE
PROG_SRC=replace.c
PROG_EXE=replace.exe
COMB_SRC=replace_comb.c

#
# DAIKON TEST
#
# change the dir in your system
DAIKON_DIR=/home/kkma/UMD/trunk/daikon
KVASIR_OUTPUT=daikon-output
DAIKON_OUTPUT=daikon-output.txt
DAIKON_PPT_LIST_FILE=ppt-file
DAIKON_VAR_LIST_FILE=var-file
DAIKON_CLASSPATH=$(DAIKON_DIR)/jyaml-1.3.jar:$(DAIKON_DIR)/java:.
DAIKON_INVFILE=$(PROG_EXE).inv.gz
KVASIR_DTRACE=DAIKONDIR=$(DAIKON_DIR) $(DAIKON_DIR)/bin/kvasir-dtrace 

$(PROG_EXE): $(PROG_SRC)
	gcc -gdwarf-2 -O0 $(PROG_SRC) -o $(PROG_EXE)

run_kvasir_dump_ppt_file: $(PROG_EXE)
	if [ ! -f $(DAIKON_PPT_LIST_FILE) ]; then \
		$(KVASIR_DTRACE) --dump-ppt-file=$(DAIKON_PPT_LIST_FILE) ./$(PROG_EXE) ; \
	fi

run_kvasir_dump_var_file: $(PROG_EXE)
	if [ ! -f $(DAIKON_VAR_LIST_FILE) ]; then \
		$(KVASIR_DTRACE) --dump-var-file=$(DAIKON_VAR_LIST_FILE) ./$(PROG_EXE) ; \
	fi


INPUT_RATIO=100
run_kvasir: $(PROG_EXE) run_kvasir_dump_ppt_file run_kvasir_dump_var_file
	mkdir -p $(KVASIR_OUTPUT)
	for f in `find inputs`; \
		do \
			RANDOM=`hexdump -n4 -e\"%u\" /dev/urandom` ; \
			echo $$f ; \
			if [ -f $(KVASIR_OUTPUT)/$$f.dtrace ]; then \
				echo "(skipped)" ; \
			elif [ -d $$f ]; then \
				echo "(dir)" ;\
			elif [ $(INPUT_RATIO) -gt $$(($$RANDOM % 100))  ]; then \
				mkdir -p `dirname $(KVASIR_OUTPUT)/$$f.dtrace`; \
				$(KVASIR_DTRACE) \
					--with-dyncomp \
					--ppt-list-file=$(DAIKON_PPT_LIST_FILE) \
					--var-list-file=$(DAIKON_VAR_LIST_FILE) \
					--dtrace-file=$(KVASIR_OUTPUT)/$$f.dtrace \
					--decls-file=$(KVASIR_OUTPUT)/$$f.decls \
					./$(PROG_EXE) `cat argv-$$f` < $$f ; \
				fi \
		done


run_daikon: 
	echo $(DAIKON_DIR)
	java -classpath $(DAIKON_CLASSPATH) daikon.Daikon \
		-o $(DAIKON_INVFILE) \
		`find $(KVASIR_OUTPUT) -name "*.dtrace"` `find $(KVASIR_OUTPUT) -name "*.decls"` \
		> $(DAIKON_OUTPUT) 2>&1


#
# SYMBOLIC TEST
#

# change the dir in your system
TRUNK=../../..
INCLUDEDIR=-nostdinc -fno-builtin -Werror-implicit-function-declaration \
		   -I $(TRUNK)/includes/usr/include  \
		   -I $(TRUNK)/includes/usr/local/gcc-4.0.1/lib/gcc/i686-pc-linux-gnu/4.0.1/include
LIBCDIR=$(TRUNK)/libc
MOCKEDDIR=$(TRUNK)/mockedFns
CILLY=$(TRUNK)/otter/otter.pl \
			--merge \
			--warnall \
			--disallowDuplication \
			-D_FILE_OFFSET_BITS=64 \

## CILLY=CILLY_DONT_COMPILE_AFTER_MERGE= $(TRUNK)/cil/bin/cilly \
## 			--merge \
## 			--warnall \
## 			--disallowDuplication \
## 			-D_FILE_OFFSET_BITS=64 \


CILTEMPSDIR=CILTEMPSDIR
CILEXTRAFILE=___extra_files

run_symbolic: $(COMB_SRC)
	$(CILLY) $(INCLUDEDIR) $(COMB_SRC) \
		--doexecute \
		--printLittle \
		--yaml=pptmap.yml \
		--examfn=addstr \
		$(XARG)

run_partial_symbolic: $(COMB_SRC)
	$(CILLY) $(INCLUDEDIR) $(COMB_SRC) \
		--doexecute \
		--yaml=pptmap.yml \
		--entryfn=addstr \
		$(XARG)


lib_comb.c: 
	mkdir -p $(CILTEMPSDIR)
	$(CILLY) --useLogicalOperators --save-temps=$(CILTEMPSDIR) \
		-I $(LIBCDIR) -I $(MOCKEDDIR) \
		$(LIBCDIR)/*.c  $(MOCKEDDIR)/*.c \
		--out=lib_comb.c $(INCLUDEDIR)
	rm -rf $(CILEXTRAFILE) $(CILTEMPSDIR)

$(COMB_SRC): lib_comb.c $(PROG_SRC) 
	mkdir -p $(CILTEMPSDIR)
	$(CILLY) --useLogicalOperators --save-temps=$(CILTEMPSDIR) \
		-I $(LIBCDIR) -I $(MOCKEDDIR) \
		lib_comb.c $(PROG_SRC) \
		 --out=$(COMB_SRC) $(INCLUDEDIR)
	rm -rf $(CILEXTRAFILE) $(CILTEMPSDIR)

#
# CLEAN
#

clean:
	rm -rf lib_comb.c $(COMB_SRC) $(PROG_EXE) $(PROG_EXE).dSYM
	rm -rf $(CILEXTRAFILE) $(CILTEMPSDIR)
	rm -rf $(DAIKON_INVFILE)

distclean: clean
	rm -rf $(KVASIR_OUTPUT) $(DAIKON_PPT_LIST_FILE) $(DAIKON_VAR_LIST_FILE)


