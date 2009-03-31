CIL_DIR=../../cil/
CIL=env CILLY_DONT_COMPILE_AFTER_MERGE= \
			$(CIL_DIR)/bin/cilly \
			-Ilibc \
			-I. \
			-fno-builtin \
			--domakeCFG \
			--merge \
			--keepmerged \
			--printAssign \
				$*

SYMTEST=symtest1
SRC=ngircd-0.12.0-patched.c
OPT=--printNothing

all: libccode mockcode ngircd.o 
	@echo "\nBuilding symtest...\n"
	$(CIL)  --doexecute $(OPT) $(SYMTEST).c ngircd.o libccode_comb.c mockcode_comb.c -o symtest


libccode:
	@echo "\nBuilding libc...\n"
	$(CIL)  -D__STUB_SIMPLIFY__ libc/*.c -o libccode

mockcode:
	@echo "\nBuilding mock...\n"
	$(CIL)  -include symtest_mock.h mock/*.c -o mockcode


ngircd.o: $(SRC) 
	@echo "\nBuilding ngircd...\n"
	$(CIL)  -D__STUB_SIMPLIFY__ -include elim_main.h -c $(SRC) -o ngircd.o

test: libccode
	$(CIL)  --doexecute $(TEST) libccode_comb.c -o $(TEST) 

clean:
	rm -f ___extra_files
	rm -f *_comb.c
	rm -f *.o

