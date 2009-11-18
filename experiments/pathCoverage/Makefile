CILOBJDIR=../../cil/obj/x86_LINUX
EXTRALIBDIRS = ../../camlidl/runtime ../../stp/lib ../../ocamlstp /usr/local/lib/ocaml
STPVCFILE=../../ocamlstp/stpvc.cmxa

guaranteedCoverage_printPath: guaranteedCoverage_printPath.ml
	ocamlopt -dtypes -c -I $(CILOBJDIR) guaranteedCoverage_printPath.ml
	ocamlopt -ccopt "$(foreach lib,$(EXTRALIBDIRS),-L$(lib))" -I $(CILOBJDIR) -o guaranteedCoverage_printPath unix.cmxa str.cmxa $(STPVCFILE) $(CILOBJDIR)/cil.cmxa guaranteedCoverage_printPath.cmx
