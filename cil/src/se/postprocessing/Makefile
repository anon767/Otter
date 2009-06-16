CILOBJDIR=../../../obj/x86_LINUX
EXTRALIBDIRS = ../../../../camlidl/runtime ../../../../stp/lib ../../../../ocamlstp /usr/local/ocaml-3.11.0/lib/ocaml
STPVCFILE=../../../../ocamlstp/stpvc.cmxa

guaranteedCoverage: guaranteedCoverage.ml
	ocamlopt -annot -c -I $(CILOBJDIR) guaranteedCoverage.ml
	ocamlopt -ccopt "$(foreach lib,$(EXTRALIBDIRS),-L$(lib))" -I $(CILOBJDIR) -o guaranteedCoverage unix.cmxa str.cmxa $(STPVCFILE) $(CILOBJDIR)/cil.cmxa guaranteedCoverage.cmx
