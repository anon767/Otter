CILOBJDIR=../../cil/obj/x86_LINUX
EXTRALIBDIRS = ../../camlidl/runtime ../../stp/lib ../../ocamlstp /usr/local/lib/ocaml
STPVCFILE=../../ocamlstp/stpvc.cmxa

BZ2DIR=camlbz2-0.6.0
BZ2FILE=$(BZ2DIR)/bz2.cmxa

UUENCODEDIR=uuencode
UUENCODEFILE=$(UUENCODEDIR)/uuencode.cmxa

OBJDIRS=$(CILOBJDIR) $(BZ2DIR) $(UUENCODEDIR)

all: guaranteedPathCoverage uncompressPaths

guaranteedPathCoverage: guaranteedPathCoverage.ml $(UUENCODEFILE) $(BZ2FILE)
	ocamlopt -dtypes -c $(foreach dir,$(OBJDIRS),-I $(dir)) guaranteedPathCoverage.ml
	ocamlopt -ccopt "$(foreach lib,$(EXTRALIBDIRS),-L$(lib))" $(foreach dir,$(OBJDIRS),-I $(dir)) -o guaranteedPathCoverage unix.cmxa str.cmxa $(STPVCFILE) $(BZ2FILE) $(UUENCODEFILE) $(CILOBJDIR)/cil.cmxa guaranteedPathCoverage.cmx

uncompressPaths: uncompressPaths.ml $(UUENCODEFILE) $(BZ2FILE)
	ocamlopt -o uncompressPaths -I $(UUENCODEDIR) $(UUENCODEFILE) -I $(BZ2DIR) $(BZ2FILE) uncompressPaths.ml

$(UUENCODEFILE):
	make -C $(UUENCODEDIR)

$(BZ2FILE):
	cd $(BZ2DIR) && ./configure && make

clean:
	rm -f $(foreach suffix,cmi cmx o,guaranteedPathCoverage.$(suffix) uncompressPaths.$(suffix)) guaranteedPathCoverage.annot guaranteedPathCoverage uncompressPaths

clean-all: clean
	make -C $(UUENCODEDIR) clean
	make -C $(BZ2DIR) clean
