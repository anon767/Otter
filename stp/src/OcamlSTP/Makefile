TOP = ../..
include $(TOP)/scripts/Makefile.common


OCAMLFLAGS = -w A -warn-error A
OCAMLLDFLAGS = -linkall -cclib -lstdc++
OCAMLRUNPARAM = b
OCAMLDOCDIR = doc

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDOC = ocamldoc

VALGRIND := $(shell which valgrind)
OCAMLINCDIR := $(shell $(OCAMLC) -where)

CXXFLAGS += -I$(OCAMLINCDIR)


all : lib doc

test : test-native-code

lib : libOcamlSTP.a OcamlSTP.a OcamlSTP.cmi OcamlSTP.cma OcamlSTP.cmxa


OcamlSTP.a OcamlSTP.cmi OcamlSTP.cma OcamlSTP.cmxa : OcamlSTP.mli OcamlSTP.ml libOcamlSTP.a libstp.a
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLLDFLAGS) -a -o OcamlSTP.cmxa OcamlSTP.mli OcamlSTP.ml -cclib -lOcamlSTP -cclib -lstp
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLLDFLAGS) -a -custom -o OcamlSTP.cma OcamlSTP.mli OcamlSTP.ml -cclib -lOcamlSTP -cclib -lstp

libOcamlSTP.a : OcamlSTP_impl.o
	$(RM) libOcamlSTP.a
	$(AR) qcs libOcamlSTP.a OcamlSTP_impl.o

# make local copy and link to it, to avoid adding spurious autolink flags to the Ocaml archives
libstp.a : $(TOP)/lib/libstp.a
	ln -f $< $@


doc : doc/index.html

doc/index.html : OcamlSTP.mli OcamlSTP.ml | OcamlSTP.cmi
	mkdir -p $(OCAMLDOCDIR)
	$(OCAMLDOC) -html -d $(OCAMLDOCDIR) $^


test-native-code : lib
	$(OCAMLOPT) -g -I . -o TestOcamlSTP.native OcamlSTP.cmxa TestOcamlSTP.ml -verbose
	OCAMLRUNPARAM=$(OCAMLRUNPARAM) $(VALGRIND) ./TestOcamlSTP.native

test-byte-code : lib
	$(OCAMLC) -g -I . -o TestOcamlSTP.byte OcamlSTP.cma TestOcamlSTP.ml -verbose
	OCAMLRUNPARAM=$(OCAMLRUNPARAM) $(VALGRIND) ./TestOcamlSTP.byte


clean ::
	$(RM) libstp.a
	$(RM) libOcamlSTP.a OcamlSTP_impl.o
	$(RM) $(addprefix OcamlSTP.,cmi cma cmxa cmi cmo cmx o a)
	$(RM) $(addprefix TestOcamlSTP.,native byte cmi cmo cmx o)
	$(RM) -r $(OCAMLDOCDIR)
	$(RM) depend


depend: OcamlSTP_impl.cpp
	@$(call makedepend,$@,OcamlSTP_impl.cpp)

-include depend
