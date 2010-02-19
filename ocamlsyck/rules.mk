# -*- Mode: Makefile -*-
# Copyright (c) 2007 Christopher R. Waterson

# Rules for building Caml stuff.

.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxa .cma

.PRECIOUS: %.cmi

.PHONY: all clean subdirs $(SUBDIRS)

# Include any C(PP)FLAGS specified in the environment
ifneq ($(CPPFLAGS),)
OCAMLCFLAGS	+= $(foreach fl,$(CPPFLAGS),-ccopt -Wp,$(fl))
endif

ifneq ($(CFLAGS),)
OCAMLCFLAGS	+= $(foreach fl,$(CFLAGS),-ccopt $(fl))
endif

# Include any LDFLAGS specified in the environment
ifneq ($(LDFLAGS),)
OCAMLLDFLAGS	+= $(foreach fl,$(LDFLAGS),-cclib $(fl))
endif

# Include any packages specified by the makefile.
ifneq ($(PACKAGES),)
comma		:= ,
empty		:=
space		:= $(empty) $(empty)
OCAMLPKGS	:= -package $(subst $(space),$(comma),$(PACKAGES))
PKGDEPS		:= $(foreach pkg,$(PACKAGES),\
			$(shell OCAMLPATH=$(DEPTH) $(OCAMLFIND) query \
			-predicates byte -format %d/%a $(pkg)))
endif

# Be noisy about the build if VERBOSE is set.
ifeq ($(VERBOSE),1)
OCAMLFLAGS	+= -verbose
endif

# ----------------------------------------------------------------------
#
# The rules.
#

%: $(srcdir)/%.ml $(PKGDEPS)
	OCAMLPATH=$(DEPTH) $(OCAMLFIND) $(OCAMLC) $(OCAMLPKGS) \
	-linkpkg $(OCAMLFLAGS) -o $@ $(filter %.$(OBJ),$^) \
	$(srcdir)/$*.ml

%: %.cmo
	OCAMLPATH=$(DEPTH) $(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -o $@ $^

%: %.cmx
	OCAMLPATH=$(DEPTH) $(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) -o $@ $^

%.cmi: $(srcdir)/%.mli
	OCAMLPATH=$(DEPTH) $(OCAMLFIND) $(OCAMLC) $(OCAMLPKGS) \
	$(OCAMLFLAGS) -o $@ -c $(srcdir)/$*.mli

%.o: $(srcdir)/%.c
	OCAMLPATH=$(DEPTH) $(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) -c $^

%.cmo: $(srcdir)/%.ml
	OCAMLPATH=$(DEPTH) $(OCAMLFIND) $(OCAMLC) $(OCAMLPKGS) \
	$(OCAMLFLAGS) -o $@ -c $(srcdir)/$*.ml

%.cmx: $(srcdir)/%.ml
	OCAMLPATH=$(DEPTH) $(OCAMLFIND) $(OCAMLOPT) $(OCAMLPKGS) \
	$(OCAMLFLAGS) -o $@ -c $(srcdir)/$*.ml

%.cma:
	$(OCAMLMKLIB) -o $* $^ $(LDFLAGS)

%.cmxa:
	$(OCAMLMKLIB) -o $* $^ $(LDFLAGS)

.PRECIOUS: %.ml

# ----------------------------------------------------------------------
#
# The targets.
#

DEPFILE := .depend

all: $(DEPFILE) $(TARGETS) subdirs

depend: $(DEPFILE) subdirs

clean: subdirs
	$(RM) -r *.cma *.cmxa *.cmx *.cmi *.cmx *.cmo *.o *.a *.so *.annot \
	$(TARGETS) $(DEPFILE) $(GARBAGE)

distclean: clean
	$(RM) -r Makefile $(DISTGARBAGE) *~

subdirs: $(SUBDIRS)

$(SUBDIRS):
	cd $@ && $(MAKE) $(MAKECMDGOALS)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
CAMLFILES=$(wildcard $(srcdir)/*.ml $(srcdir)/*.mli)
ifeq ($(CAMLFILES),)
$(DEPFILE):
else
$(DEPFILE): $(CAMLFILES)
	OCAMLPATH=$(DEPTH) $(OCAMLFIND) $(OCAMLDEP) $^ | \
	sed -e 's|$(srcdir)/||g' > $@
endif
-include $(DEPFILE)
endif
endif

