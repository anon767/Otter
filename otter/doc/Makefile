TRUNK= ../../
OBJ= \
	-I $(TRUNK)/cil/obj/x86_DARWIN/  \
	-I $(TRUNK)/ocamlstp/  \
	-I $(TRUNK)/ocamlsyck/yaml/  \
	-I $(TRUNK)/otter/_build/src/Otter/ \

all: html dot

html:
	mkdir -p html; \
	ocamldoc -v -html -d html $(OBJ) $(TRUNK)/otter/src/Otter/*.ml 

dot:
	mkdir -p dot; \
	ocamldoc -v -dot \
		-dot-reduce \
		-o dot/ocamldoc.out $(OBJ) $(TRUNK)/otter/src/Otter/*.ml; \
	dot -v -Tpdf dot/ocamldoc.out -o dot/ocamldoc.pdf

clean:
	rm -rf html dot

distclean: clean


