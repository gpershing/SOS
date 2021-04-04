# Creates the main compiler
sos.native:
	opam config exec -- \
	ocamlbuild -use-ocamlfind sos.native

.PHONY : clean

test: 
	./tests/testall.sh

clean : 
	ocamlbuild -clean
