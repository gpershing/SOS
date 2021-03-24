.PHONY : all

all : sos.native

sos.native:
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm sos.native

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

%.cmx : %.ml
	ocamlfind ocamlopt -c -package llvm $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

# dependencies
codegen.cmo : ast.cmi sast.cmi
codegen.cmx : ast.cmx sast.cmi
sastprint.cmo : semant.cmo ast.cmi sast.cmi
sastprint.cmx : semant.cmx ast.cmi sast.cmi
semant.cmo : ast.cmi sast.cmi
semant.cmx : ast.cmi sast.cmi
parsertest.cmo : scanner.cmo parser.cmi ast.cmi
parsertest.cmx : scanner.cmx parser.cmx ast.cmi
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml sastprint astprint
