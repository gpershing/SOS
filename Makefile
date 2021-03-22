sastprint : parser.cmo scanner.cmo semant.cmo sastprint.cmo
	ocamlc -o sastprint $^

# builds a ./test executable
astprint : parser.cmo scanner.cmo astprint.cmo
	ocamlc -o astprint $^

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

# dependencies
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
