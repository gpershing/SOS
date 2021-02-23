# builds a ./test executable
test : parser.cmo scanner.cmo parsertest.cmo
	ocamlc -o test $^

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

# dependencies
parsertest.cmo : scanner.cmo parser.cmi ast.cmi
parsertest.cmx : scanner.cmx parser.cmx ast.cmi
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml test
