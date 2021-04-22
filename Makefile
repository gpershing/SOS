.PHONY : all
all: sos.native util_math.o

# Creates the main compiler
sos.native:
	opam config exec -- \
	ocamlbuild -use-ocamlfind sos.native

util_math: util_math.c
	cc -lm -o util_math -DBUILD_TEST util_math.c

.PHONY : clean

test: 
	./tests/testall.sh

clean : 
	ocamlbuild -clean
	rm util_math.o
