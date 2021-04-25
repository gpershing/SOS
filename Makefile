.PHONY : all
all: sos.native util_math.o util_opengl.o

# Creates the main compiler
sos.native:
	opam config exec -- \
	ocamlbuild -use-ocamlfind sos.native

util_math: util_math.c
	cc -lm -o util_math -DBUILD_TEST util_math.c

util_opengl: util_opengl.c
	cc -o util_opengl -DBUILD_TEST util_opengl.c -I/usr/local/include/ -L/user/local/lib/ -lOSMesa -lGLU -lm

.PHONY : clean

test: 
	./testall.sh

clean : 
	ocamlbuild -clean
	rm util_math.o util_opengl.o

