all: grader

grader: src/grader.ml Makefile.coq src/graderplugin.ml src/graderplugin.mllib
	+make -f Makefile.coq
	ocamlopt unix.cmxa src/grader.ml -o grader

Makefile.coq: Make
	+coq_makefile -f Make -o Makefile.coq

clean:
	-make -f Makefile.coq clean
	rm -rf Makefile.coq
