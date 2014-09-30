all: Makefile.coq
	+make -f Makefile.coq

Makefile.coq: Make
	+coq_makefile -f Make -o Makefile.coq

clean:
	-make -f Makefile.coq clean
	rm -rf Makefile.coq
