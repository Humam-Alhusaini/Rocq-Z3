clean-smt2:
	rm -f *.smt2

COQBIN?=

%: Makefile.rocq

Makefile.rocq: _CoqProject
	$(COQBIN)rocq makefile -f _CoqProject -o Makefile.rocq

-include Makefile.rocq
