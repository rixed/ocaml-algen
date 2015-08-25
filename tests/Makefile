REQUIRES = algen
OCAMLPATH = ../..

PROGRAMS = test.byte bbox.byte matrix.byte
all: $(PROGRAMS)
opt: $(PROGRAMS:.byte=.opt)

include ../make.common

.cmo.byte:
	$(OCAMLC)   -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $^

.cmx.opt:
	$(OCAMLOPT) -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLOPTFLAGS) $^

-include .depend
