NAME = algen

all: byte opt
byte: $(NAME).cma
opt: $(NAME).cmxa

include make.common

ML_SOURCES = algen_intf.ml algen_impl.ml algen_vector.ml algen_matrix.ml

REQUIRES =

.PHONY: all install uninstall reinstall

ML_OBJS  = $(ML_SOURCES:.ml=.cmo)
ML_XOBJS = $(ML_SOURCES:.ml=.cmx)

$(NAME).cma: $(ML_OBJS)
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $(ML_OBJS)

$(NAME).cmxa: $(ML_XOBJS)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $(ML_XOBJS)

install: all
	if test -f $(NAME).cmxa ; then extra="$(NAME).cmxa *.cmx $(NAME).a" ; fi ; \
	ocamlfind install $(NAME) *.cmi algen_intf.ml $(NAME).cma META $$extra

uninstall:
	ocamlfind remove $(NAME)

reinstall: uninstall install

check: $(NAME).cma $(NAME).cmxa
	$(MAKE) -C tests all opt
	@for i in tests/*.byte tests/*.opt ; do $$i || exit ; done ; echo "Ok"

clean-spec:
	$(MAKE) -C tests clean

-include .depend
