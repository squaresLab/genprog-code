TARGETS = predict.otarget
OCAMLBUILD ?= ocamlbuild

CIL=/Users/csl9q/research/cil

OCAML_OPTIONS = '-thread','-I '$(CIL)'/','-I '$(CIL)'/src','-I '$(CIL)'/src/ext','-I '$(CIL)'/src/frontc','-I '$(CIL)'/obj/x86_DARWIN'


COPTIONS = \
  -I $(CIL)/ \
  -I $(CIL)/src \
  -I $(CIL)/src/ext \
  -I $(CIL)/src/frontc \
  -I $(CIL)/obj/x86_DARWIN

OCAMLC =        ocamlc                          $(COPTIONS)
OCAMLOPT =      ocamlopt                        $(COPTIONS)
OCAMLDEP =      ocamldep                        $(COPTIONS)
OCAMLLEX =      ocamllex 

all: predict instrument 
#	$(OCAMLBUILD) -cflags $(OCAML_OPTIONS) -lflags $(OCAML_OPTIONS),'cil.cmxa batteries.cmxa' predict.otarget
#	$(OCAMLOPT) -o $@ unix.cmxa str.cmxa cil.cmxa $^

PREDICATE_MODULES = \
	utils.ml \
	DPGlobs.ml \
	globals.ml \
	invariant.ml \
	memory.ml \
	state.ml \
	graph.ml \
	predict.ml \
	belprop.ml \
	main.ml \

predict: $(PREDICATE_MODULES)
	$(OCAMLBUILD) -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS),'cil.cmxa batteries.cmxa' predict.otarget

%.cmo: %.ml 
	@if [ -f $*.mli -a ! -f $*.cmi ] ; then $(OCAMLC) -c -g $*.mli ; fi 
	$(OCAMLC) -c -g $*.ml
	@$(OCAMLDEP) $*.ml > $*.d 

%.cmx: %.ml 
	@if [ -f $*.mli -a ! -f $*.cmi ] ; then $(OCAMLC) -c -g $*.mli ; fi 
	$(OCAMLOPT) -c $*.ml
	@$(OCAMLDEP) $*.ml > $*.d 

%.cmi: %.mli
	$(OCAMLC) -c -g $*.mli

%.ml: %.mll
	$(OCAMLLEX) $*.mll

INSTR_MODULES = \
	utils.cmo \
	DPGlobs.cmo \
	data_instr.cmo \

instrument: $(INSTR_MODULES:.cmo=.cmx)
	$(OCAMLOPT) -o $@ unix.cmxa str.cmxa cil.cmxa $^

clean:
	$(OCAMLBUILD) -clean; rm -f *.cmo *.cmi *.d *.cmx *.dx *.o instrument
