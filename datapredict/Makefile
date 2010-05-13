# Makefile for Weimer's Genetic Programming Prototype Tool

# You must set the CIL environment variable for this to work. It should
# point to the directory with cil.spec in it. Mine is:
# /home/weimer/src/cil 

CIL=/Users/csl9q/research/cil

OCAML_OPTIONS = \
  -I $(CIL)/ \
  -I $(CIL)/src \
  -I $(CIL)/src/ext \
  -I $(CIL)/src/frontc \
  -I $(CIL)/obj/x86_DARWIN

OCAMLC =        ocamlc                          $(OCAML_OPTIONS)
OCAMLOPT =      ocamlopt                        $(OCAML_OPTIONS)
OCAMLDEP =      ocamldep                        $(OCAML_OPTIONS)
OCAMLLEX =      ocamllex 

###
#
# You should not have to change anything below this line. 
#
###

# We use an internal utility to auto-generate token information,
# visitor code and pretty-printing code from ocaml type definitions. 
# If you don't change "tokens.type" or "jabs.ml" you won't need this. 

all: predict instrument

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

# NOTE: Module order is important!  OCaml module dependencies cannot
# be cyclic, and the order presented must respect the dependency order.

PREDICATE_MODULES = \
	globals.cmo \
	state.cmo \
	graph.cmo \
	main.cmo \

predict: $(PREDICATE_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ unix.cmxa str.cmxa cil.cmxa $^

INSTR_MODULES = \
	data_instr.cmo \

instrument: $(INSTR_MODULES:.cmo=.cmx)
	$(OCAMLOPT) -o $@ unix.cmxa str.cmxa cil.cmxa $^

clean:
	rm -f *.cmo *.cmi *.d *.cmx *.dx *.o predict instrument
