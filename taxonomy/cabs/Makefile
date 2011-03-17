# Makefile

#tmp: lexer.cmo

all: main diffparse

export OCAMLRUNPARAM=l=256M

# directories
ELKHOUND := ./elsa-2005.08.22b/elkhound/

# compiler options
OCAMLCFLAGS := -noassert
OCAMLOPTFLAGS := $(OCAMLCFLAGS)

export CIL=/Users/csl9q/research/cil-1.3.7/

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

# run elkhound on a grammar spec
# (for now I don't want to use the interface stuff)
# 
# cygwin bug...

%.ml: %.mlm
	m4 $*.mlm > $*.ml

%.ml: %.gr
	m4 $*.gr > $*.grp
	$(ELKHOUND)/elkhound -ocaml -v -o $* $*.grp
	rm $*.mli

diffparser.ml: diffparser.gr partial.gr
	m4 diffparser.gr > diffparser.grp
	$(ELKHOUND)/elkhound -ocaml -v -o diffparser diffparser.grp
	rm -f diffparser.mli

%.mll: %.mllm tokens.lexer tokens.type
	m4 $*.mllm > $*.mll

# run ocamllex on a lex spec
%.ml: %.mll
	ocamllex $*.mll

# NOTE: Module order is important!  OCaml module dependencies cannot
# be cyclic, and the order presented must respect the dependency order.

# these are the minimal set of modules needed for an elkhound parser
MINIMAL_MODULES = \
  smutil.cmo \
  useract.cmo \
  lexerint.cmo \
  parsetables.cmo \
  arraystack.cmo \
  objpool.cmo \
  glr.cmo

# these are modules for the test program; they include some specific 
# grammar tables/actions, a lexer, a deterministic LR parser, the
# parse-tree-printing stuff (ptreenode, ptreeact), and a main() function
LIB_MODULES = \
  diffabs.cmo \
  diffvisit.cmo \
  diffprint.cmo \
  lexerhack.cmo \
  $(MINIMAL_MODULES) \
  difflexer.cmo \
  diffparser.cmo \
  actions.cmo \
  diffparse.cmo \
  mycparser.cmo \
  lrparse.cmo \
  ptreenode.cmo \
  ptreeact.cmo \
  mycparse.cmo \
  diff2cil.cmo 

MAIN_MODULES = \
  $(LIB_MODULES) \
  main.cmo

diffparse: $(MAIN_MODULES:.cmo=.cmx)
	$(OCAMLOPT) -a -o $@.cmxa -g $^

main: $(MAIN_MODULES:.cmo=.cmx)
	$(OCAMLOPT) -o $@ str.cmxa unix.cmxa cil.cmxa $^ 


# dependencies
ALL_MODULES = \
  $(MAIN_MODULES)

-include $(ALL_MODULES:.cmo=.d)
-include $(ALL_MODULES:.cmo=.dx)

clean:
	rm -f *.cmo *.cmi *.d *.cmx *.dx *.grp *.o
	rm -f main cparser.ml diffparser.ml cparse.ml diffparse.ml difflexer.ml difflexer.mll
