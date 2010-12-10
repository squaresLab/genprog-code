# Makefile

#tmp: lexer.cmo

all: main

export OCAMLRUNPARAM=l=256M

# directories
ELKHOUND := ./elsa-2005.08.22b/elkhound/

# compiler options
OCAMLCFLAGS := -noassert
#OCAMLCFLAGS :=

#OCAMLOPTFLAGS := $(OCAMLCFLAGS) -p
OCAMLOPTFLAGS := $(OCAMLCFLAGS)

# using dependency strategy from 
# http://www.cs.berkeley.edu/~smcpeak/autodepend/autodepend.html
%.cmo: %.ml
	ocamlopt -c -g $(OCAMLCFLAGS) $*.ml
	@ocamldep $*.ml > $*.d

# I'll try maintaining separate dependency info for .cmo and .cmx
%.cmx: %.ml
	ocamlopt -c $(OCAMLOPTFLAGS) $*.ml
	@ocamldep $*.ml > $*.dx

# run elkhound on a grammar spec
# (for now I don't want to use the interface stuff)
# 
# cygwin bug...

%.ml: %.mlm
	m4 $*.mlm > $*.ml

%.ml: %.gr
	m4 $*.gr > $*.grp
	$(ELKHOUND)/elkhound -ocaml -v -o $* $*.grp
	rm -f $*.mli

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
MAIN_MODULES = \
  pretty.cmo \
  escape.cmo \
  errormsg.cmo \
  longarray.cmo \
  growarray.cmo \
  cabs.cmo \
  trace.cmo \
  cabshelper.cmo \
  cabsvisit.cmo \
  whitetrack.cmo \
  cprint.cmo \
  lexerhack.cmo \
  $(MINIMAL_MODULES) \
  clexer.cmo \
  diffparser.cmo \
  diffparse.cmo \
  cparser.cmo \
  lrparse.cmo \
  ptreenode.cmo \
  ptreeact.cmo \
  cparse.cmo \
  main.cmo

main: $(MAIN_MODULES:.cmo=.cmx)
	ocamlopt -o $@ -g $^


# dependencies
ALL_MODULES = \
  $(MAIN_MODULES)

-include $(ALL_MODULES:.cmo=.d)
-include $(ALL_MODULES:.cmo=.dx)

clean:
	rm -f *.cmo *.cmi *.d *.cmx *.dx
	rm -f main cparser.ml diffparser.ml cparse.ml diffparse.ml clexer.ml
