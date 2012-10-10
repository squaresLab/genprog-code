# Makefile for Program Repair Tool (v2)

# You may need to set the CIL environment variable for this to
# work. It should point to the base of the CI source directory, mine
# is /usr/local/src/cil.
#
# If the CIL is not set then the ocamlfind utility will be used to set
# the CIL environment variable.

OS=$(shell uname)
ifeq ($(OS),Linux)
	OS=LINUX
 endif
ifeq ($(OS),Darwin)
	OS=DARWIN
endif

ifneq ($(CIL),)
	OCAML_OPTIONS = -I $(CIL)/obj/x86_$(OS)
else
  ifneq ($(shell type ocamlfind 2> /dev/null),)
	OCAML_OPTIONS = -I $(shell ocamlfind query cil)
  else
    ifeq ($(MAKECMDGOALS),)
$(error Please set the CIL environment variable)
    else
      ifneq ($(filter-out clean,$(MAKECMDGOALS)),)
$(error Please set the CIL environment variable)
      endif
    endif
  endif
endif

OCAMLC   = ocamlc -g $(OCAML_OPTIONS)
OCAMLOPT = ocamlopt -w Aelzv $(OCAML_OPTIONS)
OCAMLDEP = ocamldep $(OCAML_OPTIONS)
OCAMLLEX = ocamllex
OCAMLDOC = ocamldoc $(OCAML_OPTIONS)

MBITS := $(if $(shell touch null.ml ; $(OCAMLOPT) -c null.ml ; file null.o | grep 64 ; rm null.*),-m64,-m32)

# We use an internal utility to auto-generate token information,
# visitor code and pretty-printing code from ocaml type definitions. 
# If you don't change "tokens.type" or "jabs.ml" you won't need this. 

ALL = repair
all: $(ALL)

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

BASE_MODULES = \
  stats2.cmo \
  global.cmo \
  cdiff.cmo \
  rep.cmo \
  fitness.cmo \
  stringrep.cmo \
  minimization.cmo \
  cilprinter.cmo \
  cilrep.cmo \
  population.cmo \
  search.cmo \

REPAIR_MODULES = \
  $(BASE_MODULES) \
  main.cmo

STANDARD_LIBS = \
	bigarray.cmxa \
	unix.cmxa \
	str.cmxa \
	nums.cmxa

repair: $(REPAIR_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ $(STANDARD_LIBS) cil.cmxa $^

-include $(ALL_MODULES:.cmo=.d)

testsuite: ../test/Makefile
	cd ../test/ && make

doc: $(ALL_MODULES:.cmo=.ml) $(ALL_MODULES:.cmo=.cmx) ../README.txt
	$(OCAMLDOC) -html -d ../doc/ ../README.txt $(ALL_MODULES:.cmo=.ml)

clean:
	rm -f repair repair.byte *.mli *.cmo *.cmi *.d *.cmx *.dx *.o $(ALL) ../doc/*
