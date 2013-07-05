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
OCAMLOPT = ocamlopt -w Aelzv-7 $(OCAML_OPTIONS)
OCAMLDEP = ocamldep $(OCAML_OPTIONS)
OCAMLLEX = ocamllex
OCAMLDOC = ocamldoc $(OCAML_OPTIONS)

MBITS := $(if $(shell touch null.ml ; $(OCAMLOPT) -c null.ml ; file null.o | grep 64 ; rm null.*),-m64,-m32)

###
#
# You should not have to change anything below this line. 
#
###

# We use an internal utility to auto-generate token information,
# visitor code and pretty-printing code from ocaml type definitions. 
# If you don't change "tokens.type" or "jabs.ml" you won't need this. 

ALL = repair nhtserver distserver
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

ifdef USE_PELLACINI 
PELLACINI = pellacini.cmo cgrep.cmo 
endif
ELF_OPTS_OCAML=-ccopt -L.

BASE_MODULES = \
  elf.cmo \
  stats2.cmo \
  global.cmo \
  distglobal.cmo \
  cdiff.cmo \
  template.cmo \
  rep.cmo \
  fitness.cmo \
  stringrep.cmo \
  gaussian.cmo \
  elfrep.cmo \
  asmrep.cmo \
  minimization.cmo \
  cilprinter.cmo \
  golf.cmo \
  ptranal.cmo \
  knownfuns.cmo \
  progeq.cmo \
  cilrep.cmo \
  population.cmo \
  search.cmo \
  multiopt.cmo \
  $(PELLACINI) \
  network.cmo \
  llvmrep.cmo

REPAIR_MODULES = \
  $(BASE_MODULES) \
  main.cmo

STANDARD_LIBS = \
	bigarray.cmxa \
	unix.cmxa \
	str.cmxa \
	nums.cmxa

../obj/genprog.cmxa: $(REPAIR_MODULES:.cmo=.cmx)
	$(OCAMLOPT) -a -o $@ $^ -cclib -l_elf_stubs $(ELF_OPTS_OCAML)

lib: ../obj/genprog.cmxa

repair: $(REPAIR_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ $(STANDARD_LIBS) cil.cmxa $^ -cclib -l_elf_stubs $(ELF_OPTS_OCAML)

repair.byte: $(REPAIR_MODULES)
	$(OCAMLC) -o $@ $(STANDARD_LIBS:.cmxa=.cma) cil.cma $^ -cclib -l_elf_stubs $(ELF_OPTS_OCAML)

edit: $(BASE_MODULES:.cmo=.cmx) edit.cmx
	$(OCAMLOPT) -o $@ $(STANDARD_LIBS) cil.cmxa $^ -cclib -l_elf_stubs $(ELF_OPTS_OCAML)

edit.byte: $(BASE_MODULES) edit.cmo
	$(OCAMLC) -o $@ $(STANDARD_LIBS:.cmxa=.cma) cil.cma $^ -cclib -l_elf_stubs $(ELF_OPTS_OCAML)

NHT_MODULES = \
  global.cmo \
  nhtserver.cmo

nhtserver: $(NHT_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ $(STANDARD_LIBS) $^

DIST_SERVER_MODULES = \
  global.cmo \
  distglobal.cmo \
  distserver.cmo

distserver: $(DIST_SERVER_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ $(STANDARD_LIBS) $^

CDIFF_MODULES = \
	global.cmo \
	cdiff.cmo \
	minimization.cmo \
	cdiffmain.cmo

cdiff: $(CDIFF_MODULES:.cmo=.cmx)
	$(OCAMLOPT) -o $@ $(STANDARD_LIBS) cil.cmxa $^

###
#
# Integration with a simple C utility for elf manipulation:
#
###

libelf.o: libelf.c 
	gcc -c $(MBITS) -o libelf.o -I"`$(OCAMLC) -where`" -fPIC $<

dll_elf_stubs.so: libelf.o
	ocamlmklib -o _elf_stubs $< $(ELF_OPTS)

elf.mli: elf.ml
	$(OCAMLC) -i $< > $@

elf.cmi: elf.mli
	$(OCAMLC) -c $<

elf.cmo: elf.ml elf.cmi
	$(OCAMLC) -c $<


elf.cma:  elf.cmo  dll_elf_stubs.so
	$(OCAMLC) -a  -o $@  $< -dllib -l_elf_stubs $(ELF_OPTS_OCAML)

elf.cmx: elf.ml elf.cmi dll_elf_stubs.so
	$(OCAMLOPT) -c $<

elf.cmxa: elf.cmx dll_elf_stubs.so
	$(OCAMLOPT) -a  -o $@  $< -cclib -l_elf_stubs $(ELF_OPTS_OCAML)

elfrep.cma: elfrep.cmo dll_elf_stubs.so
	$(OCAMLC) -a  -o $@  $< -dllib -l_elf_stubs $(ELF_OPTS_OCAML)

# dependencies
ALL_MODULES = \
	$(REPAIR_MODULES) \
	distserver.cmo \
	nhtserver.cmo

-include $(ALL_MODULES:.cmo=.d)

testsuite: ../test/Makefile
	cd ../test/ && make

doc: $(ALL_MODULES:.cmo=.ml) $(ALL_MODULES:.cmo=.cmx) ../README.txt
	$(OCAMLDOC) -html -d ../doc/ ../README.txt $(ALL_MODULES:.cmo=.ml)

clean:
	rm -f repair nhtserver repair.byte cdiff *.mli *.cmo *.cmi *.d *.cmx *.dx *.o lib_elf_stubs.a dll_elf_stubs.so $(ALL) ../doc/*
