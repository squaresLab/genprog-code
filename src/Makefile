# Makefile for Program Repair Tool (v2) 

# You must set the CIL environment variable for this to work. It should
# point to the directory with cil.spec in it. Mine is:
# /home/claire/src/cil 

GEN_OPTS = \
  -I $(CIL)/ \
  -I $(CIL)/src \
  -I $(CIL)/src/ext \
  -I $(CIL)/src/frontc

# change this to $(CIL)/obj/x86_DARWIN on OS X
OCAML_OPTIONS = $(GEN_OPTS) -I $(CIL)/obj/x86_LINUX

OCAMLC   = ocamlc -g $(OCAML_OPTIONS)
OCAMLOPT = ocamlopt -w Aelzv $(OCAML_OPTIONS)
OCAMLDEP = ocamldep $(OCAML_OPTIONS)
OCAMLLEX = ocamllex
OCAMLDOC = ocamldoc $(OCAML_OPTIONS)

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

REPAIR_MODULES = \
  elf.cmo \
  stats2.cmo \
  global.cmo \
  distglobal.cmo \
  cdiff.cmo \
  template.cmo \
  rep.cmo \
  stringrep.cmo \
  gaussian.cmo \
  elfrep.cmo \
  asmrep.cmo \
  fitness.cmo \
  minimization.cmo \
  cilprinter.cmo \
  cilrep.cmo \
  population.cmo \
  search.cmo \
  multiopt.cmo \
  $(PELLACINI) \
  network.cmo \
  main.cmo

repair: $(REPAIR_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ bigarray.cmxa unix.cmxa str.cmxa nums.cmxa cil.cmxa $^ -cclib -l_elf_stubs $(ELF_OPTS_OCAML)

repair.byte: $(REPAIR_MODULES)
	$(OCAMLC) -o $@ bigarray.cma unix.cma nums.cma str.cma cil.cma $^ -cclib -l_elf_stubs $(ELF_OPTS_OCAML)

NHT_MODULES = \
  global.cmo \
  nhtserver.cmo

nhtserver: $(NHT_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ nums.cmxa unix.cmxa str.cmxa $^

DIST_SERVER_MODULES = \
  global.cmo \
  distglobal.cmo \
  distserver.cmo

distserver: $(DIST_SERVER_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ nums.cmxa unix.cmxa str.cmxa $^

###
#
# Integration with a simple C utility for elf manipulation:
#
###

libelf.o: libelf.c 
	gcc -c -o libelf.o -I"`$(OCAMLC) -where`" -fPIC $<

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
	nhtserver.cmo

-include $(ALL_MODULES:.cmo=.d)

testsuite: ../test/Makefile
	cd ../test/ && make

doc: $(ALL_MODULES:.cmo=.ml)
	$(OCAMLDOC) -html -d ../doc/ $^

clean:
	rm -f repair nhtserver repair.byte *.mli *.cmo *.cmi *.d *.cmx *.dx *.o lib_elf_stubs.a dll_elf_stubs.so $(ALL) ../doc/*
