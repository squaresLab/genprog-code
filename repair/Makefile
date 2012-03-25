# Makefile for Program Repair Tool (v2) 

# You must set the CIL environment variable for this to work. It should
# point to the directory with cil.spec in it. Mine is:
# /home/weimer/src/cil 


ifeq ($(CLAIRE_MAC),true)
export CIL=/Users/claire/research/cil-1.3.7
endif
GEN_OPTS = \
  -I $(CIL)/ \
  -I ../ga/ \
  -I $(CIL)/src \
  -I $(CIL)/src/ext \
  -I $(CIL)/src/frontc

ifdef CLAIRE_MAC
OCAML_OPTIONS = $(GEN_OPTS) -I $(CIL)/obj/x86_DARWIN
else
OCAML_OPTIONS = $(GEN_OPTS) -I $(CIL)/obj/x86_LINUX
endif

OCAMLC   = ocamlc -g $(OCAML_OPTIONS)
OCAMLOPT = ocamlopt -w Aelzv $(OCAML_OPTIONS)
OCAMLDEP = ocamldep $(OCAML_OPTIONS)
OCAMLLEX = ocamllex

###
#
# You should not have to change anything below this line. 
#
###

# We use an internal utility to auto-generate token information,
# visitor code and pretty-printing code from ocaml type definitions. 
# If you don't change "tokens.type" or "jabs.ml" you won't need this. 

ALL = repair nhtserver
all: $(ALL)


main.cmx : main.ml
	$(OCAMLOPT) -pp "camlp4o pa_macro.cmo $(ELFOPTS2)" -c main.ml

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

ifndef DONT_USE_ELF
ELF1 = elf.cmo 
ELF2 = elfrep.cmo
ELFOPTS1 = -cclib -l_elf_stubs $(ELF_OPTS_OCAML)
ELFOPTS2 = -DELF
endif

REPAIR_MODULES = \
  $(ELF1) \
  stats2.cmo \
  global.cmo \
  gaussian.cmo \
  ../ga/cdiff.cmo \
  rep.cmo \
  stringrep.cmo \
  $(ELF2) \
  asmrep.cmo \
  cilprinter.cmo \
  cilrep.cmo \
  sourcereader.cmo \
  diffprocessor.cmo \
  minimization.cmo \
  fitness.cmo \
  search.cmo \
  multiopt.cmo \
  $(PELLACINI) \
  network.cmo \
  main.cmo

repair: $(REPAIR_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ bigarray.cmxa unix.cmxa str.cmxa nums.cmxa cil.cmxa $^ $(ELFOPTS1)

repair.byte: $(REPAIR_MODULES)
	$(OCAMLC) -o $@ bigarray.cma unix.cma nums.cma str.cma cil.cma $^ $(ELFOPTS1)

NHT_MODULES = \
  global.cmo \
  nhtserver.cmo

nhtserver: $(NHT_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ nums.cmxa unix.cmxa str.cmxa $^

###
#
# Integration with a simple C utility for elf manipulation:
#
###
ELF_OPTS_OCAML=-ccopt -L.

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
  $(NHT_MODULES)

-include $(ALL_MODULES:.cmo=.d)

clean:
	rm -f repair nhtserver repair.byte *.mli *.cmo *.cmi *.d *.cmx *.dx *.o lib_elf_stubs.a dll_elf_stubs.so $(ALL)
