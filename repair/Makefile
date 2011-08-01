# Makefile for Program Repair Tool (v2) 

# You must set the CIL environment variable for this to work. It should
# point to the directory with cil.spec in it. Mine is:
# /home/weimer/src/cil 


ifeq ($(CLAIRE_MAC),true)
export CIL=/Users/csl9q/research/cil-1.3.7
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

REPAIR_MODULES = \
  elf.cmo \
  stats2.cmo \
  global.cmo \
  gaussian.cmo \
  ../ga/cdiff.cmo \
  rep.cmo \
  stringrep.cmo \
  elfrep.cmo \
  asmrep.cmo \
  jast.cmo \
  javarep.cmo \
  cilprinter.cmo \
  cilrep.cmo \
  cilpatchrep.cmo \
  minimization.cmo \
  sourcereader.cmo \
  diffprocessor.cmo \
  fitness.cmo \
  search.cmo \
  multiopt.cmo \
  $(PELLACINI) \
  network.cmo \
  main.cmo

repair: $(REPAIR_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ bigarray.cmxa unix.cmxa str.cmxa cil.cmxa $^ -cclib -l_wrap_stubs $(ELF_OPTS_OCAML)

repair.byte: $(REPAIR_MODULES)
	$(OCAMLC) -o $@ bigarray.cma unix.cma str.cma cil.cma $^ -cclib -l_wrap_stubs $(ELF_OPTS_OCAML)

NHT_MODULES = \
  global.cmo \
  nhtserver.cmo

nhtserver: $(NHT_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ unix.cmxa str.cmxa $^

###
#
# Integration with the lisp library for elf manipulation:
#  located at http://gitweb.adaptive.cs.unm.edu/elf.git
#  in the "shared-library" branch
#
###
ELF_SO_EXISTS := $(wildcard libelf.so)
ifeq ($(strip $(ELF_SO_EXISTS)),)

ELF_OPTS=
ELF_OPTS_OCAML=-ccopt -L.
LIBELF_SO=

wrap.o: wrap_noelf.c 
	gcc -c -o wrap.o -I"`$(OCAMLC) -where`" -fPIC $< $(ELF_OPTS)

else

ELF_OPTS=-L. -L/usr/local/lib -lelf -lecl
ELF_OPTS_OCAML=-ccopt -L/usr/local/lib -ccopt -L. -cclib -lelf -cclib -lecl
LIBELF_SO=libelf.so

wrap.o: wrap.c libelf.so
	gcc -c -I"`$(OCAMLC) -where`" -fPIC $< $(ELF_OPTS)

endif


dll_wrap_stubs.so: wrap.o $(LIBELF_SO)
	ocamlmklib -o _wrap_stubs $< $(ELF_OPTS)

elf.mli: elf.ml
	$(OCAMLC) -i $< > $@

elf.cmi: elf.mli
	$(OCAMLC) -c $<

elf.cmo: elf.ml elf.cmi
	$(OCAMLC) -c $<

elf.cma:  elf.cmo  dll_wrap_stubs.so
	$(OCAMLC) -a  -o $@  $< -dllib -l_wrap_stubs $(ELF_OPTS_OCAML)

elf.cmx: elf.ml elf.cmi dll_wrap_stubs.so
	$(OCAMLOPT) -c $<

elf.cmxa: elf.cmx dll_wrap_stubs.so
	$(OCAMLOPT) -a  -o $@  $< -cclib -l_wrap_stubs $(ELF_OPTS_OCAML)

elfrep.cma: elfrep.cmo dll_wrap_stubs.so
	$(OCAMLC) -a  -o $@  $< -dllib -l_wrap_stubs $(ELF_OPTS_OCAML)

elfmain.mli: elfmain.ml
	$(OCAMLC) -i $< > $@

elfmain.cmi: elfmain.mli
	$(OCAMLC) -c $<

elfmain.cmx: elfmain.ml elfmain.cmi dll_wrap_stubs.so
	$(OCAMLOPT) -c $<

# dependencies
ALL_MODULES = \
  $(REPAIR_MODULES) \
  $(NHT_MODULES)

-include $(ALL_MODULES:.cmo=.d)

clean:
	rm -f *.mli *.cmo *.cmi *.d *.cmx *.dx *.o lib_wrap_stubs.a dll_wrap_stubs.so $(ALL)
