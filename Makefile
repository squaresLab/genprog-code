# Makefile for Program Repair Tool (v2) 

# You must set the CIL environment variable for this to work. It should
# point to the directory with cil.spec in it. Mine is:
# /home/weimer/src/cil 

# Uncomment the next line to build with Pellacini support 
# USE_PELLACINI=1

OCAML_OPTIONS = \
  -I $(CIL)/ \
  -I $(CIL)/src \
  -I $(CIL)/src/ext \
  -I $(CIL)/src/frontc \
  -I $(CIL)/obj/x86_LINUX 

OCAMLC =        ocamlc         -g                 $(OCAML_OPTIONS)
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

ifdef USE_PELLACINI 
PELLACINI = pellacini.cmo cgrep.cmo 
endif

REPAIR_MODULES = \
  stats2.cmo \
  global.cmo \
  cdiff.cmo \
  rep.cmo \
  stringrep.cmo \
  asmrep.cmo \
  jast.cmo \
  javarep.cmo \
  cilrep.cmo \
  fitness.cmo \
  search.cmo \
  multiopt.cmo \
  $(PELLACINI) \
  main.cmo \

repair: $(REPAIR_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ bigarray.cmxa unix.cmxa str.cmxa cil.cmxa $^

###
#
# Building integration with the shared lisp library for elf manipulation:
#
#   Hopefully the pre-compiled library will work for you and you won't
#   need to install the dev version of ECL to compile libelf.so.
#
###
ELF_OPTS=-L. -L/usr/local/lib -lelf -lecl
ELF_OPTS_OCAML=-ccopt -L/usr/local/lib -ccopt -L. -cclib -lelf -cclib -lecl

wrap.o: wrap.c libelf.so
	gcc -c -I"`$(OCAMLC) -where`" -fPIC $< $(ELF_OPTS)

dll_wrap_stubs.so: wrap.o libelf.so
	ocamlmklib -o _wrap_stubs $< $(ELF_OPTS)

elf.mli: elf.ml
	$(OCAMLC) -i $< > $@

elf.cmi: elf.mli
	$(OCAMLC) -c $<

elf.cmo: elf.ml elf.cmi
	$(OCAMLC) -c $<

elf.cma:  elf.cmo  dll_wrap_stubs.so
	$(OCAMLC) -a  -o $@  $< -dllib -l_wrap_stubs $(ELF_OPTS_OCAML)

elf.cmx: elf.ml elf.cmi
	$(OCAMLOPT) -c $<

elf.cmxa:  elf.cmx  dll_wrap_stubs.so
	$(OCAMLOPT) -a  -o $@  $< -cclib -l_wrap_stubs $(ELF_OPTS_OCAML)

elfrep.cma: elfrep.cmo
	$(OCAMLC) -a  -o $@  $< -dllib -l_wrap_stubs $(ELF_OPTS_OCAML)

repair-elf: $(REPAIR_MODULES:.cmo=.cmx) elf.cmx elfrep.cmx
	$(OCAMLOPT) -o $@ bigarray.cmxa unix.cmxa str.cmxa cil.cmxa elf.cmxa $^ -cclib -l_wrap_stubs $(ELF_OPTS_OCAML)

# dependencies
ALL_MODULES = \
  $(REPAIR_MODULES) 

-include $(ALL_MODULES:.cmo=.d)

clean:
	rm -f *.cmo *.cmi *.d *.cmx *.dx *.o $(ALL)
