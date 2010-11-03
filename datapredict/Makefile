TARGETS = predict.otarget instrument.otarget
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

INSTR_MODULES = \
	utils.ml \
	DPGlobs.ml \
	data_instr.ml \

instrument: $(INSTR_MODULES)
	$(OCAMLBUILD) -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS),'cil.cmxa' instrument.otarget

clean:
	$(OCAMLBUILD) -clean
