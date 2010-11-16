TARGETS = taxonomy.otarget
OCAMLBUILD ?= ocamlbuild

CIL=/Users/csl9q/research/cil

OCAML_OPTIONS = '-thread','-I /Users/csl9q/research/genprog/code-cbi/','-I '$(CIL)'/','-I '$(CIL)'/src','-I '$(CIL)'/src/ext','-I '$(CIL)'/src/frontc','-I '$(CIL)'/obj/x86_DARWIN'

all: taxonomy

TAXONOMY_MODULES = \
	../genprog/utils.ml \
	main.ml \

taxonomy: $(PREDICATE_MODULES)
	$(OCAMLBUILD) -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS),'utils.cmx cil.cmxa batteries.cmxa' taxonomy.otarget

clean:
	$(OCAMLBUILD) -clean
