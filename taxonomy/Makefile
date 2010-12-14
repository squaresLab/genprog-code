OCAMLBUILD ?= ocamlbuild

#OCAML_OPTIONS = '-thread','-I /Users/csl9q/research/genprog/code-cbi/','-I '$(CIL)'/','-I '$(CIL)'/src','-I '$(CIL)'/src/ext','-I '$(CIL)'/src/frontc','-I '$(CIL)'/obj/x86_DARWIN'
OCAML_OPTIONS = '-thread','-I /Users/csl9q/research/genprog/code-cbi/','-I /Users/csl9q/research/taxonomy/cabs/'

all: taxonomy

taxonomy:
	$(OCAMLBUILD) -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS) main.native

clean:
	$(OCAMLBUILD) -clean
