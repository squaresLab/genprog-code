OCAMLBUILD ?= ocamlbuild

CIL=/Users/csl9q/research/cil

#OCAML_OPTIONS = '-thread','-I /Users/csl9q/research/genprog/code-cbi/','-I '$(CIL)'/','-I '$(CIL)'/src','-I '$(CIL)'/src/ext','-I '$(CIL)'/src/frontc','-I '$(CIL)'/obj/x86_DARWIN'

export OCAMLRUNPARAM=l=256M
OCAML_OPTIONS = '-thread'

all: main difflib

difflib:
	$(OCAMLBUILD) -classic-display -no-hygiene -cflags $(OCAML_OPTIONS) -lflags $(OCAML_OPTIONS) difflib.a
	$(OCAMLBUILD) -classic-display -no-hygiene -cflags $(OCAML_OPTIONS) -lflags $(OCAML_OPTIONS) difflib.cmxa

main: 
	$(OCAMLBUILD) -classic-display -no-hygiene -cflags $(OCAML_OPTIONS) -lflags $(OCAML_OPTIONS) main.native

clean:
	$(OCAMLBUILD) -clean
