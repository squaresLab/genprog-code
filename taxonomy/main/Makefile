OCAMLBUILD ?= ocamlbuild

OCAML_OPTIONS ='-thread','-I /Users/csl9q/research/','-I /Users/csl9q/research/taxonomy/cabs/_build','-I /Users/csl9q/research/taxonomy/cabs/'

all: taxonomy

taxonomy:
	$(OCAMLBUILD) -classic-display -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS),'difflib.cmxa utils.cmxa batteries.cmxa' main.native

clean:
	$(OCAMLBUILD) -clean
