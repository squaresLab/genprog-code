OCAMLBUILD ?= ocamlbuild

OCAML_OPTIONS ='-thread','-I /home/claire/','-I /home/claire/taxonomy/cabs/_build','-I /home/claire/taxonomy/cabs/'

all: taxonomy

taxonomy:
	$(OCAMLBUILD) -classic-display -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS),'difflib.cmxa utils.cmxa batteries.cmxa' main.native

clean:
	$(OCAMLBUILD) -clean
