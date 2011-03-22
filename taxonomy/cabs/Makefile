OCAMLBUILD ?= ocamlbuild

OCAML_OPTIONS = '-thread','-I /Users/csl9q/research/'

export OCAMLRUNPARAM=l=256M

all: main difflib

difflib:
	$(OCAMLBUILD) -libs utils,batteries -classic-display -no-hygiene -cflags $(OCAML_OPTIONS) -lflags $(OCAML_OPTIONS) difflib.cma
	$(OCAMLBUILD) -libs utils,batteries -classic-display -no-hygiene -cflags $(OCAML_OPTIONS) -lflags $(OCAML_OPTIONS) difflib.cmxa

main: 
	$(OCAMLBUILD) -libs utils,batteries -classic-display -no-hygiene -cflags $(OCAML_OPTIONS) -lflags $(OCAML_OPTIONS) main.native

clean:
	$(OCAMLBUILD) -clean
