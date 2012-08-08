OCAMLBUILD ?= ocamlbuild

OCAML_OPTIONS = '-thread','-I /Users/claire/research/'

export OCAMLRUNPARAM=l=256M

all: main difflib

difflib:
	$(OCAMLBUILD) -classic-display -no-hygiene -cflags $(OCAML_OPTIONS) -lflags $(OCAML_OPTIONS) difflib.cma
	$(OCAMLBUILD) -classic-display -no-hygiene -cflags $(OCAML_OPTIONS) -lflags $(OCAML_OPTIONS) difflib.cmxa

main: 
	$(OCAMLBUILD) -classic-display -no-hygiene -cflags $(OCAML_OPTIONS) -lflags $(OCAML_OPTIONS) main.native

clean:
	$(OCAMLBUILD) -clean
