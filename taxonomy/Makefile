OCAMLBUILD ?= ocamlbuild


OCAML_OPTIONS = '-thread','-I /Users/csl9q/research/genprog/code-cbi/'

all: main

main:
	$(OCAMLBUILD) -classic-display -no-skip -I cabs -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS),'difflib.cmxa utils.cmxa batteries.cmxa' main.native

clean:
	$(OCAMLBUILD) -clean
