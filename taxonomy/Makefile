OCAMLBUILD ?= ocamlbuild


OCAML_OPTIONS = '-thread','-I /Users/csl9q-2/research/genprog/code-cbi/'

all: main

main:
	$(OCAMLBUILD) -classic-display -no-skip -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS),'utils.cmxa batteries.cmxa' main.native

clean:
	$(OCAMLBUILD) -clean
