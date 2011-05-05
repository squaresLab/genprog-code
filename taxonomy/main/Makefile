OCAMLBUILD ?= ocamlbuild

OCAML_OPTIONS ='-thread','-I /Users/csl9q/research/','-I /Users/csl9q/research/taxonomy/cabs/_build','-I /Users/csl9q/research/taxonomy/cabs/'

#OCAML_OPTIONS ='-thread','-I /home/claire/','-I /home/claire/taxonomy/cabs/_build','-I /home/claire/taxonomy/cabs/'

all: taxonomy

taxonomy:
	$(OCAMLBUILD) -libs difflib,utils,batteries -classic-display -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS) main.native 
#	$(OCAMLBUILD) -libs difflib,utils,batteries -classic-display -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS) main.byte

clean:
	$(OCAMLBUILD) -clean
