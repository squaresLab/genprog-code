export DYLD_LIBRARY_PATH=/Users/claire/research/taxonomy/z3/lib:$DYLD_LIBRARY_PATH
OCAMLBUILD ?= ocamlbuild

PATH_TO_Z3_OCAML	= /Users/claire/research/taxonomy/z3/ocaml/
PATH_TO_Z3_LIBS		= /Users/claire/research/taxonomy/z3/lib/
PATH_TO_Z3_BIN		= /Users/claire/research/taxonomy/z3/bin/

OCAMLLIB=/usr/local/lib/ocaml
# libocamlidl.a is in /usr/local/lib/ocaml on my mac and /usr/lib/ocaml on Liskov (I think)

OCAML_OPTIONS ='-thread','-I /Users/claire/research/cil-1.3.7/obj/x86_DARWIN','-I $(PATH_TO_Z3_OCAML)','-I $(PATH_TO_Z3_LIBS)','-ccopt -fopenmp'

export OCAMLRUNPARAM=l=256M

all: taxonomy

taxonomy:
	$(OCAMLBUILD) -libs batteries,cil,z3 -classic-display -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS) main.native 

clean:
	$(OCAMLBUILD) -clean
