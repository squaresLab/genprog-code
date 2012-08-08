#export DYLD_LIBRARY_PATH=/home/claire/research/taxonomy/z3/lib:$DYLD_LIBRARY_PATH
OCAMLBUILD ?= ocamlbuild


OS=$(shell uname)
ifeq ($(OS),Linux)
	OS=LINUX
endif
ifeq ($(OS),Darwin)
	OS=DARWIN
endif

OCAML_OPTIONS = -I $(CIL)/obj/x86_$(OS)./

PATH_TO_Z3_OCAML	= /Users/claire/research/taxonomy/z3/ocaml/
PATH_TO_Z3_LIBS		= /Users/claire/research/taxonomy/z3/lib/
PATH_TO_Z3_BIN		= /Users/claire/research/taxonomy/z3/bin/

OCAMLLIB=/usr/lib/ocaml
# libocamlidl.a is in /usr/local/lib/ocaml on my mac and /usr/lib/ocaml on Liskov (I think)

OCAML_OPTIONS ='-thread','-I $(CIL)/obj/x86_$(OS)','-I $(PATH_TO_Z3_OCAML)','-I $(PATH_TO_Z3_LIBS)','-ccopt -fopenmp'

export OCAMLRUNPARAM=l=256M

all: taxonomy

taxonomy:
	$(OCAMLBUILD) -libs batteries,cil,z3 -classic-display -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS) main.native

taxonomy-prof:
	$(OCAMLBUILD) -libs batteries,cil,z3 -classic-display -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS) main.p.native

taxonomy-byte:
	$(OCAMLBUILD) -libs batteries,cil,z3 -classic-display -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS) main.d.byte

clean:
	$(OCAMLBUILD) -clean
