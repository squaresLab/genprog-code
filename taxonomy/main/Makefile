#export DYLD_LIBRARY_PATH=/Users/claire/research/taxonomy/z3/lib:$DYLD_LIBRARY_PATH
OCAMLBUILD ?= ocamlbuild

OCAMLLIB=/usr/lib/ocaml
# libocamlidl.a is in /usr/local/lib/ocaml on my mac and /usr/lib/ocaml on Liskov (I think)

OCAML_OPTIONS ='-thread','-I /home/claire/cil-1.3.7/obj/x86_LINUX'

export OCAMLRUNPARAM=l=256M

all: taxonomy

taxonomy:
	$(OCAMLBUILD) -libs batteries,cil -classic-display -cflags $(OCAML_OPTIONS) -no-hygiene -lflags $(OCAML_OPTIONS) main.native 

clean:
	$(OCAMLBUILD) -clean
