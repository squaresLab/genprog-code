# Makefile for Program Repair Tool (v2)

# You may need to set the CIL environment variable for this to
# work. It should point to the base of the CI source directory, mine
# is /usr/local/src/cil.
#
# If the CIL is not set then the ocamlfind utility will be used to set
# the CIL environment variable.

OS=$(shell uname)
ifeq ($(OS),Linux)
	OS=LINUX
 endif
ifeq ($(OS),Darwin)
	OS=DARWIN
endif

ifneq ($(MAKECMDGOALS),clean)
  ifeq ($(CIL),)
    ifneq ($(shell type ocamlfind 2> /dev/null),)
		OCAML_OPTIONS := \
			$(shell ocamlfind query findlib 2> /dev/null) \
			$(shell ocamlfind query cil 2> /dev/null)
    endif
  endif
  ifeq ($(OCAML_OPTIONS),)
        OCAML_OPTIONS = $(CIL)/obj/x86_$(OS)
  endif
  ifeq ($(wildcard $(OCAML_OPTIONS)/cil.cmx),)
        OCAML_OPTIONS = $(wildcard $(CIL)/_build/src/cil.cmx)
        OCAML_OPTIONS := $(patsubst %/src/cil.cmx,%,$(OCAML_OPTIONS))
    ifneq ($(OCAML_OPTIONS),)
        OCAML_OPTIONS := \
          $(OCAML_OPTIONS) \
          $(OCAML_OPTIONS)/src \
          $(OCAML_OPTIONS)/src/ext \
          $(OCAML_OPTIONS)/src/ext/pta \
          $(OCAML_OPTIONS)/src/frontc \
          $(OCAML_OPTIONS)/ocamlutil
    endif
  endif
  ifeq ($(OCAML_OPTIONS),)
$(error Could not find CIL include path. Is $$CIL set correctly?)
  else
        OCAML_OPTIONS := $(foreach d,$(OCAML_OPTIONS),-I $d)
  endif
endif

OCAMLC   = ocamlc -g $(OCAML_OPTIONS)
OCAMLOPT = ocamlopt -w Aelzv-7 $(OCAML_OPTIONS)
OCAMLDEP = ocamldep $(OCAML_OPTIONS)
OCAMLLEX = ocamllex
OCAMLDOC = ocamldoc $(OCAML_OPTIONS)

MBITS := $(if $(shell touch null.ml ; $(OCAMLOPT) -c null.ml ; file null.o | grep 64 ; rm null.*),-m64,-m32)

###
#
# You should not have to change anything below this line. 
#
###

# We use an internal utility to auto-generate token information,
# visitor code and pretty-printing code from ocaml type definitions. 
# If you don't change "tokens.type" or "jabs.ml" you won't need this. 

ALL = repair nhtserver distserver test-cache-reader
all: $(ALL)

%.cmo: %.ml 
	@if [ -f $*.mli -a ! -f $*.cmi ] ; then $(OCAMLC) -c -g $*.mli ; fi 
	$(OCAMLC) -c -g $*.ml
	@$(OCAMLDEP) $*.ml > $*.d 

%.cmx: %.ml 
	@if [ -f $*.mli -a ! -f $*.cmi ] ; then $(OCAMLC) -c -g $*.mli ; fi 
	$(OCAMLOPT) -c $*.ml
	@$(OCAMLDEP) $*.ml > $*.d 

%.cmi: %.mli
	$(OCAMLC) -c -g $*.mli

%.ml: %.mll
	$(OCAMLLEX) $*.mll

# NOTE: Module order is important!  OCaml module dependencies cannot
# be cyclic, and the order presented must respect the dependency order.

ifdef USE_PELLACINI 
PELLACINI = pellacini.cmo cgrep.cmo 
endif

BASE_MODULES = \
  stats2.cmo \
  global.cmo \
  trie.cmo \
  distglobal.cmo \
  cdiff.cmo \
  template.cmo \
  rep.cmo \
  fitness.cmo \
  simplerep.cmo \
  stringrep.cmo \
  gaussian.cmo \
  asmrep.cmo \
  minimization.cmo \
  cilprinter.cmo \
  knownfuns.cmo \
  progeq.cmo \
  lasetemplates.cmo \
  cilrep.cmo \
  population.cmo \
  search.cmo \
  multiopt.cmo \
  $(PELLACINI) \
  network.cmo \
  llvmrep.cmo

REPAIR_MODULES = \
  $(BASE_MODULES) \
  version.cmo \
  main.cmo

STANDARD_LIBS = \
	bigarray.cmxa \
	unix.cmxa \
	str.cmxa \
	nums.cmxa \
	dynlink.cmxa \
	findlib.cmxa 

# This .PHONY target stores the current repository version into an OCaml file
# so that we can report it in the debug log. It is .PHONY to ensure that make
# always rebuilds the file; otherwise, a stale file might keep reporting the
# wrong version even after many updates.

.PHONY: version.ml
version.ml:
	if expr "`svnversion`" : "[0-9]" > /dev/null ; then \
	  echo "let version = \"`svnversion`\"" > $@ ; \
	  case `svnversion` in \
	    *M) svn status | grep "^M" | sed -e 's/^\(.*\)/^ "\\n\1"/' >> $@ ;;\
	  esac ; \
	elif hg id > /dev/null 2>&1 ; then \
	  echo "let version = \"`hg id -n -b` (Mercurial)\"" > $@ ; \
	  case `hg id -n` in \
	    *+) hg status | grep "^M" | sed -e 's/^\(.*\)/^ "\\n\1"/' >> $@ ;; \
	  esac ; \
	else \
	  echo "let version = \"`date`\"" > $@ ; \
	fi

../obj/genprog.cmxa: $(REPAIR_MODULES:.cmo=.cmx)
	$(OCAMLOPT) -a -o $@ $^

lib: ../obj/genprog.cmxa

repair: $(REPAIR_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ $(STANDARD_LIBS) cil.cmxa pta.cmxa liveness.cmxa $^

repair.chroot:  $(REPAIR_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ $(STANDARD_LIBS) cil.cmxa $^ -cc "gcc -Wl,-dynamic-linker,/lib64-20/ld-linux-x86-64.so.2,-rpath=/lib64-20"

repair.byte: $(REPAIR_MODULES)
	$(OCAMLC) -o $@ $(STANDARD_LIBS:.cmxa=.cma) cil.cma pta.cma liveness.cma $^

edit: $(BASE_MODULES:.cmo=.cmx) edit.cmx
	$(OCAMLOPT) -o $@ $(STANDARD_LIBS) cil.cmxa $^

edit.byte: $(BASE_MODULES) edit.cmo
	$(OCAMLC) -o $@ $(STANDARD_LIBS:.cmxa=.cma) cil.cma $^

NHT_MODULES = \
  global.cmo \
  nhtserver.cmo

nhtserver: $(NHT_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ $(STANDARD_LIBS) $^

DIST_SERVER_MODULES = \
  global.cmo \
  distglobal.cmo \
  distserver.cmo

distserver: $(DIST_SERVER_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ $(STANDARD_LIBS) $^

test-cache-reader:
	ln -s repair test-cache-reader

CDIFF_MODULES = \
	global.cmo \
	cdiff.cmo \
	minimization.cmo \
	cdiffmain.cmo

cdiff: $(CDIFF_MODULES:.cmo=.cmx)
	$(OCAMLOPT) -o $@ $(STANDARD_LIBS) cil.cmxa $^

# dependencies
ALL_MODULES = \
	$(REPAIR_MODULES) \
	distserver.cmo \
	nhtserver.cmo

-include $(ALL_MODULES:.cmo=.d)

testsuite: ../test/Makefile
	cd ../test/ && make

doc: $(ALL_MODULES:.cmo=.ml) $(ALL_MODULES:.cmo=.cmx) ../README.txt
	$(OCAMLDOC) -html -d ../doc/ ../README.txt $(sort $(ALL_MODULES:.cmo=.ml))

clean:
	rm -f repair nhtserver test-cache-reader repair.byte cdiff *.mli *.cmo *.cmi *.d *.cmx *.dx *.o $(ALL) ../doc/*
