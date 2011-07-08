# -*- mode: org -*-
#+Title: Automated Evolutionary repair of ASM

The following steps should be sufficient to run repairs at the level
of compiled assembly code.

1. first follow the general instillation instructions in README.txt to
   install OCaml CIL etc... note that the source code is now located in
   the base of the genprog-code repository

2. for now, some of the logic is located in the external mem-mapping
   lisp script included in this directory, there is no reason why this
   logic could not be re-written in OCaml in asmrep.ml aside from time
   constraints.  Ensure that mem-mapping is in your PATH.

   This script requires a recent version of clisp be installed on the
   users machine.  The clisp executable is expected to be located at
   : /usr/local/bin/clisp

   The recommended way to install clisp is to first grab the
   development sources using mercurial from
   : http://clisp.hg.sourceforge.net:8000/hgroot/clisp/clisp

   Then I'd recommend installing clisp using your package manager,
   this will facilitate the bootstrapping build process but isn't
   strictly necessary.  Finally follow the instructions in the clisp
   source directory.

3. install oprofile and gdb using your package manager, these are used
   to sample execution traces

4. a number of common unix utilities are required including awk, join,
   and grep.  Again these are the time-saving hacks of an OCaml
   neophyte, the same logic could easily be implemented directly in
   OCaml by someone with more familiarity with the language.

5. run make in the base of the genprog-code directory to build the
   repair script

6. before running repair build the .s assembly file with something
   like
   : gcc -S uniq.c

7. repair is run in the same manner as with C level repairs, the
   contents of an example configuration file are included below

   #+begin_example asm.conf
     --program uniq.s
     --pos-tests 5
     --neg-tests 1
     --allow-coverage-fail
     --search ga
     --keep-source
     --promut 1
     --popsize 100
     --generations 10
     --asm-sample-runs 0
   #+end_example
