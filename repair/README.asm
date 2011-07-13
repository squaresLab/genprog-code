# -*- mode: org -*-
#+Title: Automated Evolutionary repair of ASM

1. first follow the general instillation instructions in README.txt to
   install OCaml CIL etc...

2. install oprofile and gdb using your package manager, these are used
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
