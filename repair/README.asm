# -*- mode: org -*-
#+Title: Automated Evolutionary repair of ASM

1. install oprofile and gdb using your package manager, these are used
   to sample execution traces

2. follow the general instillation instructions in README.txt to
   install OCaml CIL etc...

3. build the .s assembly file with something like
   : gcc -S program.c

4. start oprofile (pass the --no-vmlinux option if needed)
   : sudo opcontrol --start

5. repair is run in the same manner as with C level repairs, the
   contents of an example configuration file are included below
   #+begin_example asm.conf
     --program program.s
     --pos-tests 5
     --neg-tests 1
     --allow-coverage-fail
     --search ga
     --keep-source
     --promut 1
     --popsize 100
     --generations 10
     --asm-sample-runs 1000
   #+end_example
