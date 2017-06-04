asmrep.ml: Representation for Automated Evolutionary repair of x86 Assembly
===========================================================================

Version 7 of [asmrep.ml](asmrep.ml) has been almost completely changed from
earlier versions, but usage is still fairly straightforward. This representation
was designed around AT&T syntax x86 or x64 assembly output by the GAS assembler
and will likely not work with other assembly formats. Use the string
representation with `--rep txt` instead when modifying these files.

The assembly representation uses knowledge about x86 assembly to reduce the size
of the fault/search and fix spaces. To turn off either, use
`--disable-reduce-fix-space` and `--disable-reduce-search-space`.

1. follow the general instillation instructions in the
   main [README.md](../README.md) to install OCaml CIL etc...

2. build the .s assembly file with something like `gcc -S program.c`

3. repair is run in the same manner as with C level repairs, the contents of an
   example configuration file are included below

        --program program.s
        --pos-tests 5
        --neg-tests 1
        --allow-coverage-fail
        --search ga
        --keep-source
        --promut 1
        --popsize 100
        --generations 10

Questions about asmrep.ml version 7 should be directed to Jeremy Lacomis:
<jlacomis@cmu.edu>, while questions about earlier versions should be directed to
Eric Schulte: <eschulte@cs.unm.edu>.
