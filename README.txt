
            Weimer's Notes On His Genetic Programming Prototype
                       Fri Jul 11 21:34:39 EDT 2008
                                      *

*********
* 0. Basics
*********

I did this under unix: 

Linux yuki 2.6.23.15-80.fc7 #1 SMP Sun Feb 10 17:29:10 EST 2008 i686 i686 i386 GNU/Linux

... it shouldn't really matter, although I assume standard unix shell
scripting (e.g., my GP fitness function calls "diff" and "wc" and whatnot,
so it may not work on win32 without cygwin). 

I'm assuming that you know how to do 'configure' and edit Makefiles and
whatnot. 

I wrote my prototype in OCaml, a functional language favored by PL
researchers -- it's based on the Meta-Language, which is particularly
convenient for writing a program in one language that looks at programs in
another language (i.e., it's good for writing program analyses and
transformations, or for tinkering with ASTs).

If you have a standard linux distro your package manager will have it
somewhere. If not, you can get it here:

http://caml.inria.fr/ocaml/release.en.html

I did everything with 3.09.3, but anything newer should also work: 

yuki:~/genprog/ga$ ocamlc -v
The Objective Caml compiler, version 3.09.3
Standard library directory: /usr/lib/ocaml

*********
* 1. Libraries
*********

You'll need CIL, the library I use for parsing C and getting ASTs. I used
CIL 1.3.6, but anything newer should also work. 

http://hal.cs.berkeley.edu/cil/

George C. Necula, Scott McPeak, Shree Prakash Rahul, Westley Weimer: CIL:
Intermediate Language and Tools for Analysis and Transformation of C
Programs. Conference on Compiler Construction (CC) 2002: 213-228

http://www.cs.virginia.edu/~weimer/p/weimer-cc2002.pdf

Getting CIL up and running (once you have ocaml) should be as simple as:

        cd cil
        ./configure
        make 
        make cillib

Important: Make sure you do "make cillib", since we use CIL as a library.

Set the environment variable CIL to point to whereever cil.spec ends up
living. So if you put cil.tar.gz in /home/weimer/src and extracted it
to /home/weimer/src/cil, you should do

        export CIL=/home/weimer/src/cil

You'll need to set this every time, so put it in your shell startup script. 

*********
* 2. SVN Checkout 
*********

This command will check all of the files out of our SVN repository.  

svn co https://turing.cs.virginia.edu/svn/genprog

Read through the source of ga/modify.ml -- that single file has all of the
GP stuff, and I put a bunch of comments in it. 

*********
* 3. Building
*********

Currently there are three main top-level directories: 

ga/                     -- my prototype source code
nullhttpd-0.5.0/        -- nullhttpd source code
test-nullhttpd/         -- temp directory for running my single experiment

You should be able to go into ga and just do "make clean". Here's what I
see when I do so: 

  yuki:~/genprog/ga$ make clean
  rm -f *.cmo *.cmi *.d *.cmx *.dx *.o coverage modify
  yuki:~/genprog/ga$ make
  ocamlopt                        -I /home/weimer/src/cil// -I
  /home/weimer/src/cil//src -I /home/weimer/src/cil//src/ext -I
  /home/weimer/src/cil//src/frontc -I /home/weimer/src/cil//obj/x86_LINUX  -c
  coverage.ml
  ocamlopt                        -I /home/weimer/src/cil// -I
  /home/weimer/src/cil//src -I /home/weimer/src/cil//src/ext -I
  /home/weimer/src/cil//src/frontc -I /home/weimer/src/cil//obj/x86_LINUX  -o
  coverage unix.cmxa str.cmxa cil.cmxa coverage.cmx
  ocamlopt                        -I /home/weimer/src/cil// -I
  /home/weimer/src/cil//src -I /home/weimer/src/cil//src/ext -I
  /home/weimer/src/cil//src/frontc -I /home/weimer/src/cil//obj/x86_LINUX  -c
  stats2.ml
  ocamlopt                        -I /home/weimer/src/cil// -I
  /home/weimer/src/cil//src -I /home/weimer/src/cil//src/ext -I
  /home/weimer/src/cil//src/frontc -I /home/weimer/src/cil//obj/x86_LINUX  -c
  modify.ml
  ocamlopt                        -I /home/weimer/src/cil// -I
  /home/weimer/src/cil//src -I /home/weimer/src/cil//src/ext -I
  /home/weimer/src/cil//src/frontc -I /home/weimer/src/cil//obj/x86_LINUX  -o
  modify unix.cmxa str.cmxa cil.cmxa stats2.cmx modify.cmx
  yuki:~/genprog/ga$ 

The file 'coverage' instruments a C program so that we can extract the
critical path from a failed run. It also builds a serialized AST for the
program, as well as a hashtable mapping integers to statements. 

The file 'modify' actually does the genetic programming. It uses the
serialized AST, the source code, the path produced by a single run, etc.,
as well as two external scripts: ./test-good.sh and ./test-bad.sh, which
codify the good testcases and the anomaly. 

*********
* 4. Reproducing the GCD Experiment
*********

Inside ga/ is a subdirectory ga/quad/ that holds the GCD experiment. I'm
not sure why I named it quad -- I've forgotten. Anyway, I've already run
'coverage' on gcd.c, but you can reproduce that with something like: 
        
        ../coverage gcd.c > gcd-coverage.c 

That will produce the .ast and .ht files. Then compile the coverage
version:

        gcc -o gcd-coverage gcd-coverage.c

And run it on the bad input: 

        ./gcd-coverage 0 55

Press Ctrl-C really fast, 'cause it loops forever making a big path. 

Now you're ready to try out the genetic programming. Take a look at
test-good.sh and test-bad.sh to see how they work. They'll be pretty opaque
for now, but it's basic regresion testing: they just compare the output
against some reference output. Note that "ulimit -t 1" limits the bad test
to 1 second of CPU time, so it can't loop forever. 

Anyway, here's one command I used for the GCD example: 

          ../modify --swap 0 --del 0 --mut 0.1 gcd.c 

I have checked in my reference output in gcd.c-reference.debug, and I
have also zipped up all of the intermediate files into 
output-of-successful-gcd-run-2008-07-05.zip in the main genprog directory.
If you can't repro my results, look at the output for difference and then
email me. The nullhttpd experiment is a little tougher, so master the GCD
thing before trying it. 

*********
* 5. Reproducing the Nullhttpd Experiment
*********

The first step here is combining nullhttp into a single C file. CIL does
this for you automatically: you just use "cilly" instead of "gcc". 

        cd nullhttpd-0.5.0/src
        make CC="/home/weimer/src/cil/bin/cilly --merge --keepmerged"

If you do it correctly, you will generate httpd_comb.c in 

        nullhttpd-0.5.0/httpd/bin

I've left a version of httpd_comb.c checked in just in case you're having
trouble repro'ing this step. 

You then run coverage on it

        coverage httpd_comb.c > httpd_coverage.c
        gcc -o httpd_coverage httpd_coverage -lpthread 

Now, nullhttpd reads a configuration file in ../etc/httpd.cfg whenever you
start it up. If you run it without one, it will create a blank one for you.
Mine looks like this: 

  # This file contains system settings for Null httpd.
  SERVER_BASE_DIR = "/home/weimer/genprog/nullhttpd-0.5.0/httpd"
  SERVER_BIN_DIR  = "/home/weimer/genprog/nullhttpd-0.5.0/httpd/bin"
  SERVER_CGI_DIR  = "/home/weimer/genprog/nullhttpd-0.5.0/httpd/cgi-bin"
  SERVER_ETC_DIR  = "/home/weimer/genprog/nullhttpd-0.5.0/httpd/etc"
  SERVER_HTTP_DIR = "/home/weimer/genprog/nullhttpd-0.5.0/httpd/htdocs"
  SERVER_LOGLEVEL = "1"
  SERVER_HOSTNAME = "yuki.cs.virginia.edu"
  SERVER_PORT     = "8080"
  SERVER_MAXCONN  = "50"
  SERVER_MAXIDLE  = "120"

You'll want to change it so that SERVER_HOSTNAME and the _DIR things point
to whereever you actually have it. Do not just put localhost for
SERVER_HOSTNAME. Use your real full hostname. 

You can run the default original version first to see if you've gotten it
working. I checked in a copy of my webpage as index.html, so you can do
something like:

        mozilla http://localhost:8080/index.html

To see if it works. 

Now go into

        genprog/test-nullhttpd

And run "make". Aside from "limit" and "limit5" (or whatever), this should
build nullhttp-exploit. Test it:

  ./nullhttpd-exploit -h your.full.host.name -t 2 -p 8080

It will crash your nullhttpd. You can tell if it works by inspecting the
running process list, or by trying to get index.html again. 

Make sure that you can reliably run "out of the box" unmodified nullhttpd,
serve pages, and kill it with the exploit before going further.  

Ready? Ok, so now go back to httpd_coverage and compile it.  Delete or move
away any .path files in the directory. Then run it.  Now rather than
getting index.html, run the exploit. The server should crash, but it should
leave a big .path file in the directory. You should now have:

-rw-rw-r-- 1 weimer 151166 2008-07-05 13:30 httpd_comb.c
-rw-rw-r-- 1 weimer 164724 2008-07-05 13:30 httpd_comb.c.ast
-rw-rw-r-- 1 weimer 198616 2008-07-05 13:30 httpd_comb.c.ht
-rw-rw-r-- 1 weimer 285581 2008-07-05 13:30 httpd_comb.c.path

Copy those to genprog/test-nullhttpd -- you now have everything you need to
run the genetic algorithm on nullhttpd and try to find a working variant.
First, take a look at test-good.sh and test-bad.sh -- they're a bit more
complicated now because they use that port argument the modify.ml source
mentioned (you read it, right?). 

Anyway, test-good.sh and test-bad.sh are a little more complicated now,
because each webserver needs its own 'sandbox'. So we copy the htdocs
directory afresh for each one, and make a special httpd.conf file for each
one. Then we run each variant in its own little sandbox. 

This is the command I used to get it working:

  ../ga/modify --good "./limit5 ./test-good.sh" --bad "./limit5 ./test-bad.sh" --ldflags -lpthread --mut 0.03 httpd_comb.c

"limit5" means "run me for at most 5 seconds"

I've checked in my debugging output into httpd_comb-reference.debug. I
have also made a big ZIP file of all of the temporary files generated and
put it in output-of-successful-nullhttpd-run-2008-07-05.zip . 

Presto, you're done. 

// EOF: Fri Jul 11 22:07:03 EDT 2008
