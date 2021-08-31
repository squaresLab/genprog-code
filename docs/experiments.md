---
layout: page
title: "Experiments"
description: "Experiments and data sets used for GenProg publications"
header-img: "img/header.jpg"
---

We are committed to reproducible research and to making our tools freely
available. Since our tools, techniques and benchmark suites evolve and improve
over time, there is no single all-inclusive package. Instead, for major papers,
we try to make available a snapshot of the programs and benchmarks used in that
paper, so that others can reproduce or extend those experiments.

---

Code
-------------------------

There are several sources of code used in our publications:

* [GenProg Source Code (for C)](https://github.com/squaresLab/genprog-code). We tag releases
for experiments in various papers, described on the
[releases page.](https://github.com/squaresLab/genprog-code/releases) 
* [Software Evolution
Library](https://github.com/eschulte/software-evolution). This common interface
abstracts over multiple types 
of software objects including abstract syntax trees parsed from source code,
LLVM IR, compiled assembler, and linked ELF binaries. Mutation and evaluation
methods are implemented on top of this interface. This code was used for
experiments appearing in [ASPLOS 2014](/genprog-code/publications/index.html#asplos14).   
* [Genetic Optimization
Algorithm](https://github.com/eschulte/goa). This GOA implementation accepts an assembly 
program with a workload and a fitness function (e.g., power consumption) and
optimizes that fitness function. This source code was used for experiments
appearing in [ASPLOS 2014](/genprog-code/publications/index.html#asplos14). 
* <a id="gpv1.0"></a>The experiments in ICSE 2009, GECCO 2009, GECCO 2010 and TSE 2012 used an older
version of GenProg. A snapshot of that codebase is available:
[GenProg v1.0](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-source-v1.tar.gz)


---

Subject Programs 
-------------------------

<a id="manybugs"></a>**ManyBugs and IntroClass**  
The [RepairBenchmarks website](http://repairbenchmarks.cs.umass.edu/) contains
detailed information on the ManyBugs and IntroClass benchmarks, described in
detail in [TSE 2015](/genprog-code/publications/index.html#tse15), including the baseline
experimental results for GenProg, AE, and TRPAutoRepair.

<a id="icse12"></a>**105 GenProg ICSE 2012 Program Bugs**  
These scenarios and results were used for the systematic study on program repair published in ICSE 2012
([Paper](/genprog-code/publications/index.html#icse12)), and the study of representation and
operator choices in GECCO 2012 ([Paper](/genprog-code/publications/index.html#gecco12)).
***Note: these benchmarks are deprecated.*** We include these results for completeness,
but we discourage their use in future work. Instead, the TSE 2015 benchmarks release
(above) includes important corrections.  
[Virtual Machine Images](http://dijkstra.eecs.umich.edu/genprog/resources/genprog_images)
[Buggy Programs](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-icse2012-benchmarks/)

<a id="tse12"></a>**TSE 2012 Bugs**  
These programs were used in the experiments in [this
paper](/genprog-code/publications/index.html#tse12); they are a superset of the programs/bugs used
in ICSE 2009 and GECCO 2009.  The virtual machine image demonstrates the wu-ftpd
repair described in that article.  Instructions assume GenProg v1.0.  
[Virtual Machine Images](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-tse2012-wuftpd.vdi)
[VM Instructions](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-tse2012-wuftpd.txt)
[Buggy Programs](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-benchmarks-tse-2012.tar.gz)
[Workloads](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-tse2012-workload.tar.gz)

**GECCO 2010**  
In GECCO 2010, we investigated alternative fitness functions for test-guided
APR.  Instructions assume GenProg v1.0.  
[Buggy Programs](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-benchmarks-2010.tar.gz)

**2009 Buggy Programs**  
These experiments cover the GenProg publications in both ICSE 2009 and GECCO 2009.
 Instructions assume GenProg v1.0.  
[Buggy Programs](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-benchmarks-2009.tar.gz)

**SIR**  
The ASPLOS 2013 paper includes results on the [Software-artifact Infrastructure
Repository](http://sir.unl.edu/).

**PARSEC**  
The [ASPLOS 2014](/genprog-code/publications/index.html#asplos14) paper makes use of the [PARSEC](http://parsec.cs.princeton.edu/) benchmark.


---

Experimental results
-------------------------

### Automatic program repair

**ASE 2013** ([Paper](/genprog-code/publications/index.html#ase13))  
These experiments relate to the Adaptive Equality repair algorithm that uses an
approximation to program equivalence to reduce the search and introduces on-line
learning strategies to order test cases and repairs.  
[Code](https://github.com/squaresLab/genprog-code/tree/releases/v3.0)
[Experimental Results](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-ase2013-results.zip)

**ICSE 2012** ([Paper](/genprog-code/publications/index.html#icse12))  
A systematic study of program repair.  These experiments were conducted on AWS, using images
that we have converted to VirtualBox format.  The READMEs also point to a
publicly-available AMI.  Please use <a href="#manybugs">ManyBugs</a> for all
future experiments.  
[Code](https://github.com/squaresLab/genprog-code/tree/releases/v2.0)
[Virtual Machine Images](http://dijkstra.eecs.umich.edu/genprog/resources/genprog_images)
[Experimental Results](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-icse2012-results.zip)

**TSE 2012** ([Paper](/genprog-code/publications/index.html#tse12))  
These experiments used GenProg 1.0.  The virtual machine image demonstrates the wu-ftpd
repair described in that article.  
[Virtual Machine Images](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-tse2012-wuftpd.vdi)
[VM Instructions](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-tse2012-wuftpd.txt)
[Results]()

**ICSE/GECCO 2009**  
These results cover the GenProg publications in both ICSE 2009 and GECCO 2009.  
[Experimental Results](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-results-2009.tar.gz)

### Search specifics

**GECCO 2012**  
These include repair results for various genetic algorithm parameter values.  
[Experimental Results](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-gecco2012-results.tar.gz)

**GECCO 2010**  
In GECCO 2010, we investigated alternative fitness functions for test-guided
APR.  
[Experimental Results]()


### Patch quality, software robustness 

**ISSTA 2012**  
This dataset includes the subject code and questions presented to humans,
as well as the human responses.  
[Dataset](http://dijkstra.eecs.umich.edu/genprog/resources/issta2012-study-data.zip)


**GPEM 2013**  
These results relate to neutral mutants and software mutational
robustness. Experimental results
for
[higher order neutral mutants](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-results-gpem-2013-higher-order.tar.bz2) are
also available.
[Benchmarks](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-benchmarks-gpem-2013.tar.gz2) 
[Sorting Programs](https://github.com/eschulte/sorters) [Experimental Results](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-results-gpem-2013.tar.gz)

---

Non-functional or quality properties:
-------------------------------------

**Pacific Graphics 2015**

These experiments use GenProg-like approaches to automatically generate band-limited procedural shaders.
[Dataset](https://www.eecs.umich.edu/~weimer/data/bandlimiting/)

**ASPLOS 2014**  
These experiments use GenProg-like approaches to reduce the power consumption of
software.  
[Experimental Results](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-results-asplos-2014.tar.bz2)
[Code](https://github.com/eschulte/goa/tree/asplos2014)

**ASPLOS 2013**  
These experiments relate to the automated repair of assembly and binaries in
embedded systems.
[Benchmarks](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-benchmarks-asplos-2013.tar.bz2)
[Experimental Results](http://dijkstra.eecs.umich.edu/genprog/resources/genprog-results-asplos-2013.tar.bz2)

