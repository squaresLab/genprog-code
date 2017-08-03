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

*This page is sorted topically*

---

Automated Program Repair:
-------------------------

**TSE 2015** ([Paper](/publications/index.html#tse15))  
The [RepairBenchmarks website](http://repairbenchmarks.cs.umass.edu/) contains
detailed information on the ManyBugs and IntroClass benchmarks, as well as the baseline
experimental results described in the article.

**ASE 2013** ([Paper](/publications/index.html#ase13))  
These experiments relate to the Adaptive Equality repair algorithm that uses an
approximation to program equivalence to reduce the search and introduces on-line
learning strategies to order test cases and repairs. The experiments were
conducted on the 105 ICSE 2012 program bugs (below).  
[Experimental Results](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-ase2013-results.zip)
[Code](https://github.com/squaresLab/genprog-code/tree/releases/v3.0)

**105 GenProg ICSE 2012 Program Bugs**  
These scenarios and results were used for the systematic study on program repair published in ICSE 2012
([Paper](/publications/index.html#icse12)), the Adaptive Equality
repair algorithm (above), and the study of representation and operator choices
in GECCO 2012 ([Paper](/publications/index.html#gecco12)).
***Note: these benchmarks are deprecated.*** We include these results for completeness,
but we discourage their use in future work. Instead, the more recent TSE 2015
extension (above) includes important corrections.  
[Virtual Machine Images](http://dijkstra.cs.virginia.edu/genprog/resources/genprog_images)
[Benchmarks](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-icse2012-benchmarks/)
[Experimental Results](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-icse2012-results.zip)
[Code](https://github.com/squaresLab/genprog-code/tree/releases/v2.0)

**TSE 2012** ([Paper](/publications/index.html#tse12))
These experiments used with an older version of GenProg featured in
TSE 2012.  
[Virtual Machine Images](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-tse2012-wuftpd.vdi)
[Instructions](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-tse2012-wuftpd.txt)
[Benchmarks](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-benchmarks-tse-2012.tar.gz)
[Workloads](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-tse2012-workload.tar.gz)
[Code](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-source-v1.tar.gz)

**ICSE/GECCO 2009:**  
These experiments cover the GenProg publications in both ICSE 2009 and GECCO 2009.  
[Benchmarks](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-benchmarks-2009.tar.gz)
[Experimental Results](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-results-2009.tar.gz)
[Code](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-source-v1.tar.gz)

---

Search specifics:
-----------------

**SSBSE 2016:**  
[FIXME]()

**GECCO 2012:**  
These include repair results for various genetic algorithm parameter values.  
[Experimental Results](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-gecco2012-results.tar.gz)

**GECCO 2010:**  
[Benchmarks](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-benchmarks-2010.tar.gz)
[Code](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-source-v1.tar.gz)

---

Patch quality, software robustness:
-----------------------------------

**ESEC/FSE 2015:**  
[FIXME]()

**GPEM 2013:**  
These results relate to neutral mutants and software mutational
robustness. Experimental results
for
[higher order neutral mutants](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-results-gpem-2013-higher-order.tar.bz2) are
also
available.
[Benchmarks](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-benchmarks-gpem-2013.tar.gz2) [Sorting Programs](https://github.com/eschulte/sorters) [Experimental Results](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-results-gpem-2013.tar.gz)

**ISSTA 2012:**  
This dataset includes the subject code and questions presented to humans,
as well as the human responses.  
[Dataset](http://dijkstra.cs.virginia.edu/genprog/resources/issta2012-study-data.zip)

---

Non-functional or quality properties:
-------------------------------------

**ASPLOS 2014:**  
These experiments use GenProg-like approaches to reduce the power consumption of
software.  
[Experimental Results](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-results-asplos-2014.tar.bz2)
[Code](https://github.com/eschulte/goa/tree/asplos2014)

**ASPLOS 2013:**  
These experiments relate to the automated repair of assembly and binaries in
embedded systems.
[Benchmarks](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-benchmarks-asplos-2013.tar.bz2)
[Experimental Results](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-results-asplos-2013.tar.bz2)

