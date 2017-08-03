---
layout: page
title: "Publications"
description: "GenProg-related publications"
header-img: "img/header.jpg"
---

This page focusing primarily on publications whose data and experiments were
implemented on top of the GenProg codebase by researchers related to the core
GenProg team.  [program-repair.org](http://program-repair.org/) provides a much
more complete, community-supported list of papers related to program repair
generally, including papers that build upon or compare to GenProg.

*This page is sorted topically.*


---

Broad overviews and high-level arguments
-------------------------

Claire Le Goues, Stephanie Forrest, Westley Weimer: Current Challenges in
Automatic Software Repair. Software Quality Journal (SQJO), 2013.  
[PDF](https://squareslab.github.io/papers-repo/pdfs/weimer-sqj-2013.pdf) 
[DOI](https://doi.org/10.1007/s11219-013-9208-0) 
[BibTeX](https://squareslab.github.io/papers-repo/bib/legoues-sqjo13.bib)

Westley Weimer: Advances in Automated Program Repair and a Call To
Arms. Symposium on Search Based Software Engineering (SSBSE), 2013.  
[Slides](https://squareslab.github.io/papers-repo/pdfs/weimer-ssbse2013-presentation.pdf) 
[BibTeX](https://squareslab.github.io/papers-repo/bib/weimer-ssbse13.bib)

Westley Weimer, Stephanie Forrest, Claire Le Goues, ThanhVu Nguyen: Automatic
Program Repair With Evolutionary Computation. Communications of the ACM (CACM), 2010.  
[PDF](https://squareslab.github.io/papers-repo/pdfs/p109-weimer.pdf) 
[DOI](http://doi.acm.org/10.1145/1735223.1735249)
[BibTeX](https://squareslab.github.io/papers-repo/bib/weimer-cacm10.bib)

Claire Le Goues, Stephanie Forrest, Westley Weimer: The Case for Software
Evolution. Foundations of Software Engineering Working Conference on the Future
of Software Engineering (FoSER), at the International Symposium on Foundations
of Software Engineering (FSE), 2010.  
[PDF](https://squareslab.github.io/papers-repo/pdfs/p205-legoues.pdf) 
[DOI](http://doi.acm.org/10.1145/1882362.1882406)
[BibTeX](https://squareslab.github.io/papers-repo/bib/legoues-foser10.bib)


---

Automated Program Repair
-------------------------

### ...targeting C programs

<a id="tse15"></a>Claire Le Goues, Neal Holtschulte, Edward K. Smith, Yuriy Brun, Premkumar
Devanbu, Stephanie Forrest, Westley Weimer. The ManyBugs and IntroClass
Benchmarks for Automated Repair of C Programs. IEEE Transactions on Software
Engineering (TSE), 2015 (featured article).  
[PDF](https://squareslab.github.io/papers-repo/pdfs/benchmarks-2015-tse-preprint.pdf) 
[DOI](https://doi.org/10.1109/TSE.2015.2454513) 
[BibTeX](https://squareslab.github.io/papers-repo/bib/legoues-tse15.bib) | 
[Code, Dataset, and Results](http://repairbenchmarks.cs.umass.edu/)

<a id="ase13"></a>Westley Weimer, Zachary P. Fry, Stephanie Forrest: Leveraging Program
Equivalence for Adaptive Program Repair: Models and First Results. Automated
Software Engineering (ASE), 2013.  
[PDF](https://squareslab.github.io/papers-repo/pdfs/weimer-ase2013-preprint.pdf) 
[DOI](https://doi.org/10.1109/ASE.2013.6693094) 
[BibTeX](https://squareslab.github.io/papers-repo/bib/weimer-ase13.bib) | 
[Code](https://github.com/squaresLab/genprog-code/tree/releases/v3.0)
[Dataset](/experiments/index.html#icse12)
[Experimental Results](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-ase2013-results.zip)

<a id="icse12"></a>Claire Le Goues, Michael Dewey-Vogt, Stephanie Forrest, Westley Weimer. A
Systematic Study of Automated Program Repair: Fixing 55 out of 105 bugs for $8
Each. International Conference on Software Engineering (ICSE), 2012. **Note:
please use the more recent <a href="/experiments/index.html#manybugs">ManyBugs
dataset</a> for future experiments.**  
[PDF](https://squareslab.github.io/papers-repo/pdfs/weimer-icse2012-genprog-preprint.pdf)
[DOI](https://doi.org/10.1109/ICSE.2012.6227211)
[BibTeX](https://squareslab.github.io/papers-repo/bib/legoues-icse12.bib) | 
[Code](https://github.com/squaresLab/genprog-code/tree/releases/v2.0)
[Virtual Machine Images](http://dijkstra.cs.virginia.edu/genprog/resources/genprog_images)
[Dataset](/experiments/index.html#icse12)
[Experimental Results](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-icse2012-results.zip)


<a id="tse12"></a>Claire Le Goues, ThanhVu Nguyen, Stephanie Forrest, Westley Weimer. GenProg: A
Generic Method for Automated Software Repair. IEEE Trans. Software Engineering
(TSE), 2012 (featured article).   
[PDF](https://squareslab.github.io/papers-repo/pdfs/weimer-tse2012-genprog.pdf) 
[DOI](https://doi.org/10.1109/TSE.2011.104)
[BibTeX](https://squareslab.github.io/papers-repo/bib/legoues-tse12.bib) | 
[Code](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-source-v1.tar.gz)
[Dataset, Workloads, and Demo](/experiments/index.html#tse12)
[Experimental Results]()


Stephanie Forrest, Westley Weimer, ThanhVu Nguyen, Claire Le Goues. A Genetic
Programming Approach to Automated Software Repair. Genetic and Evolutionary
Computing Conference (GECCO), 2009 (best paper award) (gold,
human-competitive award).  
[PDF](https://squareslab.github.io/papers-repo/pdfs/weimer-gecco2009.pdf) 
[DOI](http://doi.acm.org/10.1145/1569901.1570031)
[BibTeX](https://squareslab.github.io/papers-repo/bib/forrest-gecco10.bib) | 
[Code](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-source-v1.tar.gz)
[Dataset](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-benchmarks-2009.tar.gz)
[Experimental Results](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-results-2009.tar.gz)

ThanhVu Nguyen, Westley Weimer, Claire Le Goues, Stephanie Forrest. Using
Execution Paths to Evolve Software Patches. Workshop on Search-Based Software
Testing (SBST) 2009 (best short paper award) (best presentation award).  
[PDF](https://squareslab.github.io/papers-repo/pdfs/nguyen-sbst09.pdf) 
[DOI](http://doi.acm.org/10.1145/1882362.1882406)
[BibTeX](https://squareslab.github.io/papers-repo/bib/nguyen-sbst09.bib)

Westley Weimer, ThanVu Nguyen, Claire Le Goues, Stephanie Forrest: Automatically
Finding Patches Using Genetic Programming. International Conference on Software
Engineering (ICSE), 2009 (distinguished paper award) (IFIP TC2 Manfred
Paul award).  
[PDF](https://squareslab.github.io/papers-repo/pdfs/weimer-icse2009-genprog.pdf) 
[DOI](https://doi.org/10.1109/ICSE.2009.5070536)
[BibTeX](https://squareslab.github.io/papers-repo/bib/weimer-icse09.bib) | 
[Code](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-source-v1.tar.gz)
[Dataset](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-benchmarks-2009.tar.gz)
[Experimental Results](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-results-2009.tar.gz)

### ...targeting assembly code

Eric M. Schulte, Westley Weimer, Stephanie Forrest. Repairing COTS Router
Firmware without Access to Source Code or Test Suites: A Case Study in
Evolutionary Software Repair, GECCO Companion, 2015.  
[PDF](https://squareslab.github.io/papers-repo/pdfs/schulte-gecco15.pdf)
[DOI](http://doi.acm.org/10.1145/2739482.2768427)
[BibTex](https://squareslab.github.io/papers-repo/bib/schulte-gecco15.bib) |
[Code, Dataset, and Results]()

<a id="asplos13"></a>Eric Schulte, Jonathan DiLorenzo, Stephanie Forrest and Westley Weimer/
Automated Repair of Binary and Assembly Programs for Cooperating Embedded
Devices. Architectural Support for Programming Languages and Operating Systems
(ASPLOS), 2013.  
[PDF](https://squareslab.github.io/papers-repo/pdfs/schulte2013embedded.pdf) 
[BibTeX]() |
[Code, Dataset, and Results]()

Eric Schulte, Stephanie Forrest, Westley Weimer. Automated Program Repair
through the Evolution of Assembly Code. Automated Software Engineering (ASE,
Short Paper), 2010.  
[PDF](https://squareslab.github.io/papers-repo/pdfs/weimer-ase2010-asm-preprint.pdf) 
[DOI](http://doi.acm.org/10.1145/1858996.1859059)
[BibTeX](https://squareslab.github.io/papers-repo/bib/schulte-ase10.bib) |
[Code, Dataset, and Results]()


---

Search specifics
-----------------

Vinicius Paulo L. Oliveira, Eduardo F. D. Souza, Claire Le Goues, and Celso
G. Camilo-Junior. Improved Crossover Operators for Genetic Programming for
Program Repair. In Proceedings of the 8th International Symposium on Search
Based Software Engineering (SSBSE), 2016.  
[PDF](https://squareslab.github.io/papers-repo/pdfs/legoues-ssbse16.pdf) 
[DOI](https://doi.org/10.1007/978-3-319-47106-8_8)
[BibTeX](https://squareslab.github.io/papers-repo/bib/oliveira-ssbse16.bib)

Claire Le Goues, Westley Weimer, Stephanie Forrest: Representations and
Operators for Improving Evolutionary Software Repair. Genetic and Evolutionary
Computing Conference (GECCO), 2012.    
[PDF](https://squareslab.github.io/papers-repo/pdfs/genprog-gecco2012-preprint.pdf) 
[DOI](http://doi.acm.org/10.1145/2330163.2330296)
[BibTeX](https://squareslab.github.io/papers-repo/bib/legoues-gecco12.bib) |
[Code](https://github.com/squaresLab/genprog-code/tree/releases/v2.0)
[Virtual Machine Images](http://dijkstra.cs.virginia.edu/genprog/resources/genprog_images)
[Dataset](/experiments/index.html#icse12)
[Experimental Results](http://dijkstra.cs.virginia.edu/genprog/resources/genprog-gecco2012-results.tar.gz)

Ethan Fast, Claire Le Goues, Stephanie Forrest, Westley Weimer: Designing better
fitness functions for automated program repair. Genetic and Evolutionary
Computing Conference (GECCO), 2010.  
[PDF](https://squareslab.github.io/papers-repo/pdfs/weimer-gecco2010.pdf) 
[DOI](http://doi.acm.org/10.1145/1830483.1830654)
[BibTeX](https://squareslab.github.io/papers-repo/bib/fast-gecco10.bib)

---

Patch quality, software robustness
-----------------------------------

Edward K. Smith, Earl Barr, Claire Le Goues, and Yuriy Brun, Is the Cure Worse
than the Disease? Overfitting in Automated Program Repair, in Proceedings of the
10th Joint Meeting of the European Software Engineering Conference and ACM
SIGSOFT Symposium on the Foundations of Software Engineering (ESEC/FSE '15), 2015.  
[PDF](https://squareslab.github.io/papers-repo/pdfs/smith15fse.pdf) 
[BibTeX]()

Zachary P. Fry, Bryan Landau, Westley Weimer: A Human Study of Patch
Maintainability. International Symposium on Software Testing and Analysis
(ISSTA), 2012.  
[PDF](https://squareslab.github.io/papers-repo/pdfs/FryISSTA12_PREPRINT.pdf) 
[BibTeX]()

Eric Schulte, Zachary P. Fry, Ethan Fast, Westley Weimer, Stephanie Forrest:
Software Mutational Robustness. Journ. Genetic Programming and Evolvable
Machines 2013: July 28  
[PDF](https://squareslab.github.io/papers-repo/pdfs/weimer-gpem2013-robust.pdf) 
[BibTeX]()

---

Non-functional or quality properties:
-------------------------------------

Adam Brady, Jason Lawrence, Pieter Peers, Westley Weimer: genBRDF: Discovering
New analytic BRDFs with Genetic Programming. ACM Transactions on Graphics
(Proc. SIGGRAPH), 2014.  
[PDF](https://squareslab.github.io/papers-repo/pdfs/brady-siggraph2014.pdf) 
[BibTeX]()

<a id="asplos14"></a>Eric Schulte, Jonathan Dorn, Stephen Harding, Stephanie Forrest, Westley Weimer:
Post-compiler Software Optimization for Reducing Energy. Architectural Support
for Programming Languages and Operating Systems (ASPLOS), 2014.
[PDF](https://squareslab.github.io/papers-repo/pdfs/schulte-asplos2014.pdf) [BibTeX]()

Pitchaya Sitthi-amorn, Nicholas Modly, Westley Weimer, Jason Lawrence: Genetic
Programming for Shader Simplification. ACM Transactions on Graphics
(Proc. SIGGRAPH Asia) 30(6): 152 (2011)  
[PDF](https://squareslab.github.io/papers-repo/pdfs/sitthiamorn_siga11.pdf) [BibTeX]()
