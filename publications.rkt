#lang scribble/html
@(require racket/match
          "publication-data.rkt"
          "templates.rkt")

@;; NOTE: For now, the policy is that a paper should be on this page if one of
@;; the authors was a member of the lab when it was published (although many of
@;; the older papers have not followed this policy).

@(define OOPSLA "Object-Oriented Programming Systems, Languages, and Applications (OOPSLA)")
@(define PLDI "Programming Language Design and Implementation (PLDI)")
@(define ICFP "International Conference on Functional Programming (ICFP)")
@(define POPL "Principles of Programming Languages (POPL)")
@(define ESOP "European Symposium on Programming (ESOP)")
@(define JFP "Journal of Functional Programming (JFP)")
@(define ECOOP "European Conference on Object-Oriented Programming (ECOOP)")
@(define SNAPL "Summit on Advances in Programming Langugages (SNAPL)")
@(define CC "International Conference on Compiler Construction (CC)")
@(define SIGCSE "SIGCSE")
@(define TOPLAS "Transactions on Programming Languages and Systems (TOPLAS)")
@(define IFL "Implementation and Application of Functional Languages (IFL)")
@(define PADL "Practical Aspects of Declarative Languages (PADL)")
@(define TFP "Trends in Functional Programming (TFP)")
@(define DLS "Dynamic Languages Symposium (DLS)")
@(define TLDI "Types in Language Design and Implementation (TLDI)")
@(define Scheme "Scheme and Functional Programming Workshop")
@(define PPDP "Principles and Practice of Declarative Programming (PPDP)")
@(define ACL2 "ACL2 Workshop")
@(define FDPE "Functional and Declarative Programming in Education (FDPE)")
@(define GPCE "Generative Programming: Concepts & Experience (GPCE)")
@(define LMCS "Logical Methods in Computer Science (LMCS)")
@(define MSCS "Mathematical Structures in Computer Science (MSCS)")
@(define LICS "Logic in Computer Science (LICS)")
@(define DBPL "Database Programming Languages (DBPL)")
@(define TCS "Theoretical Computer Science")
@(define VMCAI "Verification, Model Checking, and Abstract Interpretation (VMCAI)")
@(define HOSC "Higher-Order and Symbolic Computation")
@(define LFP "LISP and Functional Programming (LFP)")
@(define SOCP "Science of Computer Programming")

@(define old-site-pubs
   (list
    (publication "Seeing the futures: profiling shared-memory parallel Racket"
                 "James Swaine, Burke Fetscher, Vincent St-Amour, Robby Findler and Matthew Flatt"
                 "Functional High-Performance Computing (FHPC)"
                 2012
                 "http://www.ccs.neu.edu/home/stamourv/papers/seeing-the-futures.pdf")
    (publication "Practical Programming with Substructural Types"
                 "Jesse A. Tov"
                 "PhD Dissertation, Northeastern University"
                 2012
                 "http://users.eecs.northwestern.edu/~jesse/pubs/dissertation/")
    (publication "A Family of Abstract Interpretations for Static Analysis of Concurrent Higher-Order Programs"
                 "Matthew Might and David Van Horn"
                 "The 18th International Static Analysis Symposium"
                 2011
                 "http://www.ccs.neu.edu/home/dvanhorn/pubs/might-vanhorn-sas11.pdf")
    (publication "A Theory of Substructural Types and Control"
                 "Jesse A. Tov and Riccardo Pucella"
                 OOPSLA
                 2011
                 "http://users.eecs.northwestern.edu/~jesse/pubs/substructural-control")
    (publication "Practical Affine Types"
                 "Jesse A. Tov and Riccardo Pucella"
                 POPL
                 2011
                 "http://users.eecs.northwestern.edu/~jesse/pubs/alms")
    (publication "Abstracting Abstract Machines"
                 "David Van Horn and Matthew Might"
                 ICFP
                 2010
                 "http://www.ccs.neu.edu/home/dvanhorn/pubs/vanhorn-might-icfp10.pdf")
    (publication "Pushdown Control-Flow Analysis of Higher-Order Programs"
                 "Christopher Earl, Matthew Might and David Van Horn"
                 "Scheme and Functional Programming Workshop"
                 2010
                 "http://www.ccs.neu.edu/home/dvanhorn/pubs/earl-might-vanhorn-sfp10.pdf")
    (publication "Resolving and Exploiting the k-CFA Paradox"
                 "Matthew Might, Yannis Smaragdakis and David Van Horn"
                 PLDI
                 2010
                 "http://www.ccs.neu.edu/home/dvanhorn/pubs/might-smaragdakis-vanhorn-pldi10.pdf")
    (publication "Evaluating Call By Need on the Control Stack"
                 "Stephen Chang, David Van Horn and Matthias Felleisen"
                 TFP
                 2010
                 "http://www.ccs.neu.edu/home/stchang/pubs/Chang-VanHorn-Felleisen-TFP2010.pdf")
    (publication "Functional Adaptive Programming"
                 "Bryan Chadwick"
                 "PhD Dissertation, Northeastern University"
                 2010
                 "http://www.ccs.neu.edu/home/chadwick/files/thesis-single.pdf")
    (publication "Algorithms for Traversal-Based Generic Programming"
                 "Bryan Chadwick and Karl Lieberherr"
                 "Workshop on Generic Programming"
                 2010
                 "http://www.ccs.neu.edu/home/chadwick/demeterf/papers/wgp10-final.pdf")
    (publication "Weaving Generic Programming and Traversal Performance"
                 "Bryan Chadwick and Karl Lieberherr"
                 "AOSD"
                 2010
                 "http://www.ccs.neu.edu/home/chadwick/demeterf/papers/aosd10-final.pdf")
    (publication "Stateful Contracts for Affine Types"
                 "Jesse A. Tov and Riccardo Pucella"
                 ESOP
                 2010
                 "http://users.eecs.northwestern.edu/~jesse/pubs/affine-contracts/affinecontracts10-bw.pdf")
    (publication "All-Terimation(T)"
                 "Panagiotis Manolios and Aaron Turon"
                 "TACAS"
                 2009
                 "http://www.ccs.neu.edu/home/turon/tacas09.pdf")
    (publication "Regular expression derivatives reexamined"
                 "Scott Owens, John Reppy and Aaron Turon"
                 JFP
                 2009
                 "http://www.ccs.neu.edu/home/turon/re-deriv.pdf")
    (publication "The Complexity of Flow Analysis in Higher-Order Languages"
                 "David Van Horn"
                 "PhD Dissertation, Brandeis University"
                 2009
                 "http://www.ccs.neu.edu/home/dvanhorn/pubs/vanhorn-dissertation.pdf")
    (publication "A Type System for Functional Traversal-Based Aspects"
                 "Bryan Chadwick and Karl Lieberherr"
                 "FOAL Workshop"
                 2009
                 "http://www.ccs.neu.edu/home/chadwick/demeterf/papers/foal09-final.pdf")
    (publication "Deciding kCFA is complete for EXPTIME"
                 "David Van Horn and Harry G. Mairson"
                 ICFP
                 2008
                 "http://www.ccs.neu.edu/home/dvanhorn/pubs/vanhorn-mairson-icfp08.pdf")
    (publication "Flow Analysis, Linearity, and PTIME"
                 "David Van Horn and Harry G. Mairson"
                 "Static Analysis Symposium"
                 2008
                 "http://www.ccs.neu.edu/home/dvanhorn/pubs/vanhorn-mairson-sas08.pdf")
    (publication "Variable-Arity Generic Interfaces"
                 "T. Stephen Strickland and Richard Cobbe and Matthias Felleisen"
                 "(tech report) Northeastern University College of Computer and Information Science no. NU-CCIS-08-01"
                 2008
                 "http://www.ccs.neu.edu/racket/pubs/NU-CCIS-08-01.pdf")
    (publication "Haskell Session Types with (Almost) No Class"
                 "Riccardo Pucella and Jesse A. Tov"
                 "SIGPLAN Symposium on Haskell"
                 2008
                 "http://users.eecs.northwestern.edu/~jesse/pubs/haskell-session-types/")
    (publication "Caml-Shcaml: An OCaml Library for Unix Shell Programming"
                 "Alec Heller and Jesse A. Tov"
                 "SIGPLAN workshop on ML"
                 2008
                 "http://users.eecs.northwestern.edu/~jesse/pubs/caml-shcaml/")
    (publication "Much Ado about Nothing: Putting Java's Null in its Place"
                 "Richard Cobbe"
                 "PhD Dissertation, Northeastern University"
                 2008
                 "http://www.ccs.neu.edu/racket/pubs/dissertation-cobbe.pdf")
    (publication "Metaprogramming with Traits"
                 "John Reppy and Aaron Turon"
                 ECOOP
                 2007
                 "http://www.ccs.neu.edu/home/turon/ecoop07-meta-traits.pdf")
    (publication "Advanced Macrology and the Implementation of Typed Scheme"
                 "Ryan Culpepper, Sam Tobin-Hochstadt and Matthew Flatt"
                 "Scheme Workshop"
                 2007
                 "http://www.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf")
    (publication "Relating Complexity and Precision in Control Flow Analysis"
                 "David Van Horn and Harry G. Mairson"
                 ICFP
                 2007
                 "http://www.ccs.neu.edu/home/dvanhorn/pubs/vanhorn-mairson-icfp07.pdf")
    (publication "Status Report: Specifying JavaScript with ML"
                 "David Herman and Cormac Flanagan"
                 "Workshop on ML"
                 2007
                 "http://www.ccs.neu.edu/home/dherman/research/papers/ml07-javascript.pdf")
    (publication "Functional Pearl: The Great Escape. Or, How to Jump the Border Without Getting Caught"
                 "David Herman"
                 ICFP
                 2007
                 "http://www.ccs.neu.edu/home/dherman/research/papers/icfp07-great-escape.pdf")
    (publication "Space-Efficient Gradual Typing"
                 "David Herman, Aaron Tomb and Cormac Flanagan"
                 TFP
                 2007
                 "http://www.ccs.neu.edu/home/dherman/research/papers/tfp07-gradual-typing.pdf")))

@(define mf-pubs
   (list
     (publication "Is sound gradual typing dead?"
                  "Asumu Takikawa, Daniel Feltey, Ben Greenman, Max S. New, Jan Vitek, and Matthias Felleisen"
                  POPL
                  2016
                  "http://www.ccs.neu.edu/racket/pubs/popl16-tfgnvf.pdf")
     (publication "Toward practical gradual typing"
                   "Asumu Takikawa, Daniel Feltey, Earl Dean, Matthew Flatt, Robert Bruce Findler, Sam Tobin-Hochstadt, and Matthias Felleisen"
                   ECOOP
                   2015
                   "http://www.ccs.neu.edu/racket/pubs/ecoop2015-takikawa-et-al.pdf")
     (publication "The Racket Manifesto"
                  "Matthias Felleisen, Robert Bruce Findler, Matthew Flatt, Shriram Krishnamurthi, Eli Barzilay, Jay McCarthy, Sam Tobin-Hochstadt"
                  SNAPL
                  2015
                  "http://www.ccs.neu.edu/racket/pubs/manifesto.pdf")
     (publication "Feature-specific Profiling"
                  "Vincent St-Amour, Leif Andersen, Matthias Felleisen"
                  CC
                  2015
                  "http://www.ccs.neu.edu/racket/pubs/cc15-saf.pdf")
     (publication "Transferring Skills at Solving Word Problems from Computing to Algebra Through Bootstrap"
                  "Emmanuel Schanzer, Kathi Fisler, Shriram Krishnamurthi, Matthias Felleisen"
                  SIGCSE
                  2015
                  "http://www.ccs.neu.edu/racket/pubs/sigcse-sfkf.pdf")
     (publication "The Network as a Language Construct"
                  "Tony Garnock-Jones, Sam Tobin-Hochstadt, and Matthias Felleisen"
                  ESOP
                  2014
                  "http://www.ccs.neu.edu/racket/pubs/esop14-gjthf.pdf")
     (publication "Profiling for Laziness"
                  "Stephen Chang, Matthias Felleisen"
                  POPL
                  2014
                  "http://www.ccs.neu.edu/racket/pubs/popl14-cf.pdf")
     (publication "Contracts for First-Class Classes"
                  "T. Stephen Strickland, Christos Dimoulas, Asumu Takikawa, and Matthias Felleisen"
                  TOPLAS
                  2013
                  "http://www.ccs.neu.edu/racket/pubs/toplas13-sdtf.pdf")
     (publication "Option Contracts"
                  "Christos Dimoulas, Robert Bruce Findler, Matthias Felleisen"
                  OOPSLA
                  2013
                  "http://www.ccs.neu.edu/racket/pubs/oopsla13-dff.pdf")
     (publication "Gradual Typing for First-Class Classes"
                  "Asumu Takikawa, T. Stephen Strickland, Christos Dimoulas, Sam Tobin-Hochstadt, Matthias Felleisen"
                  OOPSLA
                  2012
                  "http://www.ccs.neu.edu/racket/pubs/oopsla12-tsdthf.pdf")
     (publication "Optimization Coaching"
                  "Vincent St-Amour, Sam Tobin-Hochstadt, Matthias Felleisen"
                  OOPSLA
                  2012
                  "http://www.ccs.neu.edu/racket/pubs/oopsla12-stf.pdf")
     (publication "The Call-by-need Lambda Calculus, Revisited"
                  "Stephen Chang and Matthias Felleisen"
                  ESOP
                  2012
                  "http://www.ccs.neu.edu/racket/pubs/esop12-cf.pdf")
     (publication "Complete Monitors for Behavioral Contracts"
                  "Christos Dimoulas, Sam Tobin-Hochstadt, and Matthias Felleisen"
                  ESOP
                  2012
                  "http://www.ccs.neu.edu/racket/pubs/esop12-dthf.pdf")
     (publication "From Stack Traces to Lazy Rewriting Sequences"
                  "Stephen Chang, Eli Barzilay, John Clements, Matthias Felleisen"
                  IFL
                  2011
                  "http://www.ccs.neu.edu/racket/pubs/ifl11-cbcf.pdf")
     (publication "On Contract Satisfaction in a Higher-Order World"
                  "Christos Dimoulas, Matthias Felleisen"
                  TOPLAS
                  2011
                  "http://www.ccs.neu.edu/racket/pubs/df-toplas11.pdf")
     (publication "Run Your Research"
                  "Casey Klein, John Clements, Christos Dimoulas, Carl Eastlund, Matthias Felleisen, Matthew Flatt, Jay McCarthy, Jon Rafkind, Sam Tobin-Hochstadt, Robert Bruce Findler"
                  POPL
                  2012
                  "http://eecs.northwestern.edu/~robby/lightweight-metatheory/popl2012-kcdeffmrtf.pdf")
     (publication "Typing the Numeric Tower"
                  "Vincent St-Amour, Sam Tobin-Hochstadt, Matthew Flatt, and Matthias Felleisen"
                  PADL
                  2012
                  "http://www.ccs.neu.edu/racket/pubs/padl12-stff.pdf")
     (publication "Languages as Libraries"
                  "Sam Tobin-Hochstadt, Vincent St-Amour, Ryan Culpepper, Matthew Flatt, Matthias Felleisen"
                  PLDI
                  2011
                  "http://www.ccs.neu.edu/racket/pubs/pldi11-thacff.pdf")
     (publication "Correct Blame for Contracts: No More Scapegoating"
                  "Christos Dimoulas, Robert Bruce Findler, Cormac Flanagan, Matthias Felleisen"
                  POPL
                  2011
                  "http://www.ccs.neu.edu/racket/pubs/popl11-dfff.pdf")
     (publication "Hygienic Macros for ACL2"
                  "Carl Eastlund, Matthias Felleisen"
                  TFP
                  2010
                  "http://www.ccs.neu.edu/racket/pubs/tfp10-ef.pdf")
     (publication "Contracts for First-Class Classes"
                  "T. Stephen Strickland, Matthias Felleisen"
                  DLS
                  2010
                  "http://www.ccs.neu.edu/racket/pubs/dls10-sf.pdf")
     (publication "Fortifying Macros"
                  "Ryan Culpepper, Matthias Felleisen"
                  ICFP
                  2010
                  "http://www.ccs.neu.edu/racket/pubs/icfp10-cf.pdf")
     (publication "Logical Types for Untyped Languages"
                  "Sam Tobin-Hochstadt, Matthias Felleisen"
                  ICFP
                  2010
                  "http://www.ccs.neu.edu/racket/pubs/icfp10-thf.pdf")
     (publication "TeachScheme!---A Checkpoint (Abstract)"
                  "Matthias Felleisen"
                  ICFP
                  2010
                  "http://www.ccs.neu.edu/racket/pubs/icfp10-f.pdf")
     (publication "Adding Types to Untyped Languages (Abstract)"
                  "Matthias Felleisen"
                  TLDI
                  2010
                  "http://www.ccs.neu.edu/racket/pubs/tldi10-f.pdf")
     (publication "Nested and Dynamic Contract Boundaries"
                  "T. Stephen Strickland, Matthias Felleisen"
                  IFL
                  2009
                  "http://www.ccs.neu.edu/racket/pubs/ifl09-sf.pdf")
     (publication "Contracts for First-Class Modules"
                  "T. Stephen Strickland, Matthias Felleisen"
                  DLS
                  2009
                  "http://www.ccs.neu.edu/racket/pubs/dls09-sf.pdf")
     (publication "Sequence Traces for Object-Oriented Executions"
                  "Carl Eastlund, Matthias Felleisen"
                  Scheme
                  2009
                  "http://www.ccs.neu.edu/racket/pubs/scheme2009-ef.pdf")
     (publication "Making Induction Manifest in Modular ACL2"
                  "Carl Eastlund, Matthias Felleisen"
                  PPDP
                  2009
                  "http://www.ccs.neu.edu/racket/pubs/ppdp09-ef.pdf")
     (publication "Future Contracts"
                  "Christos Dimoulas, Riccardo Pucella, Matthias Felleisen"
                  PPDP
                  2009
                  "http://www.ccs.neu.edu/racket/pubs/ppdp09-dpf.pdf")
     (publication "Ryan Culpepper, Matthias Felleisen"
                  "Debugging Hygienic Macros"
                  "Science of Computer Programming"
                  2009
                  "http://www.ccs.neu.edu/racket/pubs/cf-sp09.pdf")
     (publication "A Functional I/O System (or Fun for Freshman Kids)"
                  "Matthias Felleisen, Robert Bruce Findler, Matthew Flatt, Shriram Krishnamurthi"
                  ICFP
                  2009
                  "http://www.ccs.neu.edu/racket/pubs/icfp09-fffk.pdf")
     (publication "Automatic Verification for Interactive Graphical Programs"
                  "Carl Eastlund and Matthias Felleisen"
                  ACL2
                  2009
                  "http://www.ccs.neu.edu/racket/pubs/acl209-ef.pdf")
     (publication "Practical Variable-Arity Polymorphism"
                  "T. Stephen Strickland, Sam Tobin-Hochstadt, and Matthias Felleisen"
                  ESOP
                  2009
                  "http://www.ccs.neu.edu/racket/pubs/esop09-sthf.pdf")
     (publication "Toward a Practical Module System for ACL2"
                  "Carl Eastlund and Matthias Felleisen"
                  PADL
                  2009
                  "http://www.ccs.neu.edu/racket/pubs/padl09-ef.pdf")
     (publication "Functional Programming and Theorem Proving for Undergraduates: A Progress Report"
                  "Rex Page, Carl Eastlund, and Matthias Felleisen"
                  FDPE
                  2008
                  "http://www.ccs.neu.edu/racket/pubs/fdpe08-pef.pdf")
     (publication "The Design and Implementation of Typed Scheme"
                  "Sam Tobin-Hochstadt, Matthias Felleisen"
                  POPL
                  2008
                  "http://www.ccs.neu.edu/racket/pubs/popl08-thf.pdf")
     (publication "ACL2 for Freshmen: First Experiences"
                  "Carl Eastlund, Dale Vaillancourt, Matthias Felleisen"
                  ACL2
                  2007
                  "http://www.ccs.neu.edu/racket/pubs/acl207-evf.pdf")
     (publication "Debugging Macros"
                  "Ryan Culpepper, Matthias Felleisen"
                  GPCE
                  2007
                  "http://www.ccs.neu.edu/racket/pubs/gpce07-cf.pdf")
     (publication "Adding Delimited and Composable Control to a Production Programming Environment"
                  "Matthew Flatt, Gang Yu, Robert Bruce Findler, Matthias Felleisen"
                  ICFP
                  2007
                  "http://www.ccs.neu.edu/racket/pubs/icfp07-fyff.pdf")
     (publication "Implementation and Use of the PLT Scheme Web Server"
                  "Shriram Krishnamurthi, Peter Walton Hopkins, Jay McCarthy, Paul T. Graunke, Greg Pettyjohn, Matthias Felleisen"
                  HOSC
                  2007
                  "http://www.ccs.neu.edu/racket/pubs/hosc07-sk-mf.pdf")))

@(define amal-pubs
   (list
     (publication "Noninterference for Free"
                  "William J. Bowman and Amal Ahmed"
                  ICFP
                  2015
                  "http://www.ccs.neu.edu/home/amal/papers/nifree.pdf")
     (publication "Verified Compilers for a Multi-Language World"
                  "Amal Ahmed"
                  SNAPL
                  2015
                  "http://www.ccs.neu.edu/home/amal/papers/verifcomp.pdf")
     (publication "Database Queries that Explain their Work"
                  "James Cheney, Amal Ahmed, and Umut Acar"
                  PPDP
                  2014
                  "http://www.ccs.neu.edu/home/amal/papers/dqew.pdf")
     (publication "Verifying an Open Compiler Using Multi-Language Semantics"
                  "James T. Perconti and Amal Ahmed"
                  ESOP
                  2014
                  "http://www.ccs.neu.edu/home/amal/papers/voc.pdf")
     (publication "Logical Relations for Fine-Grained Concurrency"
                  "Aaron Turon, Jacob Thamsborg, Amal Ahmed, Lars Birkedal, Derek Dreyer"
                  POPL
                  2013
                  "http://www.ccs.neu.edu/home/amal/papers/relcon.pdf")
     (publication "An Equivalence-Preserving CPS Translation via Multi-Language Semantics"
                  "Amal Ahmed and Matthias Blume"
                  ICFP
                  2011
                  "http://www.ccs.neu.edu/home/amal/papers/epc.pdf")
     (publication "Logical Step-Indexed Logical Relations"
                  "Derek Dreyer, Amal Ahmed, and Lars Birkedal"
                  LMCS
                  2011
                  "http://www.ccs.neu.edu/home/amal/papers/lslr-lmcs.pdf")
     (publication "Blame for All"
                  "Amal Ahmed, Robert Bruce Findler, Jeremy Siek, and Philip Wadler"
                  POPL
                  2011
                  "http://plt.eecs.northwestern.edu/blame-for-all/")
     (publication "Provenance as Dependency Analysis"
                  "James Cheney, Amal Ahmed, and Umut Acar"
                  MSCS
                  2011
                  "http://www.ccs.neu.edu/home/amal/papers/provdep-mscs.pdf")
     (publication "Semantic Foundations for Typed Assembly Languages."
                  "Amal Ahmed, Andrew W. Appel, Christopher Richards, Kedar Swadi, Gang Tan, Daniel Wang"
                  TOPLAS
                  2010
                  "http://portal.acm.org/citation.cfm?doid=1709093.1709094")
     (publication "Logical Step-Indexed Logical Relations"
                  "Derek Dreyer, Amal Ahmed, and Lars Birkedal"
                  LICS
                  2009
                  "http://www.ccs.neu.edu/home/amal/papers/lslr.pdf")
     (publication "Blame for All"
                  "Amal Ahmed, Robert Bruce Findler, Jacob Matthews, and Philip Wadler"
                  "Workshop on Script to Program Evolution (STOP)"
                  2009
                  "http://www.ccs.neu.edu/home/amal/papers/blame-all.pdf")
     (publication "State-Dependent Representation Independence"
                  "Amal Ahmed, Derek Dreyer, and Andreas Rossberg"
                  POPL
                  2009
                  "http://www.ccs.neu.edu/home/amal/papers/sdri.pdf")
     (publication "Typed Closure Conversion Preserves Observational Equivalence"
                  "Amal Ahmed and Matthias Blume"
                  ICFP
                  2008
                  "http://www.ccs.neu.edu/home/amal/papers/tccpoe.pdf")
     (publication "Parametric Polymorphism Through Run-Time Sealing, or, Theorems for Low, Low Prices!"
                  "Jacob Matthews and Amal Ahmed"
                  ESOP
                  2008
                  "http://www.ccs.neu.edu/home/amal/papers/parpolyseal.pdf")
     (publication "Imperative Self-Adjusting Computation"
                  "Umut Acar, Amal Ahmed, and Matthias Blume"
                  POPL
                  2008
                  "http://www.ccs.neu.edu/home/amal/papers/impselfadj.pdf")
     (publication "Provenance as Dependency Analysis"
                  "James Cheney, Amal Ahmed, and Umut Acar"
                  DBPL
                  2007
                  "http://www.ccs.neu.edu/home/amal/papers/provdep.pdf")
     (publication "L3: A Linear Language with Locations"
                  "Amal Ahmed, Matthew Fluet, and Greg Morrisett"
                  "Fundamenta Informaticae"
                  2007
                  "http://www.ccs.neu.edu/home/amal/papers/linloc-fi07.pdf")
     (publication "Abstract Predicates and Mutable ADTs in Hoare Type Theory"
                  "Aleksander Nanevski, Amal Ahmed, Greg Morrisett, and Lars Birkedal"
                  ESOP
                  2007
                  "http://www.ccs.neu.edu/home/amal/papers/htthol.pdf")))

@(define jan-pubs
   (list
    (publication "Concrete Types for TypeScript"
                 "Gregor Richards, Francesco Zappa Nardelli, and Jan Vitek"
                 ECOOP
                 2015
                 "http://dx.doi.org/10.4230/LIPIcs.ECOOP.2015.76")
    (publication "Cooking the Books: Formalizing JMM Implementation Recipes"
                 "Gustavo Petri, Jan Vitek, and Suresh Jagannathan"
                 ECOOP
                 2015
                 "http://dx.doi.org/10.4230/LIPIcs.ECOOP.2015.445")
    (publication "Repeatability, reproducibility and rigor in CS research"
                 "Jan Vitek"
                 "PLMW@POPL"
                 2015
                 "http://doi.acm.org/10.1145/2792434.2792446")
    (publication "Atomicity Refinement for Verified Compilation"
                 "Suresh Jagannathan, Vincent Laporte, Gustavo Petri, David Pichardie, and Jan Vitek"
                 TOPLAS
                 2014
                 "http://doi.acm.org/10.1145/2601339")
    (publication "M3: high-performance memory management from off-the-shelf components"
                 "David Terei, Alex Aiken, and Jan Vitek"
                 "ISMM"
                 2014
                 "http://doi.acm.org/10.1145/2602988.2602995")
    (publication "Atomicity refinement for verified compilation"
                 "Suresh Jagannathan, Gustavo Petri, Jan Vitek, David Pichardie, and Vincent Laporte"
                 PLDI
                 2014
                 "http://doi.acm.org/10.1145/2594291.2594346")
    (publication "A fast abstract syntax tree interpreter for R"
                 "Tomas Kalibera, Petr Maj, Flor, and Jan Vitek"
                 "VEE"
                 2014
                 "http://doi.acm.org/10.1145/2576195.2576205")
    (publication "The case for the three R's of systems research: repeatability, reproducibility and rigor"
                 "Jan Vitek"
                 "VEE"
                 2014
                 "http://doi.acm.org/10.1145/2576195.2576216")))

@(define olin-pubs
   (list
    (publication "An Array-Oriented Language with Static Rank Polymorphism"
                 "Justin Slepak, Olin Shivers, and Panagiotis Manolios"
                 ESOP
                 2014
                 "http://dx.doi.org/10.1007/978-3-642-54833-8_3")
    (publication "CFA2: a Context-Free Approach to Control-Flow Analysis"
                 "Dimitrios Vardoulakis and Olin Shivers"
                 ESOP
                 2010
                 "http://www.ccs.neu.edu/home/dimvar/papers/cfa2-NU-CCIS-10-01.pdf")
    (publication "Modular rollback through control logging: a pair of twin functional pearls"
                 "Olin Shivers and Aaron Joseph Turon"
                 ICFP
                 2011
                 "http://doi.acm.org/10.1145/2034773.2034783")
    (publication "Pushdown flow analysis of first-class control"
                 "Dimitrios Vardoulakis and Olin Shivers"
                 ICFP
                 2011
                 "http://www.ccs.neu.edu/home/dimvar/papers/cfa2-1st-class.pdf")
    (publication "Ordering multiple continuations on the stack"
                 "Dimitrios Vardoulakis and Olin Shivers"
                 "PEPM"
                 2011
                 "http://www.ccs.neu.edu/home/dimvar/papers/rcps-NU-CCIS-11-01.pdf")
    (publication "Bottom-up beta-reduction: Uplinks and lambda-DAGs"
                 "Olin Shivers and Mitchell Wand"
                 "Fundamenta Informaticae"
                 2010
                 "http://dx.doi.org/10.3233/FI-2010-328")
    (publication "CFA2: A Context-Free Approach to Control-Flow Analysis"
                 "Dimitrios Vardoulakis and Olin Shivers"
                 ESOP
                 2010
                 "http://dx.doi.org/10.1007/978-3-642-11957-6_30")
    (publication "Building language towers with Ziggurat"
                 "David Fisher and Olin Shivers"
                 JFP
                 2008
                 "http://dx.doi.org/10.1017/S0956796808006928")
    (publication "Exploiting reachability and cardinality in higher-order flow analysis"
                 "Matthew Might and Olin Shivers"
                 JFP
                 2008
                 "http://dx.doi.org/10.1017/S0956796808006941")
    (publication "Why teach programming languages"
                 "Olin Shivers"
                 "SIGPLAN Notices"
                 2008
                 "http://doi.acm.org/10.1145/1480828.1480856")
    (publication "Trusted Theorem Proving: A Case Study in SLD-Resolution"
                 "Konstantine Arkoudas and Olin Shivers"
                 "ISoLA"
                 2008
                 "http://dx.doi.org/10.1007/978-3-540-88479-8_56")
    (publication "Analyzing the environment structure of higher-order languages using frame strings"
                 "Matthew Might and Olin Shivers"
                 TCS
                 2007
                 "http://dx.doi.org/10.1016/j.tcs.2006.12.031")
    (publication "Model Checking Via GammaCFA"
                 "Matthew Might, Benjamin Chambers, and Olin Shivers"
                 VMCAI
                 2007
                 "http://dx.doi.org/10.1007/978-3-540-69738-1_4")
    (publication "Multi-return function call"
                 "Olin Shivers and David Fisher"
                 JFP
                 2006
                 "http://dx.doi.org/10.1017/S0956796806006009")
    (publication "Improving flow analyses via GammaCFA: abstract garbage collection and counting"
                 "Matthew Might and Olin Shivers"
                 ICFP
                 2006
                 "http://doi.acm.org/10.1145/1159803.1159807")
    (publication "Static analysis for syntax objects"
                 "David Fisher and Olin Shivers"
                 ICFP
                 2006
                 "http://doi.acm.org/10.1145/1159803.1159817")
    (publication "Continuations and transducer composition"
                 "Olin Shivers and Matthew Might"
                 PLDI
                 2006
                 "http://doi.acm.org/10.1145/1133981.1134016")
    (publication "Environment analysis via Delta CFA"
                 "Matthew Might and Olin Shivers"
                 POPL
                 2006
                 "http://doi.acm.org/10.1145/1111037.1111049")))

@(define mitch-pubs
   (list
    (publication "Romeo: a system for more flexible binding-safe programming"
                 "Paul Stansifer and Mitchell Wand"
                 ICFP
                 2014
                 "http://doi.acm.org/10.1145/2628136.2628162")
    (publication "A Resource Analysis of the π-calculus"
                 "Aaron Joseph Turon and Mitchell Wand"
                 "Mathematical Foundations of Programming Semantics (MFPS)"
                 2011
                 "http://dx.doi.org/10.1016/j.entcs.2011.09.028")
    (publication "Parsing reflective grammars"
                 "Paul Stansifer and Mitchell Wand"
                 "LDTA"
                 2011
                 "http://doi.acm.org/10.1145/1988783.1988793")
    (publication "A separation logic for refining concurrent objects"
                 "Aaron Joseph Turon and Mitchell Wand"
                 POPL
                 2011
                 "http://doi.acm.org/10.1145/1926385.1926415")
    (publication "The Higher-Order Aggregate Update Problem"
                 "Christos Dimoulas and Mitchell Wand"
                 VMCAI
                 2009
                 "http://dx.doi.org/10.1007/978-3-540-93900-9_8")
    (publication "Essentials of programming languages (3. ed.)"
                 "Daniel P. Friedman and Mitchell Wand"
                 "MIT Press"
                 2008
                 "http://www.eopl3.com/")
    (publication "Programming languages: fundamental concepts for expanding and disciplining the mind"
                 "Mitchell Wand and Daniel P. Friedman"
                 "SIGPLAN Notices"
                 2008
                 "http://doi.acm.org/10.1145/1480828.1480857")
    (publication "A Compositional Trace Semantics for Orc"
                 "Dimitrios Vardoulakis and Mitchell Wand"
                 "COORDINATION"
                 2008
                 "http://www.ccs.neu.edu/home/dimvar/papers/orc-coord.pdf")
    (publication "A Theory of Hygienic Macros"
                 "David Herman and Mitchell Wand"
                 ESOP
                 2008
                 "http://dx.doi.org/10.1007/978-3-540-78739-6_4")
    (publication "On the correctness of the Krivine machine"
                 "Mitchell Wand"
                 HOSC
                 2007
                 "http://dx.doi.org/10.1007/s10990-007-9019-8")
    (publication "Bisimulations for Untyped Imperative Objects"
                 "Vasileios Koutavas and Mitchell Wand"
                 ESOP
                 2006
                 "http://dx.doi.org/10.1007/11693024_11")
    (publication "Small bisimulations for reasoning about higher-order imperative programs"
                 "Vasileios Koutavas and Mitchell Wand"
                 POPL
                 2006
                 "http://doi.acm.org/10.1145/1111037.1111050")
    (publication "Selectors Make Set-Based Analysis Too Hard"
                 "Philippe Meunier, Robert Bruce Findler, Paul Steckler, and Mitchell Wand"
                 HOSC
                 2005
                 "http://dx.doi.org/10.1007/s10990-005-4876-5")
    (publication "A semantics for advice and dynamic join points in aspect-oriented programming"
                 "Mitchell Wand, Gregor Kiczales, and Christopher Dutchyn"
                 TOPLAS
                 2004
                 "http://doi.acm.org/10.1145/1018203.1018208")
    (publication "Relating models of backtracking"
                 "Mitchell Wand and Dale Vaillancourt"
                 ICFP
                 2004
                 "http://doi.acm.org/10.1145/1016850.1016861")
    (publication "CPS transformation of flow information"
                 "Jens Palsberg and Mitchell Wand"
                 JFP
                 2003
                 "http://dx.doi.org/10.1017/S0956796802004513")
    (publication "Understanding aspects: extended abstract"
                 "Mitchell Wand"
                 ICFP
                 2003
                 "http://doi.acm.org/10.1145/944705.944732")
    (publication "A Modular, Extensible Proof Method for Small-Step Flow Analyses"
                 "Mitchell Wand and Galen B. Williamson"
                 ESOP
                 2002
                 "http://dx.doi.org/10.1007/3-540-45927-8_16")
    (publication "Essentials of programming languages (2. ed.)"
                 "Daniel P. Friedman, Mitchell Wand, and Christopher T. Haynes"
                 "MIT Press"
                 2001
                 "http://www.cs.indiana.edu/eopl/")
    (publication "Set constraints for destructive array update optimization"
                 "Mitchell Wand and William D. Clinger"
                 JFP
                 2001
                 "http://dx.doi.org/10.1017/S0956796801003938")
    (publication "A Semantics for Advice and Dynamic Join Points in Aspect-Oriented Programming"
                 "Mitchell Wand"
                 "SAIG"
                 2001
                 "http://dx.doi.org/10.1007/3-540-44806-3_3")
    (publication "Analysis-based program transformations"
                 "Mitchell Wand"
                 "ACM SIGSOFT Software Engineering Notes"
                 2000
                 "http://doi.acm.org/10.1145/340855.341041")
    (publication "Optimizing memory usage in higher-order programming languages: theoretical and experimental studies"
                 "Mitchell Wand and William D. Clinger"
                 "ACM SIGSOFT Software Engineering Notes"
                 2000
                 "http://doi.acm.org/10.1145/340855.341042")
    (publication "Continuation-Based Multiprocessing Revisited"
                 "Mitchell Wand"
                 HOSC
                 1999
                 "http://dx.doi.org/10.1023/A:1010049917750")
    (publication "Continuation-Based Multiprocessing"
                 "Mitchell Wand"
                 HOSC
                 1999
                 "http://dx.doi.org/10.1023/A:1010093700911")
    (publication "Trampolined Style"
                 "Steven E. Ganz, Daniel P. Friedman, and Mitchell Wand"
                 ICFP
                 1999
                 "http://doi.acm.org/10.1145/317636.317779")
    (publication "A Language for Specifying Recursive Traversals of Object Structures"
                 "Johan Ovlinger and Mitchell Wand"
                 OOPSLA
                 1999
                 "http://doi.acm.org/10.1145/320384.320391")
    (publication "Constraint Systems for Useless Variable Elimination"
                 "Mitchell Wand and Igor Siveroni"
                 POPL
                 1999
                 "http://doi.acm.org/10.1145/292540.292567")
    (publication "The Theory of Fexprs is Trivial"
                 "Mitchell Wand"
                 "Lisp and Symbolic Computation"
                 1998
                 "http://www.ccs.neu.edu/home/wand/papers/fexprs.ps")
    (publication "Revised Report on the Algorithmic Language Scheme"
                 "Harold Abelson, R. Kent Dybvig, Christopher T. Haynes, Guillermo Juan Rozas, N. I. Adams IV, Daniel P. Friedman, Eugene E. Kohlbecker, Guy L. Steele Jr., David H. Bartley, Robert H. Halstead Jr., Don Oxley, Gerald J. Sussman, G. Brooks, Chris Hanson, Kent M. Pitman, and Mitchell Wand"
                 HOSC
                 1998
                 "http://dx.doi.org/10.1023/A:1010051815785")
    (publication "Set Constraints for Destructive Array Update Optimization"
                 "Mitchell Wand and William D. Clinger"
                 "ICCL"
                 1998
                 "http://www.ccs.neu.edu/home/wand/papers/jfp-01.ps")
    (publication "Type Inference with Non-Structural Subtyping"
                 "Jens Palsberg, Mitchell Wand, and Patrick O'Keefe"
                 "Formal Aspects of Computing"
                 1997
                 "http://dx.doi.org/10.1007/BF01212524")
    (publication "Lightweight Closure Conversion"
                 "Paul Steckler and Mitchell Wand"
                 TOPLAS
                 1997
                 "http://doi.acm.org/10.1145/239912.239915")
    (publication "Denotational Semantics Using an Operationally-Based Term Model"
                 "Mitchell Wand and Gregory T. Sullivan"
                 POPL
                 1997
                 "http://doi.acm.org/10.1145/263699.263755")
    (publication "Untyped Lambda-Calculus with Input-Output"
                 "Jerzy Tiuryn and Mitchell Wand"
                 "CAAP"
                 1996
                 "http://dx.doi.org/10.1007/3-540-61064-2_46")
    (publication "Compiler Correctness for Concurrent Languages"
                 "David S. Gladstein and Mitchell Wand"
                 "COORDINATION"
                 1996
                 "http://dx.doi.org/10.1007/3-540-61052-9_49")
    (publication "Modeling Subobject-based Inheritance"
                 "Jonathan G. Rossie Jr., Daniel P. Friedman, and Mitchell Wand"
                 ECOOP
                 1996
                 "http://dx.doi.org/10.1007/BFb0053065")
    (publication "VLISP: A Verified Implementation of Scheme"
                 "Joshua D. Guttman, John D. Ramsdell, and Mitchell Wand"
                 "Lisp and Symbolic Computation"
                 1995
                 "http://link.springer.com/article/10.1007/BF01128406")
    (publication "The VLISP Verified PreScheme Compiler"
                 "Dino Oliva, John D. Ramsdell, and Mitchell Wand"
                 "Lisp and Symbolic Computation"
                 1995
                 "http://link.springer.com/article/10.1007/BF01128408")
    (publication "Strong Normalization with Non-Structural Subtyping"
                 "Mitchell Wand, Patrick O'Keefe, and Jens Palsberg"
                 "Mathematical Structures in Computer Science"
                 1995
                 "http://dx.doi.org/10.1017/S0960129500000815")
    (publication "Compiler Correctness for Parallel Languages"
                 "Mitchell Wand"
                 "FPCA"
                 1995
                 "http://doi.acm.org/10.1145/224164.224193")
    (publication "Conditional Lambda-Theories and the Verification of Static Properties of Programs"
                 "Mitchell Wand and Zheng-Yu Wang"
                 "Information and Computation"
                 1994
                 "http://dx.doi.org/10.1006/inco.1994.1072")
    (publication "Selective and Lightweight Closure Conversion"
                 "Mitchell Wand and Paul Steckler"
                 POPL
                 1994
                 "http://doi.acm.org/10.1145/174675.178044")
    (publication "Selective Thunkification"
                 "Paul Steckler and Mitchell Wand"
                 "SAS"
                 1994
                 "http://dx.doi.org/10.1007/3-540-58485-4_39")
    (publication "Specifying the Correctness of Binding-Time Analysis"
                 "Mitchell Wand"
                 JFP
                 1993
                 "http://dx.doi.org/10.1017/S0956796800000782")
    (publication "Specifying the Correctness of Binding-Time Analysis"
                 "Mitchell Wand"
                 POPL
                 1993
                 "http://doi.acm.org/10.1145/158511.158614")
    (publication "Type Reconstruction with Recursive Types and Atomic Subtyping"
                 "Jerzy Tiuryn and Mitchell Wand"
                 "TAPSOFT"
                 1993
                 "http://dx.doi.org/10.1007/3-540-56610-4_98")
    (publication "Essentials of programming languages"
                 "Daniel P. Friedman, Mitchell Wand, and Christopher T. Haynes"
                 "MIT Press"
                 1992
                 "http://www.cs.indiana.edu/eip/eopl.html")
    (publication "Type Inference for Partial Types is Decidable"
                 "Patrick O'Keefe and Mitchell Wand"
                 ESOP
                 1992
                 "http://dx.doi.org/10.1007/3-540-55253-7_24")
    (publication "Proving the Correctness of Storage Representations"
                 "Mitchell Wand and Dino Oliva"
                 LFP
                 1992
                 "http://doi.acm.org/10.1145/141471.141528")
    (publication "Type Inference for Record Concatenation and Multiple Inheritance"
                 "Mitchell Wand"
                 "Information and Computation"
                 1991
                 "http://dx.doi.org/10.1016/0890-5401(91)90050-C")
    (publication "Correctness of Static Flow Analysis in Continuation Semantics"
                 "Margaret Montenyohl and Mitchell Wand"
                 SOCP
                 1991
                 "http://dx.doi.org/10.1016/0167-6423(91)90021-O")
    (publication "Automatic Dimensional Inference"
                 "Mitchell Wand and Patrick O'Keefe"
                 "Computational Logic - Essays in Honor of Alan Robinson"
                 1991
                 "http://www.ccs.neu.edu/home/wand/papers/dimensions.ps")
    (publication "Correctness of Procedure Representations in Higher-Order Assembly Language"
                 "Mitchell Wand"
                 "Mathematical Foundations of Programming Semantics (MFPS)"
                 1991
                 "http://dx.doi.org/10.1007/3-540-55511-0_15")
    (publication "A Short Proof of the Lexical Addressing Algorithm"
                 "Mitchell Wand"
                 "Information Processing Letters"
                 1990
                 "http://dx.doi.org/10.1016/0020-0190(90)90165-T")
    (publication "Conditional Lambda-Theories and the Verification of Static Properties of Programs"
                 "Mitchell Wand and Zheng-Yu Wang"
                 LICS
                 1990
                 "http://dx.doi.org/10.1109/LICS.1990.113758")
    (publication "Incorporating Static Analysis in a Combinator-Based Compiler"
                 "Margaret Montenyohl and Mitchell Wand"
                 "Information and Computation"
                 1989
                 "http://dx.doi.org/10.1016/0890-5401(89)90052-7")
    (publication "On the Complexity of Type Inference with Coercion"
                 "Mitchell Wand and Patrick O'Keefe"
                 "FPCA"
                 1989
                 "http://doi.acm.org/10.1145/99370.99394")
    (publication "Type Inference for Record Concatenation and Multiple Inheritance"
                 "Mitchell Wand"
                 LICS
                 1989
                 "http://dx.doi.org/10.1109/LICS.1989.39162")
    (publication "The Mystery of the Tower Revealed: A Nonreflective Description of the Reflective Tower"
                 "Mitchell Wand and Daniel P. Friedman"
                 "Lisp and Symbolic Computation"
                 1988
                 "https://cs.au.dk/%7Ehosc/local/LaSC-1-1-pp11-37.pdf")
    (publication "Abstract Continuations: A Mathematical Semantics for Handling Full Jumps"
                 "Matthias Felleisen, Mitchell Wand, Daniel P. Friedman, and Bruce F. Duba"
                 LFP
                 1988
                 "http://doi.acm.org/10.1145/62678.62684")
    (publication "Corrigendum: Complete Type Inference for Simple Objects"
                 "Mitchell Wand"
                 LICS
                 1988
                 "http://dx.doi.org/10.1109/LICS.1988.5111")
    (publication "Correct Flow Analysis in Continuation Semantics"
                 "Margaret Montenyohl and Mitchell Wand"
                 POPL
                 1988
                 "http://doi.acm.org/10.1145/73560.73578")
    (publication "Linear Future Semantics and Its Implementation"
                 "Stefan K and Mitchell Wand"
                 SOCP
                 1987
                 "http://dx.doi.org/10.1016/0167-6423(87)90005-0")
    (publication "Complete Type Inference for Simple Objects"
                 "Mitchell Wand"
                 LICS
                 1987
                 "http://www.ccs.neu.edu/home/wand/papers/wand-lics-87.pdf")
    (publication "Macro-by-Example: Deriving Syntactic Transformations from their Specifications"
                 "Eugene E. Kohlbecker and Mitchell Wand"
                 POPL
                 1987
                 "http://doi.acm.org/10.1145/41625.41632")
    (publication "Obtaining Coroutines with Continuations"
                 "Christopher T. Haynes, Daniel P. Friedman, and Mitchell Wand"
                 "Computer Languages"
                 1986
                 "http://dx.doi.org/10.1016/0096-0551(86)90007-X")
    (publication "The Mystery of the Tower Revealed: A Non-Reflective Description of the Reflective Tower"
                 "Mitchell Wand and Daniel P. Friedman"
                 LFP
                 1986
                 "https://www.cs.indiana.edu/ftp/techreports/TR196.pdf")
    (publication "Finding the Source of Type Errors"
                 "Mitchell Wand"
                 POPL
                 1986
                 "http://doi.acm.org/10.1145/512644.512648")
    (publication "Continuation Semantics in Typed Lambda-Calculi (Summary)"
                 "Albert R. Meyer and Mitchell Wand"
                 "Logic of Programs"
                 1985
                 "http://dx.doi.org/10.1007/3-540-15648-8_17")
    (publication "From interpreter to compiler: a representational derivation"
                 "Mitchell Wand"
                 "Programs as Data Objects"
                 1985
                 "http://dx.doi.org/10.1007/3-540-16446-4_17")
    (publication "Embedding Type Structure in Semantics"
                 "Mitchell Wand"
                 POPL
                 1985
                 "http://doi.acm.org/10.1145/318593.318602")))

@(define will-pubs
   (list
    (publication "Bounded-latency regional garbage collection"
                 "Felix S. Klock II and William D. Clinger"
                 DLS
                 2011
                 "http://doi.acm.org/10.1145/2047849.2047859")
    (publication "Linear combinations of radioactive decay models for generational garbage collection"
                 "William D. Clinger and Fabio V. Rojas"
                 SOCP
                 2006
                 "http://dx.doi.org/10.1016/j.scico.2006.02.005")
    (publication "An experimental study of renewal-older-first garbage collection"
                 "Lars Thomas Hansen and William D. Clinger"
                 ICFP
                 2002
                 "http://doi.acm.org/10.1145/581478.581502")
    (publication "Concurrent Remembered Set Refinement in Generational Garbage Collection"
                 "David Detlefs, Ross Knippel, William D. Clinger, and Matthias Jacob"
                 "Java Virtual Machine Research and Technology Symposium"
                 2002
                 "http://www.usenix.org/publications/library/proceedings/javavm02/detlefs.html")
    (publication "Implementation Strategies for First-Class Continuations"
                 "William D. Clinger, Anne Hartheimer, and Eric Ost"
                 HOSC
                 1999
                 "http://dx.doi.org/10.1023/A:1010016816429")
    (publication "Proper Tail Recursion and Space Efficiency"
                 "William D. Clinger"
                 PLDI
                 1998
                 "http://doi.acm.org/10.1145/277650.277719")
    (publication "Generational Garbage Collection and the Radioactive Decay Model"
                 "William D. Clinger and Lars Thomas Hansen"
                 PLDI
                 1997
                 "http://doi.acm.org/10.1145/258915.258925")
    (publication "Lambda, the Ultimate Label or a Simple Optimizing Compiler for Scheme"
                 "William D. Clinger and Lars Thomas Hansen"
                 LFP
                 1994
                 "http://doi.acm.org/10.1145/182409.156786")
    (publication "Parallel Destructive Updating in Strict Functional Languages"
                 "A. V. S. Sastry and William D. Clinger"
                 LFP
                 1994
                 "http://doi.acm.org/10.1145/182409.182486")
    (publication "Order-of-evaluation Analysis for Destructive Updates in Strict Functional Languages with Flat Aggregates"
                 "A. V. S. Sastry, William D. Clinger, and Zena M. Ariola"
                 "FPCA"
                 1993
                 "http://doi.acm.org/10.1145/165180.165222")
    (publication "Macros That Work"
                 "William D. Clinger and Jonathan Rees"
                 POPL
                 1991
                 "http://doi.acm.org/10.1145/99583.99607")
    (publication "How to Read Floating-Point Numbers Accurately"
                 "William D. Clinger"
                 PLDI
                 1990
                 "http://doi.acm.org/10.1145/93542.93557")
    (publication "How to read floating point numbers accurately (with retrospective)"
                 "William D. Clinger"
                 "Best of PLDI"
                 1990
                 "http://doi.acm.org/10.1145/989393.989430")
    (publication "Implementation Strategies for Continuations"
                 "William D. Clinger, Anne Hartheimer, and Eric Ost"
                 LFP
                 1988
                 "http://doi.acm.org/10.1145/62678.62692")
    (publication "Alternative Semantics for McCarthy's amb"
                 "William D. Clinger and Charles Halpern"
                 "Seminar on Concurrency"
                 1984
                 "http://dx.doi.org/10.1007/3-540-15670-4_22")
    (publication "The Scheme 311 Compiler: An Exercise in Denotational Semantics"
                 "William D. Clinger"
                 LFP
                 1984
                 "http://dl.acm.org/citation.cfm?id=802052")
    (publication "Nondeterministic Call by Need is Neither Lazy Nor by Name"
                 "William D. Clinger"
                 "Symposium on LISP and Functional Programming"
                 1982
                 "http://dl.acm.org/citation.cfm?id=802154")))

@(define publications
   (flatten (list mf-pubs
                  old-site-pubs
                  amal-pubs
                  jan-pubs
                  mitch-pubs
                  olin-pubs
                  will-pubs)))

@(define (publication->html pub)
   (match-define (publication title authors venue year link) pub)
   @div[class: "pn-publication"]{
     @div[class: "pn-main-informations"]{
       @span[class: "pn-pub-title" title]
       @(when link
          @list{
            @span[class: "pn-pub-link"]{
              [@a[class: "pn-url" href: link link]]}})
       @br{}
       @span[class: "pn-authors" authors]
       @br{}
       @span[class: "pn-venue" venue]}})

@(define-struct publication-group (publications year) #:prefab)

@; (listof publications) -> (listof publication-group)
@(define (arrange-publications pubs)
   (define unsorted-groups
     (for/list ([pubs (in-list (group-by publication-year pubs))])
       (publication-group (sort pubs string<? #:key publication-venue)
                          (publication-year (first pubs)))))
   (sort unsorted-groups > #:key publication-group-year))

@(define (publication-group->html pub-group)
   @div[class: "pn-pub-group col-md-12 compact"]{
     @span[class: "pn-pub-year " (publication-group-year pub-group)]
     @br{}
     @(map publication->html (publication-group-publications pub-group))})

@doctype{html}
@html[lang: "en"]{
  @header{Publications}
  @body[id: "pn-top"]{
   @navbar{Publications}
   @subpage-title{Publications}

   @div[class: "pn-main-wrapper"]{
     @div[class: "row"]{
       @(map publication-group->html
             (arrange-publications publications))}}
   @footer{}
}}
