#lang scribble/html
@(require racket/match
          "templates.rkt")

@; string string string number (maybe string) -> elem?
@(define-struct publication (title authors venue year link) #:prefab)

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
    (publication "Pushdown Flow Analysis of First-Class Control"
                 "Dimitrios Vardoulakis and Olin Shivers"
                 ICFP
                 2011
                 "http://www.ccs.neu.edu/home/dimvar/papers/cfa2-1st-class.pdf")
    (publication "CFA2: a Context-Free Approach to Control-Flow Analysis"
                 "Dimitrios Vardoulakis and Olin Shivers"
                 "Logical Methods in Computer Science (LMCS)"
                 2011
                 "http://www.lmcs-online.org/ojs/viewarticle.php?id=705")
    (publication "Ordering Multiple Continuations on the Stack"
                 "Dimitrios Vardoulakis and Olin Shivers"
                 "Partial Evaluation and Program Manipulation (PEPM)"
                 2011
                 "http://www.ccs.neu.edu/home/dimvar/papers/rcps-NU-CCIS-11-01.pdf")
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
    (publication "CFA2: a Context-Free Approach to Control-Flow Analysis"
                 "Dimitrios Vardoulakis and Olin Shivers"
                 ESOP
                 2010
                 "http://www.ccs.neu.edu/home/dimvar/papers/cfa2-NU-CCIS-10-01.pdf")
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
    (publication "A Compositional Trace Semantics for Orc"
                 "Dimitrios Vardoulakis and Mitchell Wand"
                 "International Conference on Coordination Models and Languages (COORDINATION)"
                 2008
                 "http://www.ccs.neu.edu/home/dimvar/papers/orc-coord.pdf")
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
                  "Journal of Higher-Order and Symbolic Computing"
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

@(define publications
   (flatten (list mf-pubs
                  amal-pubs
                  old-site-pubs)))

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
   ;; most recent first
   (define grouped
     (group-by publication-year pubs)
     #;(reverse (sort pubs #:key publication-year)))
   (for/list ([pubs (in-list grouped)])
     (publication-group (sort pubs string<? #:key publication-venue)
                        (publication-year (first pubs)))))

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
