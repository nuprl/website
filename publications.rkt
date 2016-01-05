#lang scribble/text
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

@(define publications
   (list
    (publication "Typing the numeric tower"
                 "Vincent St-Amour, Sam Tobin-Hochstadt, Matthew Flatt and Matthias Felleisen"
                 "Practical Aspects of Declarative Languages (PADL)"
                 2012
                 "http://www.ccs.neu.edu/home/stamourv/papers/numeric-tower.pdf")
    (publication "Seeing the futures: profiling shared-memory parallel Racket"
                 "James Swaine, Burke Fetscher, Vincent St-Amour, Robby Findler and Matthew Flatt"
                 "Functional High-Performance Computing (FHPC)"
                 2012
                 "http://www.ccs.neu.edu/home/stamourv/papers/seeing-the-futures.pdf")
    (publication "Optimization coaching"
                 "Vincent St-Amour, Sam Tobin-Hochstadt and Matthias Felleisen"
                 OOPSLA
                 2012
                 "http://www.ccs.neu.edu/home/stamourv/papers/optimization-coaching.pdf")
    (publication "Practical Programming with Substructural Types"
                 "Jesse A. Tov"
                 "PhD Dissertation, Northeastern University"
                 2012
                 "http://users.eecs.northwestern.edu/~jesse/pubs/dissertation/")
    (publication "Languages as libraries"
                 "Sam Tobin-Hochstadt, Vincent St-Amour, Ryan Culpepper, Matthew Flatt and Matthias Felleisen"
                 PLDI
                 2011
                 "http://www.ccs.neu.edu/home/stamourv/papers/lang-lib.pdf")
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
                 "Trends in Functional Programming"
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
                 "European Conference on Object-Oriented Programming (ECOOP)"
                 2007
                 "http://www.ccs.neu.edu/home/turon/ecoop07-meta-traits.pdf")
    (publication "ACL2 for Freshmen: First Experiences"
                 "Carl Eastlund, Dale Vaillancourt and Matthias Felleisen"
                 "ACL2 Workshop"
                 2007
                 "http://www.ccs.neu.edu/racket/pubs/acl207-evf.pdf")
    (publication "Debugging Macros"
                 "Ryan Culpepper and Matthias Felleisen"
                 "GPCE"
                 2007
                 "http://www.ccs.neu.edu/racket/pubs/gpce07-cf.pdf")
    (publication "Advanced Macrology and the Implementation of Typed Scheme"
                 "Ryan Culpepper, Sam Tobin-Hochstadt and Matthew Flatt"
                 "Scheme Workshop"
                 2007
                 "http://www.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf")
    (publication "Adding Delimited and Composable Control to a Production Programming Environment"
                 "Matthew Flatt, Gang Yu, Robert Bruce Findler and Matthias Felleisen"
                 ICFP
                 2007
                 "http://www.ccs.neu.edu/scheme/pubs/icfp07-fyff.pdf")
    (publication "Implementation and Use of the PLT Scheme Web Server"
                 "Shriram Krishnamurthi, Peter Walton Hopkins, Jay McCarthy, Paul Graunke, Greg Pettyjohn and Matthias Felleisen"
                 "Journal of Higher-Order and Symbolic Computing"
                 2007
                 "http://www.ccs.neu.edu/scheme/pubs/hosc07-sk-mf.pdf")
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
                 "Trends in Functional Programming"
                 2007
                 "http://www.ccs.neu.edu/home/dherman/research/papers/tfp07-gradual-typing.pdf")))

@(define (publication->html pub)
   (match-define (publication title authors venue year link) pub)
   @list{
<div class="pn-publication">
  <div class="pn-main-informations">
    <span class="pn-pub-title">@|title|</span>
    @(when link
       @list{
         <span class="pn-pub-link">
           [<a class="pn-url" href="@|link|">link</a>]
         </span>})
    <br />
    <span class="pn-authors">@|authors|</span>
    <br />
    <span class="pn-venue">@|venue|</span>
  </div>
</div>
})

@(define-struct publication-group (publications year) #:prefab)

@; (listof publications) -> (listof publication-group)
@(define (arrange-publications pubs)
   ;; most recent first
   (define grouped
     (group-by publication-year pubs)
     #;(reverse (sort pubs #:key publication-year)))
   (for/list ([pubs (in-list grouped)])
     (publication-group pubs (publication-year (first pubs)))))

@(define (publication-group->html pub-group)
   @list{
<div class="pn-pub-group col-md-12 compact">
  <span class="pn-pub-year ">@(publication-group-year pub-group)</span>
  <br />
  @(map publication->html (publication-group-publications pub-group))
</div>
})

<!DOCTYPE html>
<html lang="en">
  @header{Publications}
  <body id="pn-top">
   @navbar{Publications}
   @subpage-title{Publications}

    <div class="pn-main-wrapper">
      <div class="row">
        @(map publication-group->html
              (arrange-publications publications))
      </div>
    </div>
    @footer
  </body>
</html>