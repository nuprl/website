#lang scribble/text
@(require "templates.rkt"
          (only-in racket/match match-define)
          scribble/html/html)
@(define (string-empty? s) (string=? "" s))
@(struct alum (name year psite dsite extra))
@(define (alumnus name #:year year #:personal-site [psite #f] #:dissertation [dsite #f] #:extra [extra #f])
  (alum name year psite dsite extra))
@(define (alumnus->string a)
   (match-define (alum name year psite dsite extra) a)
   (define (link url body)
     (string-append "<a href=\"" url "\">" body "</a>"))
   (define name-s
     (if psite
         (link psite name)
         name))
   (define year-s (number->string year))
   (define dsite-link
     (if dsite
         (string-append " " (link dsite "(dissertation)"))
         ""))
   (define extra-s
     (or extra ""))
   (define (cat-space xs)
     (let loop ([acc ""]
                [xs  xs])
       (cond [(empty? xs) acc]
             [else
              (define x (car xs))
              (define xs2 (cdr xs))
              (define fill
                (if (or (string-empty? acc) (string-empty? x))
                    ""
                    " "))
              (loop (string-append acc fill x) xs2)])))
   (string-append "<li>"
                  (cat-space (list name-s
                                   year-s
                                   dsite-link
                                   extra-s))
                  "</li>"))
@(define (alumnus<? a1 a2)
  (if (= (alum-year a1) (alum-year a2))
    (string<? (alum-name a1) (alum-name a2))
    (< (alum-year a1) (alum-year a2))))
@(define (alumnus*->ul a*)
  (string-append "<div class=\"col-md-4\"><ul>" (apply string-append (map alumnus->string a*)) "</ul></div>"))
@(define (alumnus-list . a-unsorted*)
  (define a* (sort a-unsorted* alumnus<?))
  (define num-groups 3)
  (define group-size (round (+ 1 (/ (length a*) 3))))
  (define a-str* (let group-alum* ([a* a*])
                   (if (>= group-size (length a*))
                     (list (alumnus*->ul a*))
                     (let-values ([(hd tl) (split-at a* group-size)])
                       (cons (alumnus*->ul hd)
                             (group-alum* tl))))))
  (string-append "<div>" (apply string-append a-str*) "</div>"))

<!DOCTYPE html>
<html lang="en">
  @header{People}
  <body id="pn-top" class="subpages">
    @navbar{People}
    @subpage-title{Staff}

    <div class="pn-main-wrapper">
      <div class="content">
        <div class="container">

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/mitch_wand.jpg" title="Mitch Wand" alt="Mitch Wand" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Mitch Wand</span><br />
                  Professor<br />
                  <a href="mailto:wand@"@"ccs.neu.edu">wand@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/home/wand">www.ccs.neu.edu/home/wand</a>
                </div>
                <div class="col-md-5 pn-muted col-md-offset-3">
                    Joined Northeastern, 1985<br />
                    Joined Indiana University, 1973<br />
                    PhD, MIT, 1973<br />
                    SB, MIT, 1969
                </div>
                <div class="col-md-12 pn-bio">
                  <p>Over the years, I have worked on a variety of problems associated with semantics of programming languages. Here is a selected list, in roughly reverse chronological order: probabilistic programming languages, binding-safe programming, aspect-oriented programming, analysis-based program transformation, compiler correctness proofs, continuations, macros.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/Clinger.jpg" title="Will Clinger" alt="Will Clinger" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">William D. Clinger</span><br />
                  Professor<br />
                  <a href="mailto:will@"@"ccs.neu.edu">will@"@"ccs.neu.edu</a><br />
                  <a href="http://www.cesura17.net/~will/Professional/">www.cesura17.net/~will/Professional/</a>
                </div>
                <div class="col-md-5 pn-muted col-md-offset-3">
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I focus on the design, specification, and implementation
		  of functional and higher-order programming languages. My
		  research interests stem from programming languages’
		  ability to connect mathematically sophisticated theories
		  of syntax and semantics to economically important details
		  of computer hardware and software.  In recent years, I
		  have designed algorithms for garbage collection for
		  Larceny -- a vehicle for experimental research on compiler
		  optimization and garbage collection. Larceny has become
		  one of the leading multiplatform implementations of the
		  Scheme programming language.
</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/matthias_felleisen.jpg" title="Matthias Felleisen" alt="Matthias Felleisen" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Matthias Felleisen</span><br />
                  Trustee Professor<br />
                  <a href="mailto:matthias@"@"ccs.neu.edu">matthias@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/home/matthias/">www.ccs.neu.edu/home/matthias</a>
                </div>
                <div class="col-md-5 pn-muted col-md-offset-3">
                    Joined Northeastern, 2001<br />
                    Joined Rice, 1987<br />
                    PhD, Indiana University, 1987<br />
                    Diplom TH Karlsruhe, 1984<br />
                    MS University of Arizona, 1981
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I explore all aspects of program design and programming language design.  My current research involves work on behavioral software contracts, gradual typing of scripting languages, language interoperability, language extensibility, and module systems. I also engage in educational outreach work. For the past 20 years, I have worked with middle schools, high schools, after-school programs, and college faculty on injecting design ideas into mathematics and computer science courses. Such educational interactions often inspire our research, and many research efforts end up improving my educational work.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/olin_shivers.jpg" title="Olin Shivers" alt="Olin Shivers" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Olin Shivers</span><br />
                  Professor<br />
                  <a href="mailto:shivers@"@"ccs.neu.edu">shivers@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/home/shivers/">www.ccs.neu.edu/home/shivers</a>
                </div>
                <div class="col-md-5 pn-muted col-md-offset-3">
                  Joined Northeastern, 2006<br />
                  Joined Georgia Tech, 1999<br />
                  Joined MIT, 1993<br />
                  PhD, Carnegie Mellon University, 1991<br />
                  BS, Yale University, 1983
                </div>
                <div class="col-md-12 pn-bio">
                  <p>My principal research interests include the construction of robust, complex software artifacts and the design of tools that assist programmers in this task; the interaction between systems and programming languages, primarily higher-order typed languages; the design and analysis of programming languages; and compilers. Before coming to Northeastern, I was a research scientist at MIT’s Artificial Intelligence Lab, a founder and CTO of the Smartleaf Corporation, and a faculty member at the Georgia Institute of Technology.</p>
                </div>
              </div>
            </div>
          </div>


          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/amal.jpg" title="Amal Ahmed" alt="Amal Ahmed" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Amal Ahmed</span><br />
                  Professor<br />
                  <a href="mailto:amal@"@"ccs.neu.edu">amal@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/home/amal/">www.ccs.neu.edu/home/amal/</a>
                </div>
                <div class="col-md-5 pn-muted col-md-offset-3">
Joined Northeastern, 2011 <br />
Joined Indiana University, 2009<br />
Joined Toyota Technological Institute, 2006<br />
PhD Princeton University, 2004<br />
MS Stanford University 1995<br />
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I work on problems involving semantics of programming languages, including advanced type systems for programs that manipulate memory, correct and secure compilation, gradual typing, and language interoperability.  My prior work has shown how to scale the logical relations proof method to realistic languages.  This technique has been used in numerous contexts, e.g., to prove compiler correctness, to verify concurrent code, to establish guarantees provided by type systems for confidentiality or differential privacy.  My present focus is on how to build verified compilers that ensure safe linking of code compiled from different programming languages. </p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/ryan_culpepper.jpg" title="Ryan Culpepper" alt="Ryan Culpepper" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Ryan Culpepper</span><br />
                  Post-doctoral researcher<br />
                  <a href="mailto:ryanc@"@"ccs.neu.edu">ryanc@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/home/ryanc/">http://www.ccs.neu.edu/home/ryanc/</a>
                </div>
                <div class="col-md-5 pn-muted col-md-offset-3">
                    Joined Northeastern, 2013<br />
                    PhD, Northeastern University, 2010<br />
                </div>
                <div class="col-md-12 pn-bio">
                  <p>Born in Houston, TX, left, went back to go to Rice University. Interested in PL and compilers. I read during the summer and play table tennis during the winter. I'm still looking for someone up here who has heard of disc golf.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/stephen_chang.jpg" title="Stephen Chang" alt="Stephen Chang" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Stephen Chang</span><br />
                  Post-doctoral researcher<br />
                  <a href="mailto:stchang@"@"ccs.neu.edu">stchang@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/home/stchang/">www.ccs.neu.edu/home/stchang</a>
                </div>
                <div class="col-md-5 pn-muted col-md-offset-3">
                    Joined Northeastern, 2014<br />
                    PhD, Northeastern University, 2014<br />
                    MS, Harvard University, 2008<br />
                    BSE, Princeton University, 2001
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I'm interested in the design of practical programming languages.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/ben_lerner.jpg" title="Ben Lerner" alt="Ben Lerner" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Ben Lerner</span><br />
                  Lecturer<br />
                  <a href="mailto:blerner@"@"ccs.neu.edu">blerner@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/home/blerner">www.ccs.neu.edu/home/blerner</a>
                </div>
                <div class="col-md-5 pn-muted col-md-offset-3">
                    Joined Northeastern, 2014<br />
                    PhD, University of Washington, 2011
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I have worked on problems in web programming semantics, including designing and analyzing extensibility mechanisms for browsers, studying the interactions between extensions and each other or with intended browser behavior.  With colleagues at Brown, I have been helping to design and implement a language that focuses on the linguistic support needed for introductory-level pedagogy.</p>
                </div>
              </div>
            </div>
          </div>

          @(person #:name "Jan Vitek"
                   #:title "Professor"
                   #:e-mail "j.vitek@neu.edu"
                   #:website "http://janvitek.org"
                   #:history "Joined Northeastern, 2014<br />
                    Joined Purdue, 1999<br />
                    PhD, University of Geneva, 1999<br />
                    MSc, University of Victoria, 1995"
                   #:bio "I work on the design and implementation of programming languages. I led the implementation of the first real-time Java virtual machine to be flight-tested. With Noble and Potter, I proposed what became known as Ownership Types.  I tried to understand JavaScript by dynamic analysis and am now looking at supporting scalable data analysis in R."
                   #:img "jan_vitek.jpg")

          @(person #:name "Paley Li"
                   #:title "Post-doctoral researcher"
                   #:e-mail "pa.li@neu.edu"
                   #:website "https://palez.github.io"
                   #:history "Joined Northeastern, 2015<br />
                    PhD, Victoria University of Wellington, 2015"
                   #:bio "My research interests is in programming languages, specifically type theory, ownership types, and local reasoning for memory management. Most recently, I have been working on formalising object cloning for ownership types."
                   #:img "paleyli.jpg")

          @(person #:name "Gabriel Scherer"
                   #:title "Post-doctoral researcher"
                   #:e-mail "gabriel.scherer@gmail.com"
                   #:website "http://www.ccs.neu.edu/home/gasche/index.html"
                   #:history "Joined Northeastern, 2016"
                   #:img "gabriel_scherer.jpg"
                   #:bio "I'm interested in theoretical aspects of type systems, programming language implementation, general programming language concepts, and even some syntactic aspects. I have a preference for the formalizable aspects, or formalizable approaches to programming language aspects, rather than the often subjective appeal to taste or intuition."
)

        </div>

        <div class="pn-separator-img">
           <h2>Students</h2>
        </div>

        <div class="container">

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/paul_stansifer.jpg" title="Paul Stansifer" alt="Paul Stansifer" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Paul Stansifer</span><br />
                  Advisor: Mitch Wand<br />
                  <a href="mailto:pauls@"@"ccs.neu.edu">pauls@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/~pauls">www.ccs.neu.edu/~pauls</a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                   Joined 2009
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I am working with Mitch on macros that can extend the underlying syntax of their language. I like cookies. And burritos.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/tony_jones.jpg" title="Tony Garnock-Jones" alt="Tony Garnock-Jones" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Tony Garnock-Jones</span><br />
                  Advisor: Matthias Felleisen<br />
                  <a href="mailto:tonyg@"@"ccs.neu.edu">tonyg@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/home/tonyg/">www.ccs.neu.edu/home/tonyg</a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                    Joined 2010
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I’m working on constructing programming languages that incorporate ideas from networking and from messaging middleware.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/asumu_takikawa.jpg" title="Asumu Takikawa" alt="Asumu Takikawa" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Asumu Takikawa</span><br />
                  Advisor: Matthias Felleisen<br />
                  <a href="mailto:asumu@"@"ccs.neu.edu">asumu@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/home/asumu/">www.ccs.neu.edu/home/asumu</a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                    Joined 2010
                </div>
                <div class="col-md-12 pn-bio">
                  <p>Racket developer, Oregonian, and PhD student at Northeastern University. Works on Typed Racket.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/jonathan_schuster.jpg" title="Jonathan Schuster" alt="Jonathan Schuster" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Jonathan Schuster</span><br />
                  Advisor: Olin Shivers and Matthias Felleisen<br />
                  <a href="mailto:schuster@"@"ccs.neu.edu">schuster@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/home/schuster/">www.ccs.neu.edu/home/schuster</a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                    Joined 2011
                </div>
                <div class="col-md-12 pn-bio">
                  <p>Having come from a development background, my research interests lie in making software development easier by improving programming languages and the ecosystems surrounding them. Currently, my research focuses on verifying actor-based programs (such as those written in Erlang or the Akka framework) against behavioral specifications expressed as simple name-passing automata.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/justin_slepak.jpg" title="Justin Slepak" alt="Justin Slepak" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Justin Slepak</span><br />
                  Advisor: Olin Shivers<br />
                  <a href="mailto:jrslepak@"@"ccs.neu.edu">jrslepak@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/home/jrslepak/">www.ccs.neu.edu/home/jrslepak</a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                    Joined 2011
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I came to Northeastern after spending several years studying in Upper Michigan. My current work focuses on array-oriented languages as an expressive way to write data-parallel numeric processing code.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/william_bowman.jpg" title="William J. Bowman" alt="William J. Bowman" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">William J. Bowman</span><br />
                  Advisor: Amal Ahmed<br />
                  <a href="mailto:wilbowma@"@"ccs.neu.edu">wilbowma@"@"ccs.neu.edu</a><br />
                  <a href="https://www.williamjbowman.com/">www.williamjbowman.com</a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                    Joined 2012
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I want to make programs easier to design, read, and write. I currently work on verifying compilers for advanced languages, and sometimes dabble in meta-programming.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/leif_andersen.jpg" title="Leif Andersen" alt="Leif Andersen" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Leif Andersen</span><br />
                  Advisor: Matthias Felleisen<br />
                  <a href="mailto:leif@"@"ccs.neu.edu">leif@"@"ccs.neu.edu</a><br />
                  <a href="http://leif.pl">leif.pl</a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                    Joined 2014
                </div>
                <div class="col-md-12 pn-bio">
                  <p>Objects in the Rear View Mirror May Appear Closer than They Are.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/Andrew-square.jpg" title="Andrew Cobb" alt="Andrew Cobb" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Andrew Cobb</span><br />
                  Advisor: Olin Shivers<br />
                  <a href="mailto:andrew.cobb@"@"gmail.com">andrew.cobb@"@"gmail.com</a><br />
                  <a href="http://"></a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                    Joined 2014
                </div>
                <div class="col-md-12 pn-bio">
                  <p></p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/ben_greenman.jpg" title="Ben Greenman" alt="Ben Greenman" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Ben Greenman</span><br />
                  Advisor: Matthias Felleisen<br />
                  <a href="mailto:types@"@"ccs.neu.edu">types@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/home/types">www.ccs.neu.edu/home/types</a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                    Joined 2014
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I like constructions. My current goal is to lower the huge run-time cost of gradual typing. I believe that safely mixing languages is the way of the future.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/max_new.jpg" title="Max New" alt="Max New" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Max New</span><br />
                  Advisor: Amal Ahmed<br />
                  <a href="mailto:maxnew@"@"ccs.neu.edu">maxnew@"@"ccs.neu.edu</a><br />
                  <a href="http://maxsnew.github.io/">maxsnew.github.io</a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                    Joined 2014
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I like reasonable programming languages.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/sam_caldwell.jpg" title="Sam Caldwell" alt="Sam Caldwell" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Sam Caldwell</span><br />
                  Advisor: Matthias Felleisen<br />
                  <a href="mailto:samc@"@"ccs.neu.edu">samc@"@"ccs.neu.edu</a><br />
                  <a href="http://www.ccs.neu.edu/home/samc">www.ccs.neu.edu/home/samc</a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                    Joined 2015
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I came to Northeastern from Austin, Texas, where I did my undergrad and spent several years working in embedded software. I’m interested in using ideas and tools from programming languages to make the world a better place for everyone that uses computers.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/benjamin_chung.jpg" title="Benjamin Chung" alt="Benjamin Chung" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Benjamin Chung</span><br />
                  Advisor: Jan Vitek<br />
                  <a href="mailto:bchung@"@"ccs.neu.edu">bchung@"@"ccs.neu.edu</a><br />
                  <a href="http://benchung.github.io">benchung.github.io</a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                    Joined 2015
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I usually work on types, currently focusing on gradual type systems.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/olivier_fluckiger.jpg" title="Olivier Flückiger" alt="Olivier Flückiger" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Olivier Flückiger</span><br />
                  Advisor: Jan Vitek<br />
                  <a href="mailto:o@"@"o1o.ch">o@"@"o1o.ch</a><br />
                  <a href="http://www.o1o.ch/about">www.o1o.ch/about</a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                    Joined 2015
                </div>
                <div class="col-md-12 pn-bio">
                  <p>My passion lies in language implementation &ndash; creating the tools and techniques required to get from powerful abstractions to efficient execution.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="row pn-person">
            <div class="col-md-12 pn-row-eq-height">
              <div class="col-md-3 pn-photo">
                <div class="img-wrapper">
                  <img src="img/daniel_patterson.jpg" title="Daniel Patterson" alt="Daniel Patterson" />
                </div>
              </div>
              <div class="col-md-9">
                <div class="col-md-4 pn-contact">
                  <span class="pn-name">Daniel Patterson</span><br />
                  Advisor: Amal Ahmed<br />
                  <a href="mailto:dbp@"@"ccs.neu.edu">dbp@"@"ccs.neu.edu</a><br />
                  <a href="https://dbp.io">dbp.io</a>
                </div>
                <div class="col-md-3 pn-muted col-md-offset-5">
                    Joined 2016
                </div>
                <div class="col-md-12 pn-bio">
                  <p>I'm interested in language and type system interoperability - in particular, languages with very different type systems interacting easily and safely.</p>
                </div>
              </div>
            </div>
          </div>


        </div>

        <div class="pn-separator-img">
           <h2>PRL Alumni and Former Members</h2>
        </div>
        <div class="container"> 
          <div class="row">
            @(alumnus-list
                @(alumnus "Dino Oliva" #:year 1992
                          #:personal-site "http://cm.bell-labs.com/cm/cs/who/oliva/"
                          #:dissertation "ftp://www.ccs.neu.edu/pub/people/wand/papers/oliva-thesis-94.ps.Z")
                @(alumnus "Greg Sullivan" #:year 1997
                          #:personal-site "http://www.ai.mit.edu/%7Egregs"
                          #:dissertation "ftp://www.ccs.neu.edu/pub/people/wand/papers/sullivan-thesis-97.ps"
                          #:extra "Advisor: Mitch Wand, Employer: BAE Systems")
                @(alumnus "David Gladstein" #:year 1996
                          #:dissertation "ftp://www.ccs.neu.edu/pub/people/wand/papers/gladstein-thesis-94.ps.Z")
                @(alumnus "Paul Steckler" #:year 1994
                          #:personal-site "http://www.ccs.neu.edu/home/steck/"
                          #:dissertation "ftp://www.ccs.neu.edu/pub/people/wand/papers/steckler-thesis-94.ps"
                          #:extra "(Galois Connections)")
                @(alumnus "Igor Siveroni" #:year 2001
                          #:personal-site "http://www.soi.city.ac.uk/%7Esbbc287/"
                          #:dissertation "http://www.siveroni.com/imperial/papers/thesis.ps")
                @(alumnus "Lars Hansen" #:year 2001
                          #:personal-site "http://www.ccs.neu.edu/home/lth/"
                          #:dissertation "http://www.ccs.neu.edu/home/lth/thesis/index.html")
                @(alumnus "Paul Graunke" #:year 2003
                          #:dissertation "http://www.ccs.neu.edu/scheme/pubs/thesis-graunke.pdf"
                          #:extra "(Galois Connections)")
                @(alumnus "Galen Williamson" #:year 2004
                          #:personal-site "http://www.ccs.neu.edu/home/gwilliam")
                @(alumnus "Johan Ovlinger" #:year 2004
                          #:personal-site "http://www.ccs.neu.edu/home/johan"
                          #:dissertation "http://www.ccs.neu.edu/research/demeter/theses/ovlinger/thesis.pdf")
                @(alumnus "John Brinckerhoff Clements" #:year 2005
                          #:personal-site "http://www.csc.calpoly.edu/%7Eclements/index.html"
                          #:dissertation "http://www.ccs.neu.edu/scheme/pubs/dissertation-clements.pdf"
                          #:extra "(California Polytechnic, San Luis Obispo, CA)")
                @(alumnus "Richard Cobbe" #:year 2009
                          #:personal-site "http://www.ccs.neu.edu/home/cobbe/"
                          #:dissertation "http://www.ccs.neu.edu/scheme/pubs/dissertation-cobbe.pdf"
                          #:extra "(MathWorks)")
                @(alumnus "Theo Skotiniotis" #:year 2010
                          #:personal-site "http://www.ccs.neu.edu/home/skotthe/"
                          #:dissertation "http://www.ccs.neu.edu/home/lieber/theses/skotiniotis/skotiniotis-dissertation.pdf")
                @(alumnus "Bryan D. Chadwick" #:year 2010
                          #:personal-site "http://bryanchadwick.com/"
                          #:dissertation "http://www.ccs.neu.edu/home/chadwick/files/thesis-single.pdf"
                          #:extra "(Broadway Technology)")
                @(alumnus "Dave Herman" #:year 2010
                          #:personal-site "http://calculist.org/"
                          #:dissertation "http://www.ccs.neu.edu/home/dherman/research/papers/dissertation.pdf"
                          #:extra "(Mozilla Research)")
                @(alumnus "Ryan Culpepper" #:year 2010
                          #:personal-site "http://www.ccs.neu.edu/home/ryanc/"
                          #:dissertation "http://www.ccs.neu.edu/scheme/pubs/dissertation-culpepper.pdf"
                          #:extra "(Northeastern University)")
                @(alumnus "Peter Dillinger" #:year 2010
                          #:personal-site "http://www.peterd.org/"
                          #:dissertation "http://www.peterd.org/pcd-diss.pdf"
                          #:extra "(Coverity, Inc.)")
                @(alumnus "Pengcheng Wu" #:year 2010
                          #:dissertation "http://www.ccs.neu.edu/home/lieber/theses/wu/Dissertation.pdf")
                @(alumnus "Sam Tobin-Hochstadt" #:year 2010
                          #:personal-site "http://www.ccs.neu.edu/home/samth/"
                          #:dissertation "http://www.ccs.neu.edu/racket/pubs/dissertation-tobin-hochstadt.pdf"
                          #:extra "Advisor: Matthias, now at Indiana University")
                @(alumnus "Felix Klock" #:year 2011
                          #:personal-site "http://www.ccs.neu.edu/home/pnkfelix/"
                          #:dissertation "http://www.ccs.neu.edu/home/pnkfelix/thesis/klock11-diss.pdf"
                          #:extra "(Mozilla Research)")
                @(alumnus "Stevie Strickland" #:year 2012
                          #:personal-site "http://www.ccs.neu.edu/%7Esstrickl/"
                          #:dissertation "http://www.ccs.neu.edu/racket/pubs/dissertation-strickland.pdf"
                          #:extra "(Brown University)")
                @(alumnus "Dionna Amalie Glaze" #:year 2015
                          #:personal-site "https://deeglaze.github.io"
                          #:dissertation "https://deeglaze.github.io/diss.pdf"
                          #:extra "(LogicBlox)")
                @(alumnus "Vincent St-Amour" #:year 2015
                          #:personal-site "http://users.eecs.northwestern.edu/~stamourv/"
                          #:dissertation "http://users.eecs.northwestern.edu/~stamourv/papers/dissertation.pdf"
                          #:extra "Advisor: Matthias, now at Northwestern University")
                @(alumnus "Carl Eastlund" #:year 2012
                          #:dissertation "http://www.ccs.neu.edu/racket/pubs/dissertation-eastlund.pdf"
                          #:extra "(Jane Street Group, LLC)"
                          #:personal-site "http://www.ccs.neu.edu/home/cce/")
                @(alumnus "Vassilis Koutavas" #:year 2008
                          #:personal-site "http://www.scss.tcd.ie/Vasileios.Koutavas/"
                          #:extra "(Trinity College, Dublin)"
                          #:dissertation "http://www.scss.tcd.ie/Vasileios.Koutavas/publications/dissertation.pdf")
                @(alumnus "James T. Perconti" #:year 2014
                          #:personal-site "http://www.ccs.neu.edu/home/jtpercon/")
                @(alumnus "Jesse A. Tov" #:year 2012
                          #:extra "(Northwestern University)"
                          #:dissertation "http://users.eecs.northwestern.edu/~jesse/pubs/dissertation/"
                          #:personal-site "http://users.eecs.northwestern.edu/~jesse/")
                @(alumnus "Aaron J. Turon" #:year 2013
                          #:dissertation "http://www.ccs.neu.edu/home/turon/thesis.pdf"
                          #:extra "(Mozilla Research)"
                          #:personal-site "http://www.ccs.neu.edu/home/turon/")
                @(alumnus "Dimitris Vardoulakis" #:year 2012
                          #:extra "(Google)"
                          #:personal-site "http://dimvar.github.io/"
                          #:dissertation "http://dimvar.github.io/papers/diss.pdf")
                @(alumnus "Ahmed Abdelmeged" #:year 2014
                          #:personal-site "http://www.ccs.neu.edu/home/mohsen/HomePage/index.html")
                @(alumnus "Phillip Mates" #:year 2015
                          #:personal-site "http://www.ccs.neu.edu/home/mates/"
                          #:extra "(Dimagi)")
                @(alumnus "Erik Silkensen" #:year 2015
                          #:personal-site "http://www.ccs.neu.edu/home/ejs/")
                @(alumnus "Christos Dimoulas" #:year 2012
                          #:dissertation "http://www.ccs.neu.edu/racket/pubs/dissertation-dimoulas.pdf"
                          #:extra "(Harvard University)"
                          #:personal-site "http://people.seas.harvard.edu/~chrdimo/")
                @(alumnus "Philippe Meunier" #:year 2006
                          #:dissertation "http://www.ccs.neu.edu/racket/pubs/dissertation-meunier.pdf"
                          #:extra "(Sirindhorn International Institute of Technology, Tahmmasat University)")
             )
          </div>
        </div>

        <div class="container"> 
          <div class="row">
            <h3 style="text-align: center;">Former Associates (Research Scientists, Post-Docs)</h3>   
              @(alumnus-list
                @(alumnus "Kenichi Asai" #:year 2004
                          #:personal-site "http://www.is.ocha.ac.jp/~asai/"
                          #:extra "(Ochanomizu University)"
                          #:dissertation "http://www.is.ocha.ac.jp/~asai/papers/thesis.ps.gz")
                @(alumnus "Eli Barzilay" #:year 2005
                          #:personal-site "http://www.barzilay.org/")
                @(alumnus "Kathi Fisler" #:year 1996
                          #:personal-site "http://www.cs.wpi.edu/~kfisler/"
                          #:extra "(Worcester Polytechnic Institute, Worcester, MA)")
                @(alumnus "Mark Krentel" #:year 1989
                          #:personal-site "http://dblp.uni-trier.de/pers/hd/k/Krentel:Mark_W="
                          #:extra "(Rice University)")
                @(alumnus "David van Horn" #:year 2009
                          #:extra "(University of Maryland)"
                          #:personal-site "http://www.cs.umd.edu/~dvanhorn/")
                @(alumnus "Matthew Flatt" #:year 1999
                          #:personal-site "http://www.cs.utah.edu/~mflatt/"
                          #:extra "(University of Utah)")
                @(alumnus "Robby Findler" #:year 2002
                          #:personal-site "https://www.eecs.northwestern.edu/~robby/"
                          #:extra "(Northwestern University)")
                @(alumnus "Cormac Flanagan" #:year 1997
                          #:personal-site "https://users.soe.ucsc.edu/~cormac/"
                          #:extra "(UC Santa Cruz)")
                @(alumnus "Shriram Krishnamurthi" #:year 2001
                          #:personal-site "https://cs.brown.edu/~sk/"
                          #:extra "(Brown University)")
                @(alumnus "Rebecca Parsons" #:year 1992
                          #:personal-site "https://www.thoughtworks.com/profiles/rebecca-parsons"
                          #:extra "(ThoughtWorks)")
                @(alumnus "Amr Sabry" #:year 1994
                          #:personal-site "http://www.cs.indiana.edu/~sabry/"
                          #:extra "(Indiana University)")
                @(alumnus "Dorai Sitaram" #:year 1994
                          #:personal-site "https://ds26gte.github.io/"
                          #:extra "(Verizon Labs, Waltham, MA)")
                @(alumnus "Andrew K. Wright" #:year 1994
                          #:extra "(Cisco)")
                @(alumnus "Mario Latendresse" #:year 1999
                          #:personal-site "http://www.ai.sri.com/~latendre/"
                          #:extra "(SRI International)")
                @(alumnus "Joe Marshall" #:year 1999
                          #:extra "(Google)")
                @(alumnus "Riccardo Pucella" #:year 2004
                          #:personal-site "http://www.ccs.neu.edu/home/riccardo/"
                          #:extra "(Olin College)")
            )
          </div>
        </div>
        @footer
      </div>
    </div>
  </body>
</html>
