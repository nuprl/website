#lang scribble/html

@; Page for lab members, past and present.
@;
@; NOTE: make sure all images for people are
@;   - size    : 180x180
@;   - quality : as low as possible
@;
@;  To change size with imagemagick:
@;   `convert -resize 180x180 ORIGINAL-FILENAME.jpg NEW-FILENAME.jpg`
@;
@;  To change quality with imagemagick:
@;   `convert -sampling-factor 4:2:0 -strip -quality 85 -interlace JPEG -colorspace RGB ORIGINAL-FILENAME.jpg NEW-FILENAME.jpg`
@;  (Try replacing '85' with a smaller number. Go lower until it looks bad!)

@; -----------------------------------------------------------------------------
@require[
  "templates.rkt"
]

@(define (string-empty? s)
   (string=? "" s))

@(struct alum (name year psite dsite extra) #:transparent)

@(define (alumnus name #:year year #:personal-site [psite #f] #:dissertation [dsite #f] #:extra [extra #f])
  (alum name year psite dsite extra))

@(define (alumnus->string alm)
   @li[@add-between[@list[
     @a[href: (alum-psite alm) (alum-name alm)]
     @number->string[(alum-year alm)]
     @(let ([diss-link (alum-dsite alm)])
        (if diss-link
          @a[href: diss-link]{(dissertation)}
          ""))
     @alum-extra[alm]] " "]])

@(define (alumnus<? a1 a2)
   (or (< (alum-year a1) (alum-year a2))
       (and (= (alum-year a1) (alum-year a2))
            (string<? (alum-name a1) (alum-name a2)))))

@(define (alumnus*->ul a*)
   @div[class: "col-md-4"]{
     @(apply ul (map alumnus->string a*))})

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
  @div[a-str*])

@doctype{html}
@html[lang: "en"]{
  @header{People}
  @body[id: "pn-top" class: "subpages"]{
    @navbar{People}
    @subpage-title{Staff}

    @div[class: "pn-main-wrapper"]{
      @div[class: "content"]{
        @div[class: "container"]{

          @person[#:name "Mitch Wand"
                  #:title "Emeritus Professor"
                  #:website "http://ccs.neu.edu/home/wand"
                  #:e-mail "wand@ccs.neu.edu"
                  #:history @list[
                    "Joined Northeastern, 1985"
                    "Joined Indiana University, 1973"
                    "PhD, MIT, 1973"
                    "BS, MIT, 1969"]
                  #:img "mitch_wand.jpg"]{
            Over the years, I have worked on a variety of problems associated with semantics of programming languages.
            Here is a selected list, in roughly reverse chronological order: probabilistic programming languages, binding-safe programming, aspect-oriented programming, analysis-based program transformation, compiler correctness proofs, continuations, macros.
          }

          @person[#:name "Karl Lieberherr "
                  #:title "Professor"
                  #:website "http://ccs.neu.edu/home/lieber/"
                  #:e-mail "lieber@ccs.neu.edu"
                  #:history @list[
                    "Joined Northeastern, 1985"
                    "Joined Princeton, 1979"
                    "Ph.D. and Diplom ETH Zurich, 1968-1977"]
                  #:img "karl_lieberherr.jpg"]{
            My current research is about using deep reinforcement learning and Monte Carlo Tree Search (a Google/DeepMind algorithm) to synthesize instance-based programs to solve problems formulated in interpreted first-order logic, e.g., algorithmic problems.
            My earlier work in programming methodology centers on using (1) context-free grammars for abstract data types and domain-specific languages (the Demeter Method); (2) a succinct and exact definition of a detector for an interesting class of software design problems: The Law of Demeter (Don't Talk to Strangers); it allows for a direct application when writing object-oriented code; (3) aspect-oriented programming.
            I am also interested in problem-solving methodology in algorithms and I helped to develop the classical golden-ratio result for Satisfiability.
          }

          @person[#:name "Matthias Felleisen"
                  #:title "Trustee Professor"
                  #:e-mail "matthias@ccs.neu.edu"
                  #:website "http://ccs.neu.edu/home/matthias"
                  #:history @list[
                    "Joined Northeastern, 2001"
                    "Joined Rice, 1987"
                    "PhD, Indiana University, 1987"
                    "Diplom TH Karlsruhe, 1984"
                    "MS University of Arizona, 1981"]
                  #:img "matthias_felleisen.jpg"]{
            I explore all aspects of program design and programming language design.  My current research involves work on behavioral software contracts, gradual typing of scripting languages, language interoperability, language extensibility, and module systems. I also engage in educational outreach work. For the past 20 years, I have worked with middle schools, high schools, after-school programs, and college faculty on injecting design ideas into mathematics and computer science courses. Such educational interactions often inspire our research, and many research efforts end up improving my educational work.
          }

          @person[#:name "Olin Shivers"
                  #:title "Professor"
                  #:e-mail "olin@ccs.neu.edu"
                  #:website "http://www.ccs.neu.edu/home/shivers/"
                  #:history @list[
                    "Joined Northeastern, 2006"
                    "Joined Georgia Tech, 1999"
                    "Joined MIT, 1993"
                    "PhD, Carnegie Mellon University, 1991"
                    "BS, Yale University, 1983"
                  ]
                  #:img "olin_shivers.jpg"]{
            My principal research interests include the construction of robust, complex software artifacts and the design of tools that assist programmers in this task; the interaction between systems and programming languages, primarily higher-order typed languages; the design and analysis of programming languages; and compilers. Before coming to Northeastern, I was a research scientist at MIT’s Artificial Intelligence Lab, a founder and CTO of the Smartleaf Corporation, and a faculty member at the Georgia Institute of Technology.
          }

          @person[#:name "Amal Ahmed"
                  #:title "Professor"
                  #:e-mail "amal@ccs.neu.edu"
                  #:website "http://ccs.neu.edu/home/amal"
                  #:history @list[
                    "Joined Northeastern, 2011"
                    "Joined Indiana University, 2009"
                    "Joined Toyota Technological Institute, 2006"
                    "Joined Harvard University, 2004"
                    "PhD Princeton University, 2004"
                  ]
                  #:img "amal.jpg"]{
            I work on problems involving semantics of programming languages, including advanced type systems for programs that manipulate memory, correct and secure compilation, gradual typing, and language interoperability.  My prior work has shown how to scale the logical relations proof method to realistic languages.  This technique has been used in numerous contexts, e.g., to prove compiler correctness, to verify concurrent code, to establish guarantees provided by type systems for confidentiality or differential privacy.  My present focus is on how to build verified compilers that ensure safe linking of code compiled from different programming languages.
          }

          @person[#:name "Ben Lerner"
                  #:title "Associate Teaching Professor"
                  #:e-mail "blerner@ccs.neu.edu"
                  #:history @list[
                    "Joined Northeastern, 2014"
                    "PhD, University of Washington, 2011"
                  ]
                  #:img "ben_lerner.jpg"]{
            I have worked on problems in web programming semantics, including designing and analyzing extensibility mechanisms for browsers, studying the interactions between extensions and each other or with intended browser behavior.  With colleagues at Brown, I have been helping to design and implement a language that focuses on the linguistic support needed for introductory-level pedagogy.
          }

          @person[#:name "Jan Vitek"
                  #:title "Professor"
                  #:e-mail "vitekj@me.com"
                  #:website "http://janvitek.org"
                  #:history '("Joined Northeastern, 2014"
                              "Joined Purdue, 1999"
                              "PhD, University of Geneva, 1999"
                              "MSc, University of Victoria, 1995")
                  #:img "jan_vitek.jpg"]{
            I work on the design and implementation of programming languages. I led the implementation of the first real-time Java virtual machine to be flight-tested. With Noble and Potter, I proposed what became known as Ownership Types.  I tried to understand JavaScript by dynamic analysis and am now looking at supporting scalable data analysis in R.
          }

          @person[#:name "Frank Tip"
                  #:title "Professor"
                  #:e-mail "tip@acm.org"
                  #:website "http://www.franktip.org/"
                  #:history @list[
                  "Joined Northeastern, 2016"
                  "Joined Samsung Research America, 2014"
                  "Joined University of Waterloo, 2012"
                  "Joined IBM T.J. Watson Research Center, 1995"
                  "PhD University of Amsterdam, 1995"
                  ]
                  #:img "frank_tip.jpg"]{
            My research is in the areas of Programming Languages and Software Engineering and is focused on the use of program analysis in tools that help increase programmer productivity and software quality. Specific topics that I've worked on in recent years include tools for detecting and localizing bugs, refactoring, test generation, and optimization.
          }

          @person[#:name "Arjun Guha"
                  #:title "Associate Professor"
                  #:e-mail "a.guha@northeastern.edu"
                  #:website "https://ccs.neu.edu/~arjunguha/"
                  #:history @list[
                  "Joined Northeastern, 2020"
                  "Joined University of Massachusetts Amherst, 2013"
                  "Postdoc, Cornell University, 2012–2013"
                  "PhD, Brown University, 2012"
                  "BA, Grinnell College, 2006"
                  ]
                  #:img "arjun_guha.png"]{
            I have broad interests in programming languages, but usually work on language-based approaches to security, reliability, and performance. I've spent several years thinking about JavaScript (semantics, type checking, static analysis, reactive programming, and more). I also spent several years working on programming languages for software-defined networking. These days, I study problems that arise in cloud computing, system configurations, and robotics, through the lens of programming languages.
          }

          @person[#:name "Jon Bell"
                  #:title "Assistant Professor"
                  #:e-mail "jon@jonbell.net"
                  #:website "https://www.jonbell.net/"
                  #:history @list[

                  "Joined Northeastern, 2020"
                  "Joined George Mason University, 2016"
                  "PhD, Columbia University, 2016"
                  "M.Phil, Columbia University, 2014"
                  "MS, Columbia University, 2011"
                  "BS, Columbia University, 2010"

                  ]
                  #:img "jon_bell.jpg"]{
            I apply a systems perspective to software engineering challenges, observing the issues that developer face when creating reliable software, and then designing new mechanisms to support developers. My research focuses on improving existing developer-written tests, making them run faster and more reliably while amplifying them to be more comprehensive and also tracking their overall quality. Rather than focus solely on finding a handful of high-value “million dollar bugs” in a small pool of high-assurance software, my research aims to have very broad impacts, helping everyday developers just as much as experts.
          }

          @person[#:name "Steven Holtzen"
                  #:title "Assistant Professor"
                  #:e-mail "s.holtzen@northeastern.edu"
                  #:website "https://www.khoury.northeastern.edu/home/sholtzen/"
                  #:history @list[

                  "Joined Northeastern, 2021"
                  "PhD, University of California, Los Angeles, 2021"

                  ]
                  #:img "steven_holtzen.png"]{
            My research focuses on programming languages and artificial intelligence. In particular, my goal is to use programming languages and program analysis techniques as a foundation for specifying and reasoning about probabilistic models. Towards this end I am interested in the design, implementation, and applications of probabilistic programming languages; foundations of probabilistic inference and tractable probabilistic modeling; automated reasoning; and probabilistic verification.
          }

          @person[#:name "Daniel Patterson"
                  #:title "Assistant Teaching Professor"
                  #:e-mail "dbp@dbpmail.net"
                  #:website "https://dbp.io"
                  #:history @list[

                  "Joined Northeastern Faculty, 2022"
                  "PhD, Northeastern University, 2022"

                  ]
                  #:img "daniel_patterson.jpg"]{
            I'm interested in language and type system interoperability @|mdash| in particular, languages with very different type systems interacting easily and safely.
          }

          @person[#:name "Ryan Doenges"
                  #:title "Postdoc"
                  #:e-mail "r.doenges@northeastern.edu"
                  #:website "http://ryandoeng.es"
                  #:history @list[
                                  "Joined Northeastern, 2023"
                                  "PhD, Cornell University, 2023"
                                  "BS, University of Washington, 2017"]
                  #:img "ryan_doenges.jpg"]{I study computer systems using tools from programming languages and formal verification, especially domain-specific languages (DSLs). My PhD introduced mechanized semantics and certified translation validators for P4, a networking DSL. My postdoctoral research focuses on DSLs for hardware. }
        }

        @div[class: "pn-separator-img"]{
           @h2{Students}}

        @div[class: "container"]{

          @person[#:name "Ming-Ho Yee"
                  #:title "Advisor: Arjun Guha"
                  #:e-mail "mhyee@ccs.neu.edu"
                  #:website "http://mhyee.com/"
                  #:history @list["Joined Northeastern, 2016"]
                  #:img "yee-crop.jpg"]{
            I'm interested in programming language design and implementation, as well as static program analysis.
          }

          @person[#:name "Michael Ballantyne"
                  #:title "Advisor: Matthias Felleisen"
                  #:e-mail "michael.ballantyne@gmail.com"
                  #:website "http://mballantyne.net"
                  #:history @list["Joined Northeastern, 2017"]
                  #:img "ballantyne.jpg"]{
            I find delight in programming languages that allow extension of their notation, syntactic forms, type systems, runtime behaviors, and development environments. My research aims to build the foundations needed to bring these extensibility features into widespread use.
          }

          @person[#:name "Olek Gierczak"
                  #:title "Advisor: Amal Ahmed"
                  #:e-mail "gierczak.o@northeastern.edu"
                  #:history @list[
                    "Joined Northeastern, 2019"]
                  #:img "olek_gierczak.jpg"]{
            I like reasoning about programming languages and compilers, using pencil and paper or proof assistants.
          }

          @person[#:name "Cameron Moy"
                  #:title "Advisor: Matthias Felleisen"
                  #:e-mail "camoy@ccs.neu.edu"
                  #:website "http://camoy.name"
                  #:history @list[
                    "Joined Northeastern, 2019"]
                  #:img "cameron_camoy.jpg"]{
             I enjoy flexible programming languages and elegant software. I'm interested in designing tools that enable everyday developers to build more robust programs.
          }

          @person[#:name "Sam Stites"
                  #:title "Advisor: Steven Holtzen"
                  #:e-mail "prl@s.ptr.dev"
                  #:history @list[
                    "Joined Northeastern, 2019"]
                  #:img "sam_stites.jpg"
                  ]{
              I research probabilistic programming languages, specifically designing languages that let users fine-tune the (statistical) inference process.
          }

          @person[#:name "Nate Yazdani"
                  #:title "Advisor: Amal Ahmed"
                  #:e-mail "nyazdani@ccs.neu.edu"
                  #:website "https://nateyazdani.github.io/"
                  #:history @list[
                    "Joined Northeastern, 2019"
                    "MS, University of Washington, 2019"]
                  #:img "nate_yazdani.jpg"]{
             I like types, proofs, and occasionally programs.
          }

          @person[#:name "Andrew Wagner"
                  #:title "Advisor: Amal Ahmed"
                  #:e-mail "ahwagner@ccs.neu.edu"
                  #:history @list[
                    "Joined Northeastern, 2020"
                    "BS, Brown University, 2020"]
                  #:img "andrew_wagner.jpg"]{
            I like to design domain-specific languages with strong guarantees, which are usually supported by rich type systems and formal methods. My current research is on language-based security.
          }

          @person[#:name "Katherine Hough"
                  #:title "Advisor: Jonathan Bell"
                  #:e-mail "hough.k@northeastern.edu"
                  #:history @list[
                    "Joined Northeastern, 2020"
                    "MS, George Mason University"
                    "BS, George Mason University"]
                  #:img "Katherine_Hough.png"]{
            My research focuses on helping developers identify and correct software bugs and vulnerabilities.
          }

          @person[#:name "Donald Pinckney"
                  #:title "Advisor: Arjun Guha"
                  #:e-mail "donald_pinckney@icloud.com"
                  #:website "https://donaldpinckney.com"
                  #:history @list[
                    "Joined Northeastern, 2020"
                    "MS, UMass Amherst"
                    "BS, UC Davis"]
                  #:img "donald_pinckney.jpg"]{
            I enjoy working on formalizing semantics of systems so as to uncover surprising behavior, and fix related bugs. Recently I'm working on understanding the semantics of package managers.
          }

          @person[#:name "Michelle Thalakottur"
                  #:title "Advisor: Amal Ahmed, Frank Tip"
                  #:e-mail "michelledt@ccs.neu.edu"
                  #:website "https://michelledaviest.github.io/"
                  #:history @list[
                    "Joined Northeastern, 2021"]
                  #:img "michelle_thalakottur.jpg"]{
            I like thinking about programming languages and compilers.
          }

          @person[#:name "John Gouwar"
                  #:title "Advisor: Arjun Guha"
                  #:e-mail "gouwar.j@northeastern.edu"
                  #:website "https://johngouwar.github.io/"
                  #:history @list[
                    "Joined Northeastern, 2021"
                    "BA, Grinnell College"]
                  #:img "john_gouwar.jpg"]{
            I really enjoy functional programming and would like to develop languages that allow for the use of functional programming techniques in areas that they have not been used before.
          }

          @person[#:name "John Li"
                  #:title "Advisor: Amal Ahmed"
                  #:e-mail "johnli0135@gmail.com"
                  #:history @list[
                    "Joined Northeastern, 2021"]
                  #:img "john_li.jpg"]{
            I like logic, semantics, and interactive theorem proving.
          }
          
          @person[#:name "Satyajit Gokhale"
                  #:title "Advisor: Frank Tip"
                  #:e-mail "gokhale.sa@northeastern.edu"
                  #:history @list[
                    "Joined PRL, 2021"
                    "MS, Northeastern University"]
                  #:img "satyajit_gokhale.jpg"]{
            I am interested in program analysis and security. I am currently working on static analysis for JavaScript, and security in PLCs.
          }
        
          @person[#:name "James Perretta"
                  #:title "Advisors: Jonathan Bell and Arjun Guha"
                  #:e-mail "perretta.j@northeastern.edu"
                  #:history @list[
                    "Joined Northeastern, 2021"
                    "MS, University of Michigan"]
                  #:img "james_perretta.jpg"]{
            I completed my master's degree at University of Michigan, where I also developed an automated grading system that is used by over 5000 students per semester. My research interests lie at the intersection of PL and Software Engineering, and my current work is focused on mutation testing.
          }

          @person[#:name "Yangtian Zi"
                  #:title "Advisor: Arjun Guha"
                  #:e-mail "ytzi@ccs.neu.edu"
                  #:website "http://ytzi.org"
                  #:history @list[
                    "Joined Northeastern, 2021"]
                  #:img "ytzi.jpg"]{
            My interests are dynamic languages implementations, Just-in-time compilers, and WebAssembly.
          }

          @person[#:name "Max Bernstein"
                  #:title "Advisor: Frank Tip and Jan Vitek"
                  #:e-mail "bernstein.ma@northeastern.edu"
                  #:website "https://bernsteinbear.com"
                  #:history @list[
                    "Joined Northeastern, 2022"]
                  #:img "max_bernstein.png"]{
          }

          @person[#:name "Minsung Cho"
                  #:title "Advisor: Steven Holtzen"
                  #:e-mail "minsung@ccs.neu.edu"
                  #:history @list[
                      "Joined Northeastern, 2022"
                      "BS, Carnegie Mellon University"]
                  #:img "minsung_cho.jpg"
                  #:website "https://cho.minsung.pl"]{
            I'm a pure math ex-pat using logic and algebra to make probabilistic programming better.
          }

          @person[#:name "Gwenyth Lincroft"
                  #:title "Advisor: Jonathan Bell"
                  #:e-mail "lincroft.g@northeastern.edu"
                  #:history @list[
                      "Joined Northeastern, 2022"
                      "BS, NC State University"
                                  ]
                  #:img "gwenyth_lincroft.png"]{
            I am interested in improving tools used by data scientists.
          }

          @person[#:name "Francesca Lucchetti"
                  #:title "Advisor: Arjun Guha"
                  #:e-mail "lucchetti.f@northeastern.edu"
                  #:history @list["Joined Northeastern, 2022"
                                  "BA, Vassar College, 2022"]
                  #:img "franlucc.jpg"]{I’m interested in developing robust, performant Large Language Models for code that can be deployed with limited compute. I like taking apart transformer layers to interpret their inner workings.}
        }

        @div[class: "pn-separator-img"]{
           @h2{PRL Alumni and Former Members}}
        @div[class: "container"]{
          @div[class: "row"]{
            @alumnus-list[
              @(alumnus "Dino Oliva" #:year 1992
                        #:personal-site "http://cm.bell-labs.com/cm/cs/who/oliva/"
                        #:dissertation "ftp://www.ccs.neu.edu/pub/people/wand/papers/oliva-thesis-94.ps.Z")
              @(alumnus "Greg Sullivan" #:year 1997
                        #:personal-site "http://www.ai.mit.edu/%7Egregs"
                        #:dissertation "ftp://www.ccs.neu.edu/pub/people/wand/papers/sullivan-thesis-97.ps"
                        #:extra "Advisor: Mitch Wand, Employer: BAE Systems")
              @(alumnus "Ian Holland" #:year 1992
                        #:personal-site "http://thefirstchurch.org/"
                        #:dissertation "ftp://ftp.ccs.neu.edu/pub/people/lieber/theses/holland/thesis.ps"
                        #:extra "(First Swampscott Church)")
              @(alumnus "Paul Bergstein" #:year 1994
                        #:personal-site "http://www.cis.umassd.edu/~pbergstein/"
                        #:dissertation "ftp://ftp.ccs.neu.edu/pub/people/lieber/theses/bergstein/thesis.ps"
                        #:extra "(UMass, Dartmouth)")
              @(alumnus "Ignacio Silva-Lepe" #:year 1994
                        #:personal-site "http://researcher.watson.ibm.com/researcher/view.php?person=us-isilval"
                        #:dissertation "ftp://ftp.ccs.neu.edu/pub/people/lieber/theses/silva-lepe/thesis.ps"
                        #:extra "(IBM Research, Yorktown)")
              @(alumnus "Cun Xiao" #:year 1994
                        #:dissertation "ftp://ftp.ccs.neu.edu/pub/people/lieber/theses/xiao/thesis.ps"
                        #:extra "(Oracle)")
              @(alumnus "Walter Hürsch" #:year 1995
                        #:personal-site "https://ch.linkedin.com/in/walterhuersch"
                        #:dissertation "ftp://ftp.ccs.neu.edu/pub/people/lieber/theses/huersch/thesis.ps"
                        #:extra "(BlueCare AG)")
              @(alumnus "Linda Seiter" #:year 1996
                        #:personal-site "http://sites.jcu.edu/math/professor/linda-m-seiter/"
                        #:dissertation "ftp://ftp.ccs.neu.edu/pub/people/lieber/theses/seiter/thesis.ps"
                        #:extra "(John Carrol University)")
              @(alumnus "David Gladstein" #:year 1996
                        #:dissertation "ftp://www.ccs.neu.edu/pub/people/wand/papers/gladstein-thesis-94.ps.Z")
              @(alumnus "Crista Lopes" #:year 1997
                        #:personal-site "http://www.ics.uci.edu/~lopes/"
                        #:dissertation "ftp://ftp.ccs.neu.edu/pub/people/lieber/theses/lopes"
                        #:extra "(UC Irvine)")
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
              @(alumnus "Doug Orleans" #:year 2005
                        #:personal-site "http://steak.place.org/dougo/"
                        #:dissertation "http://www.ccs.neu.edu/research/demeter/theses/orleans"
                        #:extra "(Gensym)")
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
              @(alumnus "David Fisher" #:year 2010
                        #:personal-site "http://www.ccs.neu.edu/home/dfisher/"
                        #:dissertation "http://www.ccs.neu.edu/home/dfisher/diss.pdf")
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
                        #:personal-site "http://sstrickl.net/"
                        #:dissertation "http://www.ccs.neu.edu/racket/pubs/dissertation-strickland.pdf"
                        #:extra "(Google)")
              @(alumnus "Dionna Amalie Glaze" #:year 2015
                        #:personal-site "https://deeglaze.github.io"
                        #:dissertation "https://deeglaze.github.io/diss.pdf"
                        #:extra "(Google)")
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
                        #:dissertation "http://www.ccs.neu.edu/home/lieber/theses/abdelmeged/scg/ahmed-thesis.html"
                        #:personal-site "http://www.ccs.neu.edu/home/mohsen/HomePage/index.html")
              @(alumnus "Phillip Mates" #:year 2015
                        #:personal-site "https://philomates.github.io/"
                        #:extra "(Igalia)")
              @(alumnus "Erik Silkensen" #:year 2015
                        #:personal-site "http://www.ccs.neu.edu/home/ejs/")
              @(alumnus "Christos Dimoulas" #:year 2012
                        #:dissertation "http://www.ccs.neu.edu/racket/pubs/dissertation-dimoulas.pdf"
                        #:extra "(Harvard University)"
                        #:personal-site "http://people.seas.harvard.edu/~chrdimo/")
              @(alumnus "Philippe Meunier" #:year 2006
                        #:dissertation "http://www.ccs.neu.edu/racket/pubs/dissertation-meunier.pdf"
                        #:extra "(Sirindhorn International Institute of Technology, Tahmmasat University)")
              @(alumnus "Paul Stansifer" #:year 2016
                        #:dissertation "https://repository.library.northeastern.edu/files/neu:cj82mb52h"
                        #:personal-site "http://www.ccs.neu.edu/home/pauls/")
              @(alumnus "Asumu Takikawa" #:year 2016
                        #:dissertation "https://repository.library.northeastern.edu/files/neu:cj82n981x"
                        #:personal-site "http://www.ccs.neu.edu/home/asumu/"
                        #:extra "(Igalia)")
              @(alumnus "Tony Garnock-Jones" #:year 2017
                        #:dissertation "http://syndicate-lang.org/tonyg-dissertation/"
                        #:personal-site "https://leastfixedpoint.com/")
              @(alumnus "Kevin Clancy" #:year 2018
                        #:extra "(Carnegie Mellon University)"
                        #:personal-site "https://kevinclancy.github.io/")
              @(alumnus "William J. Bowman" #:year 2018
                        #:dissertation "https://www.williamjbowman.com/#dissertation"
                        #:extra "(University of British Columbia)"
                        #:personal-site "https://www.williamjbowman.com")
              @(alumnus "Andrew Cobb" #:year 2019)
              @(alumnus "Di Zhong" #:year 2019)
              @(alumnus "Hyeyoung Shin" #:year 2019
                        #:personal-site "https://hyeyoungshin.github.io/"
                        #:extra "(Czech Technical University in Prague)")
              @(alumnus "Jonathan Schuster" #:year 2019
                        #:dissertation "http://hdl.handle.net/2047/D20318587"
                        #:extra "(Google)"
                        #:personal-site "http://jschuster.org/")
              @(alumnus "Celeste Hollenbeck" #:year 2019
                        #:extra "(University of Edinburgh)"
                        #:personal-site "http://celestehollenbeck.com")
              @(alumnus "Justin Slepak" #:year 2020
                        #:dissertation "http://ccs.neu.edu/~jrslepak/Dissertation.pdf"
                        #:extra "(Facebook)"
                        #:personal-site "https://jrslepak.github.io/")
              @(alumnus "Max S. New" #:year 2020
                        #:dissertation "http://maxsnew.com/docs/dissertation.pdf"
                        #:extra "(Wesleyan University)"
                        #:personal-site "http://maxsnew.com/")
              @(alumnus "Ben Greenman" #:year 2020
                        #:dissertation "http://hdl.handle.net/2047/D20398329"
                        #:extra "(Brown University)"
                        #:personal-site "https://users.cs.utah.edu/~blg/")
              @(alumnus "Leif Andersen" #:year 2022
                      #:personal-site "https://leifandersen.net/index.html"
                      #:dissertation "https://www2.ccs.neu.edu/racket/pubs/dissertation-andersen.pdf"
                      #:extra "(UMass Boston)")
              @(alumnus "Olivier Flückiger" #:year 2022
                      #:personal-site "http://www.o1o.ch/about"
                      #:dissertation "https://thesis.r-vm.net/main.pdf")
              @(alumnus "Daniel Patterson" #:year 2022
                        #:dissertation "https://dbp.io/pubs/2022/dbp-dissertation.pdf"
                        #:extra "(Northeastern University)"
                        #:personal-site "https://dbp.io/")
              @(alumnus "Aaron Weiss" #:year 2022
                        #:extra "(Roblox)"
                        #:personal-site "https://aaronweiss.us/")
              @(alumnus "Benjamin Chung" #:year 2023
                      #:personal-site "http://benchung.github.io"
                      #:dissertation "https://benchung.github.io/papers/thesis.pdf")
              @(alumnus  "Aviral Goel" #:year 2023
                         #:personal-site "http://aviral.io/"
                         #:dissertation "https://onesearch.library.northeastern.edu/permalink/01NEU_INST/87npqb/cdi_proquest_journals_2802795686")
              @(alumnus "Julia Belyakova" #:year 2023
                        #:personal-site "http://julbinb.github.io/"
                        #:dissertation "https://onesearch.library.northeastern.edu/permalink/01NEU_INST/87npqb/cdi_proquest_journals_2853689755")
              @(alumnus "Alexi Turcotte" #:year 2023
                        #:personal-site "https://reallytg.github.io/"
                        #:dissertation "https://onesearch.library.northeastern.edu/permalink/01NEU_INST/87npqb/cdi_proquest_journals_2851062525")
              @(alumnus "Ellen Arteca" #:year 2023
                        #:personal-site "https://emarteca.github.io/"
                        #:dissertation "https://onesearch.library.northeastern.edu/permalink/01NEU_INST/87npqb/cdi_proquest_journals_2851106784")
              @(alumnus "Sam Caldwell" #:year 2023
                        #:personal-site "http://ccs.neu.edu/home/samc"
                        #:dissertation "https://onesearch.library.northeastern.edu/permalink/01NEU_INST/87npqb/cdi_proquest_journals_2851110205")
              @(alumnus "Artem Pelenitsyn" #:year 2023
                        #:personal-site "https://a.pelenitsyn.top"
                        #:extra "(Purdue University)"
                        #:dissertation "https://a.pelenitsyn.top/Papers/2023-dissertation.pdf")

              ]}}

        @div[class: "container"]{
          @div[class: "row"]{
            @h3[style: "text-align: center;"]{Former Associates (Research Scientists, Post-Docs, Visiting Faculty)}
              @alumnus-list[
                @(alumnus "Kenichi Asai" #:year 2004
                          #:personal-site "http://www.is.ocha.ac.jp/~asai/"
                          #:extra "(Ochanomizu University)")
                @(alumnus "Eli Barzilay" #:year 2005
                          #:personal-site "http://www.barzilay.org/")
                @(alumnus "Kathi Fisler" #:year 1996
                          #:personal-site "http://www.cs.wpi.edu/~kfisler/"
                          #:extra "(Worcester Polytechnic Institute)")
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
                @(alumnus "Gabriel Scherer" #:year 2017
                          #:personal-site "http://gallium.inria.fr/~scherer/"
                          #:extra "(INRIA Saclay)")
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
                @(alumnus "Paley Li" #:year 2017
                          #:personal-site "https://palez.github.io"
                          #:extra "(Oracle)")
                @(alumnus "Fillip Křikava" #:year 2017
                          #:personal-site "http://fikovnik.net/"
                          #:extra "(Czech Technical University in Prague)")
                @(alumnus "Konrad Siek" #:year 2017
                          #:personal-site "https://kondziu.github.io"
                          #:extra "(Czech Technical University in Prague)")
                @(alumnus "Ryan Culpepper" #:year 2017
                          #:personal-site "http://www.ccs.neu.edu/home/ryanc/"
                          #:extra "(Czech Technical University in Prague)")
                @(alumnus "Saba Alimadadi" #:year 2019
                          #:personal-site "https://www.ece.ubc.ca/~saba/"
                          #:extra "(Simon Fraser University)")
                @(alumnus "Stephen Chang" #:year 2020
                          #:personal-site "https://stchang.github.io/"
                          #:extra "(University of Massachusetts Boston)")
                @(alumnus "John H. Reppy" #:year 2020
                          #:personal-site "http://people.cs.uchicago.edu/~jhr/"
                          #:extra "(visited from University of Chicago)")
                @(alumnus "John Boyland" #:year 2021
                          #:personal-site "http://www.cs.uwm.edu/faculty/boyland/"
                          #:extra "(visited from University of Wisconsin-Milwaukee)")
              ]}}

        @div[class: "container"]{
          @div[class: "row"]{
            @h3[style: "text-align: center;"]{Former Faculty}
              @alumnus-list[
                @(alumnus "Heather Miller" #:year 2018
                          #:personal-site "http://heather.miller.am/"
                          #:extra "(Carnegie Mellon University)")
                @(alumnus "Will Clinger" #:year 2019
                          #:personal-site "https://en.wikipedia.org/wiki/William_Clinger_(computer_scientist)")
                @(alumnus "Jason Hemann" #:year 2022
                          #:personal-site "https://jasonhemann.github.io/"
                          #:extra "(Seton Hall University)")
              ]}}
        @footer{}
}}}}
