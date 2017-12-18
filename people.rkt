#lang scribble/html

@; Page for lab members, past and present.
@;
@; NOTE: make sure all images for people are
@;   - size    : 180x180
@;   - quality : as low as possible
@;
@;  To change size with imagemagick:
@;   `convert -size 180x180 ORIGINAL-FILENAME.jpg NEW-FILENAME.jpg`
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
                  #:title "Professor"
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

          @person[#:name "Will Clinger"
                  #:title "Professor"
                  #:website "http://ccs.neu.edu/home/will"
                  #:e-mail "will@ccs.neu.edu"
                  #:history @list[]
                  #:img "Clinger.jpg"]{
            I focus on the design, specification, and implementation
            of functional and higher-order programming languages. My
            research interests stem from programming languages’
            ability to connect mathematically sophisticated theories
            of syntax and semantics to economically important details
            of computer hardware and software.  In recent years, I
            have designed algorithms for garbage collection for
            Larceny -- a vehicle for experimental research on compiler
            optimization and garbage collection. Larceny has become
            one of the leading multiplatform implementations of the
            Scheme programming language.}

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
                  #:website "http://ccs.neu.edu/home/olin"
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
                  #:title "Associate Professor"
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

          @person[#:name "Ryan Culpepper"
                  #:title "Research Scientist"
                  #:e-mail "ryanc@ccs.neu.edu"
                  #:website "http://ccs.neu.edu/home/ryanc"
                  #:history @list[
                    "Joined Northeastern, 2013"
                    "Post-doc University of Utah, 2011"
                    "PhD, Northeastern University, 2010"
                  ]
                  #:img "ryan_culpepper.jpg"]{
            Born in Houston, TX, left, went back to go to Rice University. Interested in PL and compilers. I read during the summer and play table tennis during the winter. I'm still looking for someone up here who has heard of disc golf.
          }

          @person[#:name "Stephen Chang"
                  #:title "Post-doctoral researcher"
                  #:e-mail "stchang@ccs.neu.edu"
                  #:website "http://ccs.neu.edu/home/stchang"
                  #:history @list[
                    "Joined Northeastern, 2014"
                    "PhD, Northeastern University, 2014"
                    "MS, Harvard University, 2008"
                    "BSE, Princeton University, 2001"
                  ]
                  #:img "stephen_chang.jpg"]{
            I'm interested in the design of practical programming languages.
          }

          @person[#:name "Ben Lerner"
                  #:title "Lecturer"
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
                  #:e-mail "j.vitek@neu.edu"
                  #:website "http://janvitek.org"
                  #:history '("Joined Northeastern, 2014"
                              "Joined Purdue, 1999"
                              "PhD, University of Geneva, 1999"
                              "MSc, University of Victoria, 1995")
                  #:img "jan_vitek.jpg"]{
            I work on the design and implementation of programming languages. I led the implementation of the first real-time Java virtual machine to be flight-tested. With Noble and Potter, I proposed what became known as Ownership Types.  I tried to understand JavaScript by dynamic analysis and am now looking at supporting scalable data analysis in R.
          }

          @person[#:name "Paley Li"
                 #:title "Post-doctoral researcher"
                 #:e-mail "pa.li@neu.edu"
                 #:website "https://palez.github.io"
                 #:history @list[
                   "Joined Northeastern, 2015"
                   "PhD, Victoria University of Wellington, 2015"
                 ]
                 #:img "paleyli.jpg"]{
            My research interests is in programming languages, specifically type theory, ownership types, and local reasoning for memory management. Most recently, I have been working on formalising object cloning for ownership types.
          }

          @person[#:name "Fillip Křikava"
                  #:title "Post-doctoral researcher"
                  #:e-mail "f.krikava@neu.edu"
                  #:website "http://fikovnik.net"
                  #:history @list[
                    "Joined Northeastern, 2016"
                    "Joined INRIA Lille, 2013"
                    "PhD, University of Nice Sophia-Antipolis, 2013"]
                  #:img "fillip_krikava.jpg"]{
            Coming from software engineering and domain-specific modeling. I’m interested in various aspects of programming languages in particular in static and dynamic program analysis. Most recently I have been working on dynamic tracing in R.
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

          @person[#:name "Heather Miller"
                  #:title "Assistant Clinical Professor"
                  #:e-mail "heather@ccs.neu.edu"
                  #:website "http://heather.miller.am"
                  #:history @list[
                  "Joined Northeastern, 2016"
                  "PhD, EPFL, 2015"
                  ]
                  #:img "heather_miller.jpg"]{
            I work on the intersection of programming languages and distributed systems. I am a member of the Scala team, and have worked on Scala's concurrency libraries including Scala's futures and promises library, and compile-time support for distributed programs. Much of my work is focused on transferring research results into practical use. Before joining Northeastern, I co-founded and served as the executive director of a new center at EPFL for research, education, and open source development surrounding the Scala programming language, called the Scala Center.
          }

          @person[#:name "Konrad Siek"
                  #:title "Post-doctoral researcher"
                  #:e-mail "siekkonr@fit.cvut.cz" 
                  #:website "https://kondziu.github.io"
                  #:history @list[
                    "Joined Northeastern, 2017"
                    "PhD, Poznań University of Technology, 2017"]
                  #:img "konrad.jpg"]{
            Coming from software engineering and domain-specific modeling. I’m interested in various aspects of programming languages in particular in static and dynamic program analysis. Most recently I have been working on dynamic tracing in R.
          }
        }

        @div[class: "pn-separator-img"]{
           @h2{Students}}

        @div[class: "container"]{

          @person[#:name "Tony Garnock-Jones"
                  #:title "Advisor: Matthias Felleisen"
                  #:e-mail "tonyg@ccs.neu.edu"
                  #:website "http://www.ccs.neu.edu/home/tonyg"
                  #:history @list["Joined 2010"]
                  #:img "tony_jones.jpg"]{
            I’m working on constructing programming languages that incorporate ideas from networking and from messaging middleware.
          }

          @person[#:name "Jonathan Schuster"
                  #:title "Advisor: Olin Shivers"
                  #:e-mail "schuster@ccs.neu.edu"
                  #:website "http://ccs.neu.edu/home/schuster"
                  #:history @list["Joined 2011"]
                  #:img "jonathan_schuster.jpg"]{
            Having come from a development background, my research interests lie in making software development easier by improving programming languages and the ecosystems surrounding them. Currently, my research focuses on verifying actor-based programs (such as those written in Erlang or the Akka framework) against behavioral specifications expressed as simple name-passing automata.
          }

          @person[#:name "Justin Slepak"
                  #:title "Advisor: Olin Shivers"
                  #:e-mail "jrslepak@ccs.neu.edu"
                  #:website "http://ccs.neu.edu/home/jrslepak"
                  #:history @list["Joined 2011"]
                  #:img "justin_slepak.jpg"]{
            I came to Northeastern after spending several years studying in Upper Michigan. My current work focuses on array-oriented languages as an expressive way to write data-parallel numeric processing code.
          }

          @person[#:name "William J. Bowman"
                  #:title "Advisor: Amal Ahmed"
                  #:e-mail "wjb@williamjbowman.com"
                  #:website "https://www.williamjbowman.com"
                  #:history @list["Joined 2012"]
                  #:img "william_bowman.jpg"]{
            I want to make programs easier to design, read, and write. I currently work on verifying compilers for advanced languages, and sometimes dabble in meta-programming.
          }

          @person[#:name "Leif Andersen"
                  #:title "Advisor: Matthias Felleisen"
                  #:e-mail "leif@ccs.neu.edu"
                  #:website "http://ccs.neu.edu/home/leif"
                  #:history @list["Joined 2014"]
                  #:img "leif_andersen.jpg"]{
            I study programming languages in PLT at Northeastern University. I study compilers, domain specific languages for writing compilers, and performance tools.
          }

          @person[#:name "Andrew Cobb"
                  #:title "Advisor: Olin Shivers"
                  #:e-mail "andrew.cobb@gmail.com"
                  #:website ""
                  #:history @list["Joined 2014"]
                  #:img "Andrew-square.jpg"]{
          }

          @person[#:name "Ben Greenman"
                  #:title "Advisor: Matthias Felleisen"
                  #:e-mail "types@ccs.neu.edu"
                  #:website "http://ccs.neu.edu/home/types"
                  #:history @list["Joined 2014"]
                  #:img "ben_greenman.jpg"]{
            I like constructions. My current goal is to lower the huge run-time cost of gradual typing. I believe that safely mixing languages is the way of the future.
          }

          @person[#:name "Max New"
                  #:title "Advisor: Amal Ahmed"
                  #:e-mail "maxnew@ccs.neu.edu"
                  #:website "http://maxsnew.github.io"
                  #:history @list["Joined 2014"]
                  #:img "max_new.jpg"]{
            I like reasonable programming languages.
          }

          @person[#:name "Sam Caldwell"
                  #:title "Advisor: Matthias Felleisen"
                  #:e-mail "samc@ccs.neu.edu"
                  #:website "http://ccs.neu.edu/home/samc"
                  #:history @list["Joined 2015"]
                  #:img "Samuel-Caldwell-Index-Image.jpg"]{
            I came to Northeastern from Austin, Texas, where I did my undergrad and spent several years working in embedded software. I’m interested in using ideas and tools from programming languages to make the world a better place for everyone that uses computers.
          }

          @person[#:name "Benjamin Chung"
                  #:title "Advisor: Jan Vitek"
                  #:e-mail "bchung@ccs.neu.edu"
                  #:website "http://benchung.github.io"
                  #:history @list["Joined 2015"]
                  #:img "Benjamin-Chung-Index-Image.jpg"]{
            I usually work on types, currently focusing on gradual type systems.
          }

          @person[#:name "Olivier Flückiger"
                  #:title "Advisor: Jan Vitek"
                  #:e-mail "o@o1o.ch"
                  #:website "http://www.o1o.ch/about"
                  #:history @list["Joined 2015"]
                  #:img "olivier_fluckiger.jpg"]{
            My passion lies in language implementation @|mdash| creating the tools and techniques required to get from powerful abstractions to efficient execution.
          }

          @person[#:name "Daniel Patterson"
                  #:title "Advisor: Amal Ahmed"
                  #:e-mail "dbp@ccs.neu.edu"
                  #:website "https://dbp.io"
                  #:history @list["Joined 2016"]
                  #:img "daniel_patterson.jpg"]{
            I'm interested in language and type system interoperability @|mdash| in particular, languages with very different type systems interacting easily and safely.
          }

          @person[#:name "Ming-Ho Yee"
                  #:title "Advisor: Jan Vitek"
                  #:e-mail "mhyee@ccs.neu.edu"
                  #:website "http://mhyee.com/"
                  #:history @list["Joined 2016"]
                  #:img "yee-crop.jpg"]{
            I'm interested in programming language design and implementation, as well as static program analysis.
          }

          @person[#:name "Kevin Clancy"
                  #:title "Advisor: Heather Miller"
                  #:e-mail "kclancy@ccs.neu.edu"
                  #:history @list["Joined 2016"]
                  #:img "kevin-crop.jpg"]{
            I'm interested in type systems, particularly refinement type systems these days. I’m currently in the process of designing a refinement type system which can prove functions monotone. It would have applications in concurrency and numerical computing.
          }

          @person[#:name "Aviral Goel"
                  #:title "Advisor: Jan Vitek"
                  #:e-mail "goel.av@husky.neu.edu"
                  #:history @list["Joined 2017"]
                  #:img "aviral-goel.jpeg"]{
            Currently, I am working on static program analysis. I am interested in all aspects of programming languages @|mdash| syntax, semantics, type-systems, and compilers.
          }

          @person[#:name "Aaron Weiss"
                  #:title "Advisor: Amal Ahmed"
                  #:e-mail "weiss@ccs.neu.edu"
                  #:website "https://aaronweiss.us/"
                  #:history @list["Joined 2017"]
                  #:img "aaron_weiss.jpg"]{
            I'm interested in type systems and verification. I want to help build a world where the critical software that runs our lives is provably safe.
          }

          @person[#:name "Celeste Hollenbeck"
                  #:title "Advisor: Jan Vitek"
                  #:e-mail "hollenbeck.c@husky.neu.edu"
                  #:website "http://celestehollenbeck.com"
                  #:history @list["Joined 2017"]
                  #:img "celeste_hollenbeck.jpg"]{
            I'm into program analysis, machine learning, computational topology @|mdash| nasty things that bloody my nose.
      I like to put tiaras on them and make them have tea with each other. At knifepoint.
          }

          @person[#:name "Di Zhong"
                  #:title "Advisor: Frank Tip"
                  #:e-mail "dizhong@ccs.neu.edu"
                  #:history @list["Joined 2017"]
                  #:img "di_zhong.jpg"]{
            At the moment I'm working on program analysis. I like type systems and compilers.
          }

          @person[#:name "Hyeyoung Shin"
                  #:title "Advisor: Amal Ahmed and Matthias Felleisen"
                  #:e-mail "shin.hy@husky.neu.edu"
                  #:history @list["Joined 2017"]
                  #:img "hyeyoung_shin.jpg"]{
            I am working on safe interaction between strict and lazy languages, using linking types.
          }
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
              @(alumnus "Paul Stansifer" #:year 2016
                        #:dissertation "https://repository.library.northeastern.edu/files/neu:cj82mb52h"
                        #:personal-site "http://www.ccs.neu.edu/home/pauls/")
              @(alumnus "Asumu Takikawa" #:year 2016
                        #:dissertation "https://repository.library.northeastern.edu/files/neu:cj82n981x"
                        #:personal-site "http://www.ccs.neu.edu/home/asumu/"
                        #:extra "(Igalia)")
             ]}}

        @div[class: "container"]{
          @div[class: "row"]{
            @h3[style: "text-align: center;"]{Former Associates (Research Scientists, Post-Docs)}
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
              ]}}
        @footer{}
}}}}
