#lang scribble/html
@require[
  "templates.rkt"
  (only-in racket/list add-between make-list)
]

@(define (vertical-text . str*)
   (add-between (string->list (append* str*)) (br)))

@(define (indent)
   @(make-list 4 nbsp))

@(define (instructor . name)
   @span[class: "pn-muted" name])

@doctype{html}
@html[lang: "en"]{
  @header{Teaching}
  @body[id: "pn-top"]{
    @navbar{Teaching}
    @subpage-title{Teaching}

    @div[class: "pn-main-wrapper"]{
      @div[class: "content"]{
        @div[class: "container"]{
          @div[class: "row"]{
            @p{PRL Faculty & Staff write academic @a[href: "#books1"]{books},  teach @a[href: "#courses"]{courses}, and write @a[href: "#books2"]{more books}.}}}

        @div[class: "pn-separator-img-2-short"]

        @div[class: "container"]{
          @a[name: "#books1" id: "books1"]
            @div[class: "row"]{
              @div[class: "col-md-12"]{
                @div[class: "col-md-4"]{
                  @span[class: "how-to-design-programs"]{@vertical-text{How to Design}}
                  @p{@img[src: "img/how-to-design-programs.jpg" alt: "How to Design Programs"] @br{} @span[class: "how-to-design-programs-2"]{Programs}}}
                @div[class: "col-md-8"]{
                  @p{@i{How To Design Programs} focuses on the program design process,
                     distinguishing it from other introductory books.
                     This approach fosters a variety of skills --- critical reading,
                     analytical thinking, creative synthesis, and attention to detail.
                     On the surface we use engaging contexts, our "hello world" program
                     is an animation, and students have the opportunity to program games, etc.}
                  @p{We have spent over twenty years developing the related Program By Design curriculum and have offerings at the middle-school, high-school and university levels. Our material is even used for in-house corporate training.}
                  @p{This is the primary textbook for our courses @a[href: "http://www.ccs.neu.edu/course/cs5010/"]{CS 5010} and @a[href: "http://www.ccs.neu.edu/course/cs2500/"]{CS 2500}.
                    @url{www.htdp.org} @br{}
                    @url{http://www.ccs.neu.edu/home/matthias/HtDP2e/} @br{}
                    @url{http://www.programbydesign.org}
                    @url{http://www.bootstrapworld.org}}
                  } @br{} }}}

            @div[class: "pn-separator-img-2"]
            @div[class: "container"]{
              @div[class: "row"]{
                @div[class: "col-md-12"]{
                  @div[class: "col-md-8"]{
                    @p{@i{Essentials of Programming Languages} is a study of programming languages. Our goal is to provide a deep, working  understanding of the essential concepts of programming languages. These essentials have proved to be of enduring importance; they form a basis for understanding future developments in programming languages.}
                    @p{Our approach is both theoretical and hands-on. The book provides views of programming languages using widely varying levels of abstraction, maintaining a clear connection between the high-level and low-level views. The text uses interpreters to express the semantics of many essential language elements in a way that is both clear and executable.}
                    @p{@url{http://www.eopl3.com}}}
                  @div[class: "col-md-4"]{
                      @span[class: "essentials-of-programming-languages"]{@vertical-text{Essentials}}
                      @p[style: "text-align: right ;"]{
                        @img[src: "img/essentials-of-programming-languages.jpg" alt: "Essentials of programming languages" style: "float: right;"]
                        @br{}
                        @span[class: "essentials-of-programming-languages-2"]{of Programming @br{} Languages}}}}}}

        @div[class: "pn-separator-img-2"]

        @div[class: "container"]{
          @div[class: "row"]{
             @div[class: "col-md-12"]{
               @div[class: "col-md-4"]{
                 @span[class: "how-to-design-programs"]{@vertical-text{Semantics}}
                 @p{@img[src: "img/redex.jpg" width: "250px" alt: "Semantics Engineering with PLT Redex"]
                    @br{}
                    @span[class: "how-to-design-programs-2"]{Engineering with @br{} PLT Redex}}}
               @div[class: "col-md-8"]{
                 @p{@i{Semantics Engineering with PLT Redex} is the first comprehensive presentation of reduction semantics in one volume. It also introduces the first reliable and easy-to-use tool set for such forms of semantics.}
                 @p{Automatic tool support is critical for rapid prototyping and modeling and this book is addressed to the working semantics engineer. With PLT Redex, semanticists can formulate models as grammars and reduction models on their computers with the ease of paper-and-pencil designs.}
                 @p{This text first presents a framework for the formulation of language models, focusing on equational calculi and abstract machines, then introduces PLT Redex, a suite of software tools for expressing these models as PLT Redex models. Finally, experts describe a range of models formulated in Redex.}
                 @p{This is the primary textbook for our course @a[href: "http://www.ccs.neu.edu/home/amal/course/7400-s16/"]{CS 7400} - Intensive Principles of Programming Languages.}
                 @p{@url{http://redex.racket-lang.org/}}
                 @br{} @br{}
               }}}}

         @div[class: "pn-separator-img-2-short"]

         @div[class: "container"]{
           @div[class: "row"]{
             @div[class: "col-md-12"]{
               @h2{Courses}
               @a[name: "#courses" id: "courses"]}
             @div[class: "col-md-6"]{
               @ul{
                 @li{
                   @strong{7680} Special Topics in Computer Systems
                   @br{}
                   @indent{} F 2016 Programming Models for Distributed Computing @a[href: "http://heather.miller.am/teaching/cs7680/" target: "_blank"]{link} @instructor{Heather Miller}
                 }
                 @li{
                   @strong{7480} Special Topics in Programming Languages
                   @br{}
                   @indent{} F 2016 Advanced Program Analysis @a[href: "https://docs.google.com/document/d/1JeYoC-dmCxLgYuQcv5tU1rfez6IRf_OKO4ipeRtQd7o" target: "_blank"]{link} @instructor{Frank Tip}
                   @br{}
                   @indent{} F 2015 Types, Contracts, and Gradual Typing @a[href: "http://www.ccs.neu.edu/home/amal/course/7480-f15/" target: "_blank"]{link} @instructor{Amal Ahmed}
                   @br{}
                   @indent{} S 2012 Type Systems @a[href: "http://www.ccs.neu.edu/home/amal/course/7480-s12/" target: "_blank"]{link} @instructor{Amal Ahmed}
                 }
                 @li{
                   @strong{7400} Intensive Principles of Programming Languages
                   @br{}
                   @indent{} S 2015 @a[href: "http://www.ccs.neu.edu/home/amal/course/7400-s15/" target: "_blank"]{link} @instructor{Amal Ahmed} @br{}
                   @indent{} S 2014 @a[href: "http://www.ccs.neu.edu/home/matthias/7400-s14/" target: "_blank"]{link} @instructor{Matthias Felleisen} @br{}
                   @indent{} F 2012 @a[href: "http://www.ccs.neu.edu/home/matthias/7400-f12/" target: "_blank"]{link} @instructor{Matthias Felleisen} @br{}
                   @indent{} F 2011 @a[href: "http://www.ccs.neu.edu/home/matthias/7400-f11/" target: "_blank"]{link} @instructor{Matthias Felleisen}
                 }
                 @li{
                   @strong{6515} Software Design and Development @br{}
                   @indent{} S 2013 @a[href: "http://www.ccs.neu.edu/home/matthias/6515-s13/" target: "_blank"]{link} @instructor{Matthias Felleisen}
                 }
                 @li{
                   @strong{6240} Parallel Data Processing in MapReduce @br{}
                   @indent{} S 2015 @a[href: "https://sites.google.com/site/mapreduce15s/" target: "_blank"]{link} @instructor{Jan Vitek}
                 }
                 @li{
                   @strong{5010} Program Design Paradigms @br{}
                   @indent{} F 2015 @a[href: "http://www.ccs.neu.edu/course/cs5010f15" target: "_blank"]{link} @instructor{Mitch Wand, William D. Clinger} @br{}
                   @indent{} S 2015 @a[href: "http://www.ccs.neu.edu/course/cs5010sp15" target: "_blank"]{link} @instructor{Stephen Chang} @br{}
                   @indent{} F 2014 @a[href: "http://www.ccs.neu.edu/course/cs5010f14" target: "_blank"]{link} @instructor{Mitch Wand, Stephen Chang, Jan Vitek} @br{}
                   @indent{} S 2014 @a[href: "http://www.ccs.neu.edu/course/cs5010sp14/" target: "_blank"]{link} @instructor{Ryan Culpepper} @br{}
                   @indent{} F 2013 @a[href: "http://www.ccs.neu.edu/course/cs5010f13/" target: "_blank"]{link} @instructor{Ryan Culpepper}
                 }
                 @li{
                   @strong{4620} Building Extensible Systems @br{}
                   @indent{} S 2015 @a[href: "http://www.ccs.neu.edu/home/matthias/4620-s15/" target: "_blank"]{link} @instructor{Matthias Felleisen}
                 }
             }}
             @div[class: "col-md-6"]{
               @ul{
                 @li{
                     @strong{4410} Compilers @br{}
                     @indent[] F 2013 @a[href: "http://www.ccs.neu.edu/home/amal/course/4410-s13/" target: "_blank"]{link} @instructor{Amal Ahmed} @br{}
                     @indent[] S 2012 @a[href: "http://www.ccs.neu.edu/course/csu4410/" target: "_blank"]{link} @instructor{Olin Shivers}
                 }
                 @li{
                     @strong{3800} Theory of Computation @br{}
                     @indent[] F 2015 @a[href: "http://www.ccs.neu.edu/course/cs3800f15wc/" target: "_blank"]{link} @instructor{William D. Clinger}
                 }
                 @li{
                     @strong{3500} Object Oriented Design @br{}
                     @indent[] F 2015 @a[href: "http://www.ccs.neu.edu/course/cs3500/" target: "_blank"]{link} @instructor{Ben Lerner}
                 }
                 @li{
                     @strong{2510} Fundamentals of Computer Science 2 @br{}
                     @indent[] S 2015 @a[href: "http://www.ccs.neu.edu/course/cs2510sp15/" target: "_blank"]{link} @instructor{Ben Lerner} @br{}
                     @indent[] F 2014 @a[href: "http://www.ccs.neu.edu/course/cs2510f14/" target: "_blank"]{link} @instructor{Ben Lerner} @br{}
                     @indent[] S 2014 honors @a[href: "http://www.ccs.neu.edu/home/cs2510hsp14/" target: "_blank"]{link} @instructor{Ben Lerner}
                 }
                 @li{
                     @strong{2500} Fundamentals of Computer Science @br{}
                     @indent{} F 2016 @a[href: "http://www.ccs.neu.edu/course/cs2500f16/" target: "_blank"]{link} @instructor{Matthias Felleisen, Amal Ahmed, Ben Lerner} @br{}
                     @indent{} F 2016 accelerated @a[href: "http://www.ccs.neu.edu/course/cs2500accelf16/" target: "_blank"]{link} @instructor{Olin Shivers} @br{}
                     @indent{} F 2015 @a[href: "http://www.ccs.neu.edu/course/cs2500hfa15/" target: "_blank"]{link} @instructor{Olin Shivers} @br{}
                     @indent{} F 2015 honors @a[href: "http://www.ccs.neu.edu/course/cs2500f15/" target: "_blank"]{link} @instructor{Ben Lerner} @br{}
                     @indent{} F 2014 @a[href: "http://www.ccs.neu.edu/course/cs2500f14/index.html" target: "_blank"]{link} @instructor{Olin Shivers, Amal Ahmed, Ben Lerner} @br{}
                     @indent{} S 2014 @a[href: "http://www.ccs.neu.edu/course/cs2500sp14/index.html" target: "_blank"]{link} @instructor{Amal Ahmed} @br{}
                     @indent{} F 2013 @a[href: "http://www.ccs.neu.edu/course/cs2500f13/index.html" target: "_blank"]{link} @instructor{Matthias Felleisen, Amal Ahmed} @br{}
                     @indent{} F 2011 @a[href: "http://www.ccs.neu.edu/course/csu211/" target: "_blank"]{link} @instructor{Amal Ahmed}
                 }}}}}

         @div[class: "pn-separator-img-2-short"]

         @div[class: "container"]{
           @div[class: "row"]{
             @div[class: "col-md-12"]{
               @div[class: "col-md-8"]{
                 @a[name: "#books2" id: "books2"]
                 @p{Racket is a descendant of Lisp, a programming language renowned for its elegance, power, and challenging learning curve. But while Racket retains the functional goodness of Lisp, it was designed with beginning programmers in mind and Realm of Racket is an introduction to the Racket language.}
                 @p{Our approach teaches programming by creating increasingly complex games. The journey begins with the Guess My Number game and coverage of some basic Racket etiquette. Next, readers dig into syntax and semantics, lists, structures, and conditionals, and learn to work with recursion and the GUI in building the Robot Snake game. Then it's on to lambda and mutant structs (and an Orc Battle), and fancy loops and the Dice of Doom. Finally, readers explore laziness, AI, distributed games, and the Hungry Henry game.}
                 @p{@url{http://realmofracket.com/realmofracket.com}}
                 @br{}}
               @div[class: "col-md-4"]{
                 @p[style: "text-align: right;"]{
                   @img[src: "img/racket.jpg" width: "250px" alt: "Realm of  Racket"]
                   @br{}
                   @span[class: "essentials-of-programming-languages-2"]{Realm of Racket}}}}}}

         @div[class: "pn-separator-img-2-short"]

         @div[class: "container"]{
           @div[class: "row"]{
             @div[class: "col-md-12"]{
               @div[class: "col-md-4"]{
                 @p{@img[width: "350px" src: "img/littles.gif" alt: "The Littles Series" style: "float: right;"]
                 @br{}
                 @span[class: "how-to-design-programs-2"]{The Littles Series}}}
               @div[class: "col-md-8"]{
                 @p{The Littles Series open new doors of thought to everyone who wants to find out what computing is really about.}
                 @p{The original, @i{Little LISPer}, unfolds some of the most beautiful concepts in mathematics, computer science, and logic. The follow-on books further the notion that 'thinking about computing is one of the most exciting things the human mind can do.' This sets both @i{The Little Schemer} and its companion volume, @i{The Seasoned Schemer}, apart from other books on LISP.}
                 @p{@i{The Little MLer} introduces one of the most important members of the family of programming languages. ML has emerged as a natural language for software engineering courses because it provides a sophisticated and expressive module system and is the language of choice for some NU CCIS courses.}
                 @p{Design patterns, which moved object-oriented programming to a new level, provide programmers with a language to communicate with others about their designs. As a result, programs become more readable, more reusable, and more easily extensible. @i{A Little Java, A Few Patterns}, use a small subset of Java to introduce pattern-directed program design.}
               }}}}
  @footer{}
}}}}
