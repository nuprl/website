#lang scribble/html
@(require
   racket/date
   gregor
   "templates.rkt")

@(define id 0)
@(define (seminar anchor title speaker link aff date room abstract bio)
   (set! id (add1 id))
   @list[
     @div[id: @format["seminar-~a" id] class: "col-md-12 pn-seminar"]{
       @script/inline[type: "text/javascript"]{
         if (new Date() >= new Date("@(datetime->iso8601 date)")) {
            document.getElementById("seminar-@|id|").classList.add("finished");
         }
       }
       @div[id: anchor class: "pn-main-informations"]{
         @a[onclick: "return false;" class: "pn-title" title]
         @br[]
         @span[class: "pn-name" speaker]
         @span[class: "pn-affiliation" aff]
         @a[class: "pn-url" target: "_blank" href: link link]
         @br[]
         @span[class: "pn-datetime" (~t date "d MMMM, y h:mma ")]{
           @a[href: (string-join (list "#" anchor) "")]{
             @span[class: "glyphicon glyphicon-link"]}}
         @span[class: "pn-room" room]
         @seminar-notes[anchor]}
       @div[class: "pn-abstract-bio"]{
         @p{
           @span[class: "pn-title-2"]{Abstract}
           @span[abstract]}
         @p{
           @span[class: "pn-title-2"]{Bio}
           @span[@|bio|]}}}
     @br{}])

@;; Valid file extensions for seminar notes
@(define seminar-notes-extensions '(".md" ".txt" ".org"))

@;; seminar-notes : String -> Element
@;; Search for a file of talk notes using the "anchor" to a seminar.
@;; If notes exist, return a link to them. Otherwise return an empty Element.
@(define (seminar-notes anchor)
   (define notes-path-no-ext (build-path "seminar-notes" anchor))
   (define all-notes-paths
     (for/fold ([acc '()])
               ([ext (in-list seminar-notes-extensions)])
       (define p (path-add-extension notes-path-no-ext ext))
       (if (file-exists? p) (cons p acc) acc)))
   (cond
    [(null? all-notes-paths)
     @span[class: "pn-room"]]
    [(not (null? (cdr all-notes-paths)))
     (raise-user-error 'seminar-notes "Found multiple notes files for seminar '~a'. Delete (or merge) the extras and rebuild." anchor)]
    [else
     @span[class: "pn-room"]{| @a[class: "pn-url" target: "_blank" href: @path->github-url[(car all-notes-paths)]]{Notes}}]))

@; path->github-url : Path -> String
@; Convert a filepath (relative to the root of the `nuprl/website` repository)
@;  to a URL to the same file on GitHub.
@(define (path->github-url p)
   (string-append "https://github.com/nuprl/website/blob/master/" (path->string p)))

@; TODO Have seminar return a struct; sort it by date, then create output.
@; TODO: Have seminars contain a datetime range, rather than just a start time.
@(define seminars
   (splice
    ;; (seminar
    ;;  "IDENTIFIER"
    ;;  "TITLE"
    ;;  "AUTHOR"
    ;;  "WEBSITE"
    ;;  "INSTITUTION"
    ;;  (datetime 2016 10 20 12 00)
    ;;  "WVH 366"
    ;;  @list{@p{ABSTRACT}}
    ;;  @list{@p{BIO}})

    (seminar
     "hartzell-templates"
     "Templates and Types in C++"
     "Jimmy Hartzell"
     ""
     "Tower Research"
     (datetime 2017 3 10 12 00)
     "WVH 366"
     @list{@p{Like many modern programming languages, C++ supports both generic
                   programming (templates) and object-oriented programming (inheritance and
              virtual functions). Unlike many modern programming languages, the generic
             programming mechanism does not fully leverage or interact with the type
              system. It amounts to compile-time duck typing: C++ function templates,
              like functions from a non-statically typed programming language, have
              their full interface implicit in their implementations -- making it hard
              to discern what that interface actually is.  The concept of Concepts
              (compile-time typeclasses) was slated for introduction into C++ in
             the C++11 version of the standard, then C++14, then C++17, now C++20... What can
            we do to work around their absence?}@p{Relatedly, how do we bridge the gap
            between static and dynamic polymorphism? C++ provides both, and sometimes we
            want to mix and mash them. Template tricks can also be done to accomplish this
            as well.}} @list{@p{Jimmy Hartzell is a software developer and C++ instructor at Tower Research.}})

    (seminar
     "sherman-patterns"
     "Overlapping pattern matching for programming with continuous functions"
     "Ben Sherman"
     "http://www.ben-sherman.net/"
     "MIT"
     (datetime 2017 2 24 12 00)
     "WVH 366"
     @list{@p{Topology, when viewed from an unusual perspective (formal topology), describes how to compute with certain objects that are beyond the reach of induction, such as real numbers, probability distributions, streams, and function spaces. Accordingly, one gets a programming language whose types are spaces and whose functions are continuous maps.}@p{Does this language have pattern matching? In functional programming, pattern matching allows definition of a function by partitioning the input and defining the result in each case. We generalize to programming with spaces, where patterns need not represent decidable predicates and also may overlap, potentially allowing nondeterministic behavior in overlapping regions. These overlapping pattern matches are useful for writing a wide array of computer programs on spaces, such as programs that make approximate computations or decisions based on continuous values or that manipulate "partial" datatypes.}@p{This talk will introduce topology from a computational perspective, and explore some programs that can be built with this framework using overlapping pattern matching.}}
     @list{@p{Ben Sherman is a second-year PhD student at MIT, advised by Adam Chlipala and Michael Carbin.}})

    (seminar
     "belyakova-oo"
     "Comparative Study of Generic Programming Features in Object-Oriented Languages"
     "Julia Belyakova"
     "http://staff.mmcs.sfedu.ru/~juliet/index.en.html"
     "Northeastern University"
     (datetime 2017 2 3 12 00)
     "WVH 366"
     @list{@p{Earlier comparative studies of language support for generic programming (GP) have shown that mainstream object-oriented (OO) languages such as C# and Java provide weaker support for GP as compared with functional languages such as Haskell or SML.}@p{One reason is that generic mechanisms of C# and Java are based on F-bounded polymorphism, which has certain deficiencies. Some of these deficiencies are eliminated in more recent languages, such as Scala and Kotlin. However, there are peculiarities of object-oriented languages as a whole, which prevent them from being as expressible as Haskell in terms of generic programming.}@p{In this talk we will cover the main problems of language support for generic programming in C#/Java as well as other modern object-oriented languages, including Scala and Swift. It turns out that all of these languages follow the same approach to constraining type parameters, which leads to inevitable shortcomings. To overcome them, an alternative approach is suggested in several extensions for OO languages, in particular, JavaGenus. The advantages and drawbacks of both approaches will be discussed in the talk.}@p{Here are Julia's @a[href: "http://staff.mmcs.sfedu.ru/~juliet/files/materials/seminar-talks/belyakova-PL-sem-2017-slides.pdf"]{Slides}.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   }}
     @list{@p{Julia Belyakova graduated in 2014 from Southern Federal University (Rostov-on-Don, Russia) as Master of Science in “Computer Science and Information Technologies”. She is enrolled for PhD program in Southern Federal University and came to Northeastern University as a visiting scientist. She is currently doing research in the area of generic programming for object-oriented languages.}})


    (seminar
     "vilk-browsers"
     "Making the Browser Reasonable for Sane Programmers"
     "John Vilk"
     "https://jvilk.com/"
     "University of Massachusetts Amherst"
     (datetime 2017 01 27 12 00)
     "WVH 366"
     @list{@p{Billions of people use browsers to access the web from a variety of devices, but it remains difficult to write and debug the client-side of web applications. The browser exposes unfamiliar, redundant, and incompatible abstractions for a variety of common tasks, preventing developers from adapting existing code written for other platforms. The browser environment is completely event-driven and pervasively concurrent, resulting in complex control flow and race conditions that are difficult to debug.}@p{In this talk, I will describe how my research makes it easier to write and debug web applications. Doppio is a JavaScript runtime system that lets developers run unaltered code written in general-purpose languages, such as Java and C/C++, directly inside the browser. Browsix further enhances Doppio with a kernel, processes, and shared operating system resources, letting multiple off-the-shelf programs concurrently interoperate on the same webpage. ReJS is a low-overhead and high-fidelity time-traveling debugger that lets developers instantaneously step forward and backward through a program's execution. ReJS accurately recreates the application's complete behavior, including the GUI and multiple types of race conditions, while producing miniscule and portable application traces. These resources transform the browser into a first-class application platform.}}
     @list{@p{John Vilk is a Ph.D. candidate in the College of Information and Computer Sciences at the University of Massachusetts Amherst. He is a member of the PLASMA lab, and is advised by Emery Berger. John's research aims to improve the browser as an application platform by making it easier to write, debug, and optimize complex web applications. John is a Facebook Fellow (2015), and his work on Doppio: Breaking the Browser Language Barrier is a SIGPLAN Research Highlight (2014) and a PLDI Distinguished Artifact (2014). His research forms the basis of Microsoft ChakraCore's time-traveling debugger and lets millions of people interact with legacy software and games in their browser at the Internet Archive (https://archive.org/). John received his MS from the University of Massachusetts Amherst (2013) and his BS from Worcester Polytechnic Institute (2011).}@p{You can learn more about John at his website, https://jvilk.com/.}})


    (seminar
     "new-retractionsblame"
     "Retractions and Blame"
     "Max New"
     "http://maxsnew.github.io/"
     "Northeastern University"
     (datetime 2016 12 15 12 00)
     "Behrakis 105"
     @list{@p{It has long been noted that research on contracts and gradual typing bears a striking resemblance to Dana Scott's work on retracts in domain theory. However, a concrete relationship has been elusive, especially since the notion of *blame* has seemingly no counterpart in domain theory.}@p{We connect these fields by means of gradual type precision, which has been characterized previously using blame in [1]. We propose a definition in terms of section-retraction pairs, allowing us to relate blame and retractions in a precise way. Our definition agrees with [1] on most rules, but differs in a way that suggests a modification to their definition of blame.}@p{[1] Wadler and Findler, Well-typed Programs can't Be Blamed:
http://homepages.inf.ed.ac.uk/wadler/topics/blame.html#blame-esop}}
     @list{@p{Max New is a PhD student at Northeastern University working on the semantic foundations of programming languages. He hates proving the same theorem twice and you should too.}})

    (seminar
     "scherer-emptytype"
     "Deciding program equivalence with sums and the empty type"
     "Gabriel Scherer"
     "http://www.ccs.neu.edu/home/gasche/"
     "Northeastern University"
     (datetime 2016 12 7 12 00)
     "WVH 110"
     @list{@p{The simply-typed λ-calculus, with only function types, is strongly normalizing, and its program equivalence relation is decidable: unlike in more advanced type system with polymorphism or effects, the natural "syntactic" equivalence (βη-equivalence) corresponds to natural "semantic" equivalence (observational or contextual equivalence), and is decidable. Adding product types (pairs) is easy and preserves these properties, but sums (disjoint unions) are, surprisingly, harder. It is only in 1995 that Neil Ghani proved that the equivalence in presence of sums is decidable, and the situation in presence of the empty type (zero-ary sum) was unknown.}@p{We propose an equivalence algorithm for sums and the empty type that takes inspiration from a proof-theoretic technique, named "focusing",designed for proof search. The exposition will be introductory; I will present the difficulties caused by sums and the empty type, present some of the existing approaches for sums in the literature, introduce focusing, and give a high-level intuition of our saturation-based algorithm and its termination argument. No previous knowledge of focusing is assumed. I expect some familiarity with typed lambda-calculi, but will recall all concepts used (for example, βη-equivalence) during the talk.}}
     @list{@p{Gabriel is interested in theoretical aspects of type systems,
programming language implementation, general programming language
concepts, and even some syntactic aspects. He has a preference for the
formalizable aspects, or formalizable approaches to programming
language aspects, rather than the often subjective appeal to taste or
intuition.}})


    (seminar
     "dolby-wala"
     "Analysis of Android hybrid applications and other fun with WALA"
     "Julian Dolby"
     "http://researcher.ibm.com/person/us-dolby"
     "IBM Research"
     (datetime 2016 12 1 12 00)
     "Richards Hall 140"
     @list{@p{Hybrid apps help developers build multiple apps for different platforms with less duplicated effort, by providing platform-specific functionality via native code and user interactions via javascript code. however, most hybrid apps are developed in multiple programming languages with different semantics, complicating programming. moreover, untrusted javascript code may access device-specific features via native code, exposing hybrid apps to attacks. Unfortunately, there are no existing tools to detect such vulnerabilities. In this paper, we present HybriDroid, the first static analysis framework for Android hybrid apps. First, we investigate the semantics of interoperation of Android Java and JavaScript. Then, we design and implement a static analysis framework that analyzes inter-communication between Android Java and JavaScript. We demonstrate HybriDroid with a bug detector that identifies programmer errors due to the hybrid semantics, and a taint analyzer that finds information leaks cross language boundaries. Our empirical evaluation shows that the tools are practically usable in that they found previously uncovered bugs in real-world Android hybrid apps and possible information leaks via a widely-used advertising platform.}@p{The bulk of this presentation will focus on ASE 2016 work on analysis of hybrid apps (1), a blend of per-platform native code and portable JavaScript. I will also briefly discuss two other recent projects involving WALA: ASE 2015 work on a practically tunable static analysis framework for large-scale JavaScript applications (2), and ISSTA 2015 work on scalable and precise taint analysis for Android (3).}@p{1: Sungho Lee, Julian Dolby, Sukyoung Ryu: HybriDroid: static analysis framework for Android hybrid applications. ASE 2016: 250-261}@p{2: Yoonseok Ko, Hongki Lee, Julian Dolby, Sukyoung Ryu: Practically Tunable Static Analysis Framework for Large-Scale JavaScript Applications (T). ASE 2015: 541-551}@p{3: Wei Huang, Yao Dong, Ana Milanova, Julian Dolby: Scalable and precise taint analysis for Android. ISSTA 2015: 106-117}}
     @list{@p{Julian Dolby is a Research Staff Member at the IBM Thomas J. Watson Research Center, where he works on a range of topics, including static program analysis, software testing, concurrent programming models and the semantic Web. He is one of the primary authors of the publically available Watson Libraries for Analysis (WALA) program analysis infrastructure, and his recent WALA work has focused on creating the WALA Mobile infrastructure.}@p{His program analysis work has recently focused on scripting languages like JavaScript and on security analysis of Web applications; this work has been included in IBM products, most notably Rational AppScan, Standard Edition and Source Edition. He was educated at the University of Illinois at Urbana-Champaign as a graduate student where he worked with Professor Andrew Chien on programming systems for massively-parallel machines.}})

    (seminar
     "ariola-sequent"
     "Sequent calculus as a programming language"
     "Zena Ariola"
     "http://ix.cs.uoregon.edu/~ariola/"
     "University of Oregon"
     (datetime 2016 11 28 12 00)
     "WVH 110"
     @list{@p{We will present and demonstrate the usefulness of the sequent
calculus as a formal model of computation based on interactions
between producers and consumers of results. This model leads
to a better understanding of call-by-name evaluation by
reconciling the conflicting principles of extensionality and
weak-head evaluation, thus internalizing a known parametricity
result. It allows one to explore two dualities of computation:
the duality between call-by-name and call-by-value, and the
duality between construction and deconstruction. This ultimately
leads to a better linguistic foundation for co-induction as dual
to induction. From a more practical point of view, the sequent
calculus provides a useful inspiration for the design of
intermediate languages.}}
     @list{@p{TBD}})

    (seminar
     "culpepper-probablistic"
     "Contextual Equivalence of Probabilistic Programs"
     "Ryan Culpepper"
     "http://www.ccs.neu.edu/home/ryanc/"
     "Northeastern University"
     (datetime 2016 11 03 12 00)
     "WVH 366"
     @list{@p{
In this talk I introduce a probabilistic programming language with continuous random variables and soft constraints, define contextual equivalence for the language, and then present a logical relation that is sound with respect to contextual equivalence.}@p{The question of equivalence for probabilistic programs is interesting and difficult because the meaning of a program is a measure defined by integrating over the space of all of the program's possible evaluations. Thus creating a workable logical relation requires proving integral equalities involving intermediate measures that are ``equal enough'' but still respect contextual equivalence.}}
     @list{@p{Ryan Culpepper is a research scientist at Northeastern University.  He works on probabilistic programming as well as extensible languages and tools for building domain-specific languages. He received his PhD at Northeastern and did a postdoc at the University of Utah.}})

    (seminar
     "meiklejohn-computation"
     "Declarative, Convergent Edge Computation"
     "Christopher Meiklejohn"
     "https://christophermeiklejohn.com/"
     "Université Catholique
de Louvain"
     (datetime 2016 10 27 12 00)
     "WVH 366"
     @list{@p{Consistency is hard and coordination is expensive. As we move into the
world of connected 'Internet of Things' style applications, or
large-scale mobile applications, devices have less power, periods of
limited connectivity, and operate over unreliable asynchronous
networks. This poses a problem with shared state: how do we handle
concurrent operations over shared state, while clients are offline,
and ensure that values converge to a desirable result without making
the system unavailable?}
             @p{We look at a new programming model, called Lasp. This programming
model combines distributed convergent data structures with a dataflow
execution model designed for distribution over large-scale
applications. This model supports arbitrary placement of processing
node: this enables the user to author applications that can be
distributed across data centers and pushed to the edge.}}
     @list{@p{Christopher Meiklejohn loves distributed systems and programming
languages. Previously, Christopher worked at Basho Technologies, Inc.
on the distributed key-value store, Riak. Christopher develops a
programming model for distributed computation, called Lasp.
Christopher is currently a Ph.D. student at the Université Catholique
de Louvain in Belgium.}})

    (seminar
     "verlaguet-hack"
     "The Hack Experiment & HHVM, Then And Now"
     "Julien Verlaguet & Brett Simmers"
     "http://hacklang.org/"
     "Facebook"
     (datetime 2016 10 25 13 30)
     "WVH 366"
     @list{@p{The Hack programming language developed at Facebook is an evolution of PHP. With tens of millions  of lines of PHP code in house, the lack of type and poor IDE support were felt to be major threats to Facebook. We designed Hack gradually, adding feature at regular intervals and encouraging developers to adopt them in their code. This talk will touch on the design of the Hack language, its type system, the implementation of the type checker, and the social processes involved in converting 20 millions lines of untyped code to a rich type system.}@p{HHVM is an open source virtual machine for PHP and Hack programs. It was developed by Facebook but is also used by Wikipedia, Baidu, Etsy, and many others. This talk will discuss the history of the project, what goes on behind the scenes while executing your PHP code, and what we're currently working on.}}
     @list{@p{Julien Verlaguet is a OCaml programmer with a Master from Université Pierre et Marie Curie in Paris. Before joining Facebook in 2011, he worked at Esterel technologies on verified compilation.  At Facebook he designed the Hack programming language, a typed offspring of PHP and managed to convince the majority of developers in the company to add types to their code.}@p{Brett Simmers is a Software Engineer at Facebook, where he's been working on HHVM for the past five years. He primarily works on the internals of the JIT compiler and HHVM's profile-guided optimizations. Before joining Facebook he spent two years at VMware working on the Linux version of VMware Workstation.}})
    (seminar
     "yee-flix"
     "Implementing a Functional Language for Flix"
     "Ming-Ho Yee"
     "http://mhyee.com"
     "Northeastern University"
     (datetime 2016 10 20 12 00)
     "WVH 366"
     @list{@p{Static program analysis is a powerful technique for maintaining software, with applications such as compiler optimizations, code refactoring, and bug finding. Static analyzers are typically implemented in general-purpose programming languages, such as C++ and Java; however, these analyzers are complex and often difficult to understand and maintain. An alternate approach is to use Datalog, a declarative language. Implementors can express analysis constraints declaratively, which makes it easier to understand and ensure correctness of the analysis. Furthermore, the declarative nature of the analysis allows multiple, independent analyses to be easily combined.}
           @p{Flix is a programming language for static analysis, consisting of a logic language and a functional language. The logic language is inspired by Datalog, but supports user-defined lattices. The functional language allows implementors to write functions, something which is not supported in Datalog. These two extensions, user-defined lattices and functions, allow Flix to support analyses that cannot be expressed by Datalog, such as a constant propagation analysis. Datalog is limited to constraints on relations, and although it can simulate finite lattices, it cannot express lattices over an infinite domain. Finally, another advantage of Flix is that it supports interoperability with existing tools written in general-purpose programming languages.}
           @p{This talk provides an overview of the implementation and evaluation of the Flix functional language. The implementation involves abstract syntax tree transformations, an interpreter back-end, and a code generator back-end, and must support a number of interesting language features, such as pattern matching, first-class functions, and interoperability. The evaluation compares the interpreter and code generator back-ends in terms of correctness and performance.}}
     @list{@p{Ming-Ho Yee is a Ph.D. student at Northeastern University. He works on programming language design and implementation with Jan Vitek. He received a Bachelor of Software Engineering from the University of Waterloo, and then continued for a Master of Mathematics in Computer Science under the supervision of Ondřej Lhoták.}})
     (seminar
      "wahl-decisions"
     "Behavioral Non-Portability in Decision-Making Programs"
     "Thomas Wahl"
     "http://www.ccs.neu.edu/home/wahl/"
     "Northeastern University"
     (datetime 2016 10 13 12 00)
     "WVH 366"
     @list{@p{The precise semantics of floating-point arithmetic programs depends on the execution platform, including the compiler and the target hardware. Such platform dependencies infringe on the highly desirable goal of software portability (which is in fact promised by heterogeneous computing frameworks like OpenCL): the same program run on the same inputs on different platforms can produce different results. In other words, portability does not guarantee reproducibility, and this is a more or less accepted state of affairs.}
           @p{Serious doubts on the portability of numeric applications arise when differences in results are behavioral, i.e. when they lead to changes in the control flow of a program. In this talk I will first present an algorithm that takes a numeric procedure and determines whether a given input can lead to different decisions depending merely on how the arithmetic in the procedure is compiled and executed. I will then show how this algorithm can be used in static and dynamic analyses of programs, to estimate their numeric stability. I will illustrate the results on examples characteristic of numeric computing where control flow divergence actually occurs across different execution platforms.}
           @p{Joint with Yijia Gu, Mahsa Bayati, and Miriam Leeser, Northeastern University, Boston, USA}}
     @list{@p{Thomas Wahl joined the faculty of Northeastern University in 2011. His research concerns the reliability (whatever that means) of complex computing systems. Two domains notorious for their fragility are concurrency and numerical computing. With colleagues, Wahl has investigated how floating-point arithmetic can "hijack" a program's computation when run on non-standard architectures, such as heterogeneous and custom-made embedded platforms. You will witness some hijacking attempts in the talk today.}})
    (seminar
     "sen-concolic"
     "Concolic Testing: A Decade Later"
     "Koushik Sen"
      "https://www.cs.berkeley.edu/~ksen/"
      "University of California, Berkeley"
      (datetime 2016 10 06 12 00)
      "WVH 366"
      @list{@p{Symbolic execution, which was introduced more than four decades ago, is typically used in software testing to explore as many different program paths as possible in a given amount of time, and for each path to generate a set of concrete input values exercising it, and check for the presence of various kinds of errors including assertion violations, uncaught exceptions, security vulnerabilities, and memory corruption.  A key limitation of classical symbolic execution is that it cannot generate useful test inputs if the program under test uses complex operations such as pointer manipulations and non-linear arithmetic operations.}
            @p{Our research on Concolic Testing (also known as DART: Directed Automated Random Testing or Dynamic Symbolic Execution) alleviated the limitations of classical symbolic execution by combining concrete execution and symbolic execution.  We demonstrated that concolic testing is an effective technique for generating high-coverage test suites and for finding deep errors in complex software applications. The success of concolic testing in scalably and exhaustively testing real-world software was a major milestone in the ad hoc world of software testing and has inspired the development of several industrial and academic automated testing and security tools.}
            @p{One of the key challenges of concolic testing is the huge number of programs paths in all but the smallest programs, which is usually exponential in the number of static branches in the code.  In this talk I will describe MultiSE, a new technique for merging states incrementally during symbolic execution, without using auxiliary variables. The key idea of MultiSE is based on an alternative representation of the state, where we map each variable, including the program counter, to a set of guarded symbolic expressions called a value summary. MultiSE has several advantages over conventional symbolic execution and state merging techniques: 1) value summaries enable sharing of symbolic expressions and path constraints along multiple paths, 2) value-summaries avoid redundant execution, 3) MultiSE does not introduce auxiliary symbolic values, which enables it to make progress even when merging values not supported by the constraint solver, such as floating point or function values.  We have implemented MultiSE for JavaScript programs in a publicly available open-source tool. Our evaluation of MultiSE on several programs shows that MultiSE can run significantly faster than traditional symbolic execution.}}
      @list{@p{Koushik Sen is an associate professor in the Department of Electrical Engineering and Computer Sciences at the University of California, Berkeley. His research interest lies in Software Engineering, Programming Languages, and Formal methods. He is interested in developing software tools and methodologies that improve programmer productivity and software quality. He is best known for his work on ³DART: Directed Automated Random Testing² and concolic testing. He has received a NSF CAREER Award in 2008, a Haifa Verification Conference (HVC) Award in 2009, a IFIP TC2 Manfred Paul Award for Excellence in Software: Theory and Practice in 2010, a Sloan Foundation Fellowship in 2011, a Professor R. Narasimhan Lecture Award in 2014, and an Okawa Foundation Research Grant in 2015. He has won several ACM SIGSOFT Distinguished Paper Awards. He received the C.L. and Jane W-S. Liu Award in 2004, the C. W. Gear Outstanding Graduate Award in 2005, and the David J. Kuck Outstanding Ph.D. Thesis Award in 2007, and a Distinguished Alumni Educator Award in 2014 from the UIUC Department of Computer Science. He holds a B.Tech from Indian Institute of Technology, Kanpur, and M.S. and Ph.D. in CS from University of Illinois at Urbana-Champaign.}})
     (seminar
      "haller-lacasa"
     "LaCasa: Lightweight Affinity and Object Capabilities in Scala"
     "Philipp Haller"
     "http://www.csc.kth.se/~phaller/"
     "KTH Royal Institute of Technology"
     (datetime 2016 09 29 12 30)
     "WVH 366"
     @list{@p{Aliasing is a known source of challenges in the context of imperative object-oriented languages, which have led to important advances in type systems for aliasing control. However, their large-scale adoption has turned out to be a surprisingly difficult challenge. While new language designs show promise, they do not address the need of aliasing control in existing languages.}
           @p{This paper presents a new approach to isolation and uniqueness in an existing, widely-used language, Scala. The approach is unique in the way it addresses some of the most important obstacles to the adoption of type system extensions for aliasing control. First, adaptation of existing code requires only a minimal set of annotations. Only a single bit of information is required per class. Surprisingly, the paper shows that this information can be provided by the object-capability discipline, widely-used in program security. We formalize our approach as a type system and prove key soundness theorems. The type system is implemented for the full Scala language, providing, for the first time, a sound integration with Scala’s local type inference. Finally, we empirically evaluate the conformity of existing Scala open-source code on a corpus of over 75,000 LOC.}}
     @list{@p{Philipp Haller is an assistant professor in the theoretical computer science group at KTH Royal Institute of Technology, the leading technical university in Sweden. His main research interests are programming languages, type systems, concurrent, and distributed programming. Philipp is co-author of Scala's async/await extension for asynchronous computations, and one of the lead designers of Scala's futures and promises library. As main author of the book "Actors in Scala," he created Scala's first widely-used actors library. Philipp was co-chair of the 2013 and 2014 editions of the Scala Workshop, and co-chair of the 2015 ACM SIGPLAN Scala Symposium. Previously, he has held positions at Typesafe, Stanford University, and EPFL. He received a PhD in computer science from EPFL in 2010.}})
    (seminar
     "vitek-julia"
     "Performance in Julia"
     "Jan Vitek"
     "http://janvitek.org/"
     "Northeastern University"
     (datetime 2016 09 15 12 30)
     "WVH 366"
     @list{@p{Julia, like R, is a dynamic language for scientific computing but, unlike R, it was explicitly designed to deliver performance competitive to traditional batch-compiled languages. To achieve this Julia's designers made a number of unusual choices, including the presence of a set of type annotations that are used for dispatching methods and speed up code, but not for type-checking. The result is that many Julia programs are competitive with equivalent programs written in C. This talk gives a brief overview of the key points of Julia's design and considers whether similar ideas could be adopted in R.}}
     @list{@p{Jan Vitek, @url{"http://janvitek.org/"}, is a Professor at Northeastern University CCIS. He works on the design and implementation of programming abstractions for areas and in languages that are *terrifying*. For instance, he has worked on a real-time JVM for running Java on an actual embedding real-time system, on understanding JavaScript to make it more secure, and on getting R (yes R) to be scalable for data analysis. Apparently he does these things because they are used in the "real world" to "solve problems". He also has an excellent sense of humor and didn't give me a bio, so I took some liberties and hope he doesn't mind.}})

    (seminar
     "zeilberger-refinement"
     "A Categorical Perspective on Type Refinement"
     "Noam Zeilberger"
     "http://noamz.org/"
     "Inria Ecole Polytechnique"
     (datetime 2016 07 13 11 45)
     "WVH 366"
     @list{
       @p{
         A "type refinement system" is a type system built on top of
         a typed programming language, as an extra layer of typing.  Over the
         past few years Paul-André Melliès and I have investigated a
         categorical foundation for type refinement systems, which is based on
         the idea of looking at a type refinement system "backwards", in terms
         of the functor which erases the extra layer of types.  The talk will
         give an introduction to this framework as well as to some of the
         insights that it offers, such as:
       }
       @p{* The idea of viewing Hoare logic as a type refinement system}
       @p{* The importance of the bifibrational operations of "pushing forward" and
            "pulling back" (which generalize strongest postconditions and weakest preconditions)}
       @p{* How those operations can be combined with a monoidal closed structure, to
            model type refinement systems built over pure lambda calculus}
       @p{I will not assume any prior background in category theory, although as background reading audience
          members may be interested in the lecture notes for my recent course at OPLSS
          (@url{http://noamz.org/oplss16/refinements-notes.pdf"}, especially Ch.3).}}
     @list{Since receiving his PhD from Carnegie Mellon in 2009, Noam Zeilberger has been actively pursuing a career as an itinerant postdoc, with gigs in Paris (Laboratoire PPS), Madrid (IMDEA Software), Princeton (IAS), and back in Paris (MSR-Inria).  He is currently a member of Inria Team Parsifal at Ecole Polytechnique.  In his free time, he enjoys enumerating lambda terms, and his favorite combinators are B, C and I.})

    (seminar
     "mickens-dataflow"
     "Leveraging Fine-grained Data Flows in Web Applications"
     "James Mickens"
     "http://mickens.seas.harvard.edu/"
     "Harvard University"
     (datetime 2016 07 05 13 30)
     "WVH 366"
     @list{
       A modern web page contains megabytes of HTML, CSS, images, and JavaScript. Loading such a page requires a browser to evaluate a complex dependency graph involving those resources; once the page is loaded, subsequent interactions between those resources and the user can lead to tricky-to-diagnose bugs. In this talk, I'll describe how tracking fine-grained data flows can allow us to reduce page load times by prioritizing the loads of the highest ancestors in the data flow graph. I'll also describe initial work in using data flows to assist with time-travel debugging (in which developers use a logging-and-replay framework to analyze buggy program executions).}
     @list{James Mickens is an IEEE Knight of the Republic, an ACM Templar for Non-Open Access, and a Royal Proceeding of Her Majesty’s Royal Proceedings. His appreciation for syntactically correct code has led him to be called “a semicolon in human form.” His online shopping habits have too many dimensions to be k-means clustered, so he is only shown ads about dinosaurs and ancient siege machines. This does not bother James Mickens, and explains why he spends his summers attacking France with triceratops horns.})

    (seminar
     "mccarthy-runtime"
     "A Coq Library For Internal Verification of Running-Times"
     "Jay McCarthy"
     "https://jeapostrophe.github.io/home/"
     "U. Massachusetts, Lowell"
     (datetime 2016 05 17 13 30)
     "WVH 366"
     @list{We present a Coq library that lifts an abstract yet precise
notion of running-time into the type of a function. Our library is
based on a monad that counts abstract steps, controlled by one of the
monadic operations. The monad's computational content, however, is
simply that of the identity monad so programs written in our
monad (that recur on the natural structure of their arguments) extract
into idiomatic OCaml code. We evaluated the expressiveness of the
library by proving that red-black tree insertion and search, merge
sort, insertion sort, Fibonacci, iterated list insertion, BigNum
addition, and Okasaki's Braun Tree algorithms all have their expected
running times.}
     @list{Jay McCarthy is an associate professor at UMass Lowell in the Computer
Science Department. He is a member of the PLT research group and works
on the Racket programming language. He is passionate about computer
science education & diversity, formal verification, programming
language expressiveness, and his wonderful family.})

    (seminar
     "scherer-effects"
     "New language ideas for user-defined side-effects: algebraic effect handlers."
     "Gabriel Scherer"
     "http://gallium.inria.fr/~scherer/"
     "Northeastern University"
     (datetime 2016 05 18 11 45)
     "WVH 366"
     @list{This talk reports on some cool ideas I learned during a week-long
seminar in Dagstuhl last month; it will present other people's work,
not my own. It is aimed at an audience interested in programming
language design in general, but not familiar with the theoretical
treatment of pure languages and side-effects.

We shall start with *demos* first, explaining short code examples in
three programming languages: ML with exceptions as a warmup, then the
new language 'Eff' of Matija Pretnar and Andrej Bauer, that first
implemented so-called effect handlers, and finally the language
'Frank' of Conor McBride, that integrates these handlers in a more
uniform style of "effectful call-by-value programming".

Only second will we discuss some *theory*, in an accessible way:
monads and algebraic effects, which are two distinct ways to formalize
side-effects, and the difference between "direct" and "indirect" style
of effectful programming.

Underlying this talk are two larger, important questions of
programming language design, that will be touched during the talk and
we can discuss further afterwards:
- Do programming language need a facility for user-defined side-effects?
- When should we encode new design ideas as libraries/macros in an
  expressive language, and when should we design languages afresh for
  them?}
     @list{
Gabriel is interested in theoretical aspects of type systems,
programming language implementation, general programming language
concepts, and even some syntactic aspects. He has a preference for the
formalizable aspects, or formalizable approaches to programming
language aspects, rather than the often subjective appeal to taste or
intuition.})

    (seminar
     "consel-iot"
     "Internet of Things: From Small- to Large-Scale Orchestration"
     "Charles Consel"
     "http://phoenix.inria.fr/charles-consel"
     "Bordeaux Institute of Technology"
     (datetime 2016 05 02 11 00)
     "WVH 366"
     @list{The domain of Internet of Things (IoT) is rapidly expanding beyond research and becoming a major industrial market with such stakeholders as major manufacturers of chips and connected objects, and fast-growing operators of low-power wide-area networks. Importantly, this emerging domain is driven by applications that leverage the infrastructure to provide users with innovative, high-value services. Because of this application-centric approach, software development plays a key role to realize the full potential of IoT.

In this talk, we argue that there is a continuum between orchestrating connected objects in the small and in the large, fostering a unified approach to application development. We examine the requirements for orchestrating connected objects and address them with domain-specific design concepts. We then show how to map these design concepts into dedicated programming patterns and runtime mechanisms.

Our work revolves around domain-specific notations integrated into a tool-based design methodology, dedicated to develop IoT applications. We have applied our work across a spectrum of infrastructure sizes; we present examples, ranging from an automated pilot in avionics, to an assisted living platform for the home of seniors, to a parking management system in a smart city.}
     @list{Charles Consel is a professor of Computer Science at Bordeaux Institute of Technology. He served on the faculty of Yale University, Oregon Graduate Institute and the University of Rennes.

His research contributions cover programming languages, software engineering, operating systems, pervasive computing, and assistive computing.

He leads the Phoenix group at Inria that conducts multi-disciplinary research to design, develop, deploy and assess assistive computing support. This research combines (1) Cognitive Science to study user needs and make a rigorous assessment of assistive services; (2) Sensing and actuating expertise to support the user, based on accurate and rich interactions with the environment; (3) Computer Science to support and guide the development process of the assistive services.})

    (seminar
     "aiken-stoke"
     "STOKE: Search-Based Compiler Optimization"
     "Alex Aiken"
     "https://theory.stanford.edu/~aiken/"
     "Stanford University"
     (datetime 2016 04 15 11 00)
     "WVH 366"
     @list{The optimization component of a compiler takes a program as input and
produces another, hopefully faster, program as output.  At a high level,
optimizers solve a search problem over a space of programs, but the
traditional architecture of a compiler's optimizer does no search at
all.  This talk will present the STOKE project, which is exploring the
use of Monte Carlo search methods as the basis of a modern compiler
optimizer.  We will show that search-based program optimization can
consistently improve, sometimes substantially, on current production
compilers at their highest levels of optimization and can even compete
with expert, hand-written  assembly.  We will also discuss the unique
and challenging verification problems that arise when a compiler
produces code using a random process.}
     @list{Alex Aiken is the Alcatel-Lucent Professor and the current
Tencent Chair of the Computer Science Department at Stanford. Alex
received his Bachelors degree in Computer Science and Music from
Bowling Green State University in 1983 and his Ph.D. from Cornell
University in 1988. Alex was a Research Staff Member at the IBM
Almaden Research Center (1988-1993) and a Professor in the EECS
department at UC Berkeley (1993-2003) before joining the Stanford
faculty in 2003. His research interest is in areas related to
programming languages. He is an ACM Fellow, a recipient of Phi Beta
Kappa's Teaching Award, and a former National Young Investigator.})

    (seminar
     "rubin-mobile"
     "The Secret Life of Mobile Applications"
     "Julia Rubin"
     "https://people.csail.mit.edu/mjulia/"
     "MIT"
     (datetime 2016 04 05 10 30)
     "WVH 366"
     @list{As software becomes increasingly more complex and yet more pervasive, poor understanding of software behavior compromises the quality and the integrity of software systems that we use. In this talk, I will show that automated analysis techniques can help to identify and reason about software behavior characteristics that matter to humans. After a brief overview of my current research directions, I will focus on techniques for identifying privacy violations in mobile applications, i.e., leakages of sensitive information such as user location and shopping preferences. I will present a set of solutions that rely on contextual, functional and usage-based clues for improving the accuracy of leakage detection and for distinguishing between “legitimate” and “illegitimate” information distribution patterns.}
     @list{Julia Rubin is a Postdoctoral Researcher in the EECS department at MIT. Prior to that, she was a Research Staff Member and, part of the time, a manager at IBM Research in Haifa, Israel. She received her PhD in Computer Science from the University of Toronto, Canada in 2014. Julia’s research interests are in software engineering, program analysis and software security, focusing on improving the quality and the integrity of modern software systems. Her recent work in this area won an ACM Distinguished Paper Award at ASE, two Best Paper Awards, at SPLC and CSMR, and was nominated for Facebook’s Internet Defense Prize at the USENIX Security Symposium.})

    (seminar
     "devriese-capabilities"
     "Reasoning about Object Capabilities with Logical Relations and Effect Parametricity"
     "Dominique Devriese"
     "https://distrinet.cs.kuleuven.be/people/dominiqu"
     "Katholieke Universiteit Leuven"
     (datetime 2016 02 25 13 30)
     "WVH 366"
     @list{Object capabilities are a technique for fine-grained privilege separation in programming languages and systems,with important applications in security. However, current formal characterisations do not fully capture capability-safety of a programming language and are not sufficient for verifying typical applications. Using state-of-the-art techniques from programming languages research, we define a logical relation for a core calculus of JavaScript that better characterises capability-safety. The relation is powerful enough to reason about typical capability patterns and supports evolvable invariants on shared data structures, capabilities with restricted authority over them and isolated components with restricted communication channels. We use a novel notion of effect parametricity for deriving properties about effects. We demonstrate that our results imply memory access bounds that have previously been used to characterise capability-safety.}
     @list{Dominique is a postdoctoral researcher in the research group DistriNet, part of the Computer Science department of the Katholieke Universiteit Leuven. He holds a postdoctoral research fellowship of the Research Foundation - Flanders (FWO). He works on formalising properties of object-oriented and object-capability programming languages---specifically a property called effect parametricity--is the author of the grammar-combinators Haskell parsing library, and has added instance arguments to the programming language/proof assistant Agda. He is interested in information flow security, secure compilation,full abstraction, and functional and dependently typed programming languages.})

    (seminar
     "chandra-javascript"
     "JavaScript in the Small"
     "Satish Chandra"
     "https://sites.google.com/site/schandraacmorg/"
     "Samsung"
     (datetime 2016 02 22 11 00)
     "WVH 366"
     @list{Emerging consumer electronics products are running the same software platforms that power smartphones. This leads to the appealing idea that a uniform programming abstraction can be used for app development for a range of devices, from wearables to smartphones. In practice, however, devices wary in their hardware capabilities and this has an impact on app development. In this talk, I will focus on ways in which JavaScript may be used and/or run differently to accommodate the different hardware capabilities across devices. Specifically, I will present a subset of JavaScript that strikes a balance between retaining the flavor of JavaScript (e.g. use of prototype inheritance, no explicit type declarations) and the possibility of ahead-of-time compilation to efficient code.}
     @list{
           Satish Chandra obtained a PhD from the University of Wisconsin-Madison in 1997, and a B.Tech from the Indian Institute of Technology-Kanpur in 1991, both in computer science. From 1997 to 2002, he was a member of technical staff at Bell Laboratories, where his research focused on program analysis, domain-specific languages, and data-communication protocols. From 2002 to 2013, he was a research staff member at IBM Research, where his research focused on bug finding and verification, software synthesis, and test automation. He joined Samsung Electronics in 2013, where he leads the advanced programming tools research team. He is an ACM Distinguished Scientist.})

    (seminar
     "ryu-bugs"
     "Journey to Find Bugs in Real-World Web Applications in the Wild"
     "Sukyoung Ryu"
     "http://plrg.kaist.ac.kr/ryu"
     "Korea Advanced Institute of Science and Technology"
     (datetime 2016 02 19 11 45)
     "WVH 366"
     @list{Analyzing real-world web applications is a challenging task. On top of understanding the semantics of JavaScript, it requires modeling of web documents, platform objects, and interactions between them. Most web applications load JavaScript code dynamically, which makes pure static analysis approaches inapplicable. In this talk, we present our journey to analyze JavaScript web applications in the wild using various approaches. From pure JavaScript programs to JavaScript web applications using platform-specific libraries, we explain technical challenges in analyzing each of them and how we built an open-source analysis framework for JavaScript, SAFE, that addresses the challenges incrementally.  Finally, we discuss our ongoing work on analysis of Android hybrid applications.}
     @list{})

    (seminar
     "lahav-consistency"
     "Taming release-acquire consistency"
     "Ori Lahav"
     "https://www.mpi-sws.org/~orilahav/"
     "Max Planck Institute for Software Systems"
     (datetime 2016 01 27 11 45)
     "WVH 366"
     @list{@p{Multiprocessors and concurrent programming are now pervasive. Typically, they do not guarantee sequential consistency (a.k.a. interleaving semantics), which is the standard assumption by most work on semantics and verification. Instead, they employ subtle memory models, exposing unexpected relaxed behaviors arising from hardware and compiler optimizations.}
                              @p{In this talk, I will focus on one such model --- the release-acquire fragment of the C/C++11 memory model. I will describe its merits, and show how it can be further improved, without additional implementation costs, to: (i) forbid dubious behaviors that are not observed in any implementation; (ii) support fence instructions that restore sequential consistency; and (iii) admit an equivalent intuitive operational semantics.}
                              @p{The talk is based on a joint work with Nick Giannarakis and Viktor Vafeiadis, to be presented in POPL'16.}
                              }
     @list{Ori Lahav is a postdoctoral researcher at MPI-SWS. He obtained his PhD from Tel Aviv University in the area of non-classical logics. His current research interests are in programming languages generally, with specific focus on memory models, concurrency, verification, and logic.})

    (seminar
     "patrignani-pma"
     "Secure Compilation to Protected Module Architectures"
     "Marco Patrignani"
     "http://www.mpi-sws.org/~marcopat/"
     "Max Planck Institute for Software Systems"
     (datetime 2016 01 25 11 00)
     "WVH 366"
     @list{This talk will informally describe protected module architectures (PMA), a security architecture that provides an assembly-level memory isolation mechanism. 	Then it will describe how to devise a secure (fully-abstract) compiler for an object-oriented language to PMA.  Finally, it will present how to prove the said compiler to be secure and discuss open, related research trajectories.}
     @list{Marco Patrignani did his bachelor and masters study in Bologna, then he obtained a Ph.D. in computer science from the KU Leuven, Belgium. There, with his advisors Dave Clarke and Frank Piessens, he studied secure (fully-abstract) compilation for Intel-SGX like architectures, i.e., untyped assembly languages extended with a memory isolation mechanism. Additionally, he investigated trace-based characterisation of the behaviour of those architectures. He is now a post-doc at MPI-SWS, Germany working on more secure-compilation-related topics with Deepak Garg.})

    (seminar
     "vafeiadis-weakmemory"
     "Program verification under weak memory consistency"
     "Viktor Vafeiadis"
     "http://www.mpi-sws.org/~viktor/"
     "Max Planck Institute for Software Systems"
     (datetime 2016 01 14 13 30)
     "WVH 366"
     @list{@p{
           Weak memory models formalize the inconsistent behaviors that one can observe in multithreaded programs running on modern hardware. In so doing, they complicate the already-difficult task of reasoning about correctness of concurrent code. Worse, they render impotent most formal methods that have been developed to tame concurrency, which almost universally assume a strong (i.e., sequentially consistent) memory model. In response, we have developed a number of alternative reasoning techniques that are sound for programs running weak memory consistency. I will cover both program logics, such as relaxed separation logic, as well as theorems that allow reducing reasoning about well-structured weakly consistent implementations down to sequential consistency, and show how these can be applied to reason about a practical RCU implementation.}}
     @list{
           Viktor Vafeiadis is a tenure-track researcher at MPI-SWS. He received his BA (2004) and PhD (2008) from the University of Cambridge. Before joining MPI-SWS in October 2010, he was a postdoc at Microsoft Research and at the University of Cambridge. He is interested in programming languages and verification with a focus program logics for weak memory, program logics for concurrency, compiler verifications, automated verification of concurrent programs, and interactive theorem proving.
     })

    (seminar
     "nagarakatte-llvm"
     "Lightweight Formal Methods for LLVM Verification"
     "Santosh Nagarakatte"
     "http://www.cs.rutgers.edu/~santosh.nagarakatte/"
     "Rutgers University"
     (datetime 2016 01 13 12 00)
     "WVH 366"
     @list{@p{Compilers form an integral component of the software
development ecosystem. Compiler writers must understand the
specification of source and target languages to design sophisticated
algorithms that transform programs while preserving
semantics. Unfortunately, compiler bugs in mainstream compilers are
common. Compiler bugs can manifest as crashes during compilation, or,
much worse, result in the silent generation of incorrect
programs. Such mis-compilations can introduce subtle errors that are
difficult to diagnose and generally puzzling to software
developers.}
@p{The talk will describe the problems in developing peephole
optimizations that perform local rewriting to improve the efficiency
of LLVM code. These optimizations are individually difficult to get
right, particularly in the presence of undefined behavior; taken
together they represent a persistent source of bugs.  The talk will
present Alive, a domain-specific language for writing optimizations
and for automatically either proving them correct or else generating
counterexamples.  A transformation in Alive is shown to be correct
automatically by encoding the transformation into constraints, which
are automatically checked for validity using a Satisfiability Modulo
Theory (SMT) solver. Furthermore, Alive can be automatically
translated into C++ code that is suitable for inclusion in an LLVM
optimization pass.}
@p{Alive is based on an attempt to balance usability and formal
methods; for example, it captures—but largely hides—the detailed
semantics of three different kinds of undefined behavior in LLVM. We
have translated more than 300 LLVM optimizations into Alive and, in
the process, found that eight of them were wrong.  I will conclude the
talk highlighting the lessons learned and the challenges in
incorporating lightweight formal methods in the tool-chain of the
compiler developer.}}
     @list{Santosh Nagarakatte is an Assistant Professor of Computer Science at Rutgers University. He obtained his PhD from the University of Pennsylvania. His research interests are in Hardware-Software Interfaces spanning Programming Languages, Compilers, Software Engineering, and Computer Architecture. His papers have been selected as IEEE MICRO TOP Picks papers of computer architecture conferences in 2010 and 2013. He has received the NSF CAREER Award in 2015, PLDI 2015 Distinguished Paper Award, and the Google Faculty Research Award in 2014 for his research on incorporating lightweight formal methods for LLVM compiler verification.})

     (seminar
      "andersen-profiling"
      "Feature Specific Profiling for R"
      "Leif Andersen"
      "http://leifandersen.net"
      "Northeastern University"
      (datetime 2015 12 2 12 00)
      "WVH 366"
      @list{@p{
          Programmers use profilers to understand the performance
          characteristics of
          their programs and to focus on those pieces whose improvement may yield the
          largest gains. A conventional profiler measures the time that a program
          spends in functions, methods, expressions, and statements. Racket's novel
          feature-specific profiler supplements this information with timings of
          instances of linguistic features. This paper reports the results of a
          successful reproducibility effort to adapt feature-specific profiling to
          the R programming language. Specifically, the paper demonstrates how easy
          and effective it is to add the necessary language support, that the
          approach usefully enhances the information produced by a classical
          profiler, and that the additional overhead is tolerable.}}
      @list{Leif Andersen is a second year Ph.D. student at
Northeastern University, studying programming language with Matthias
Felleisen.})

     (seminar
      "berger-performance"
      "Performance Matters"
      "Emery Berger"
      "http://emeryberger.com/"
      "U. Massachusetts, Amherst"
      (datetime 2015 11 20 10 30)
      "WVH 366"
      @list{
        @p{
            Performance clearly matters to users. The most common software update
            on the AppStore *by far* is "Bug fixes and performance enhancements."
            Now that Moore's Law Free Lunch has ended, programmers have to work
            hard to get high performance for their applications. But why is
            performance so hard to deliver?}
        @p{ I will first explain why our current approaches to evaluating and
            optimizing performance don't work, especially on modern hardware and
            for modern applications. I will then present two systems that address
            these challenges. Stabilizer is a tool that enables statistically
            sound performance evaluation, making it possible to understand the
            impact of optimizations and conclude things like the fact that the -O2
            and -O3 optimization levels are indistinguishable from noise
            (unfortunately true).}
        @p{ Since compiler optimizations have largely run out of steam, we need
            better profiling support, especially for modern concurrent,
            multi-threaded applications. Coz is a novel "causal profiler" that
            lets programmers optimize for throughput or latency, and which
            pinpoints and accurately predicts the impact of optimizations. Coz's
            approach unlocks numerous previously unknown optimization
            opportunities. Guided by Coz, we improved the performance of Memcached
            by 9%, SQLite by 25%, and accelerated six Parsec applications by as
            much as 68%; in most cases, these optimizations involved modifying
            under 10 lines of code.}
        @p{ This talk is based on work with Charlie Curtsinger published at ASPLOS
            2013 (Stabilizer) and SOSP 2015 (Coz), where it received a Best Paper
            Award.
            }}
      @list{
                     @p{Emery Berger is a Professor in the College of Information and Computer
                     Sciences at the University of Massachusetts Amherst, the flagship
                     campus of the UMass system. He graduated with a Ph.D. in Computer
                     Science from the University of Texas at Austin in 2002. Professor
                     Berger has been a Visiting Scientist at Microsoft Research (7 times)
                     and at the Universitat Politecnica de Catalunya (UPC) / Barcelona
                     Supercomputing Center (BSC).}
                     @p{Professor Berger’s research spans programming languages, runtime
                     systems, and operating systems, with a particular focus on systems
                     that transparently improve reliability, security, and performance. He
                     is the creator of a number of influential software systems including
                     Hoard, a fast and scalable memory manager that accelerates
                     multithreaded applications (used by companies including British
                                                      Telecom, Cisco, Crédit Suisse, Reuters, Royal Bank of Canada, SAP, and
                                                      Tata, and on which the Mac OS X memory manager is based); DieHard, an
                     error-avoiding memory manager that directly influenced the design of
                     the Windows 7 Fault-Tolerant Heap; and DieHarder, a secure memory
                     manager that was an inspiration for hardening changes made to the
                     Windows 8 heap.}
                     @p{His honors include a Microsoft Research Fellowship, an NSF CAREER
                     Award, a Lilly Teaching Fellowship, a Most Influential Paper Award at
                     OOPSLA 2012, a Google Research Award, a Microsoft SEIF Award, a Best
                     Artifact Award at PLDI, and Best Paper Awards at FAST, OOPSLA, and
                     SOSP; he was named an ACM Senior Member in 2010. Professor Berger is
                     currently a Member of the SIGPLAN Executive Committee and an Associate
                     Editor of the ACM Transactions on Programming Languages and Systems,
                     and will serve as Program Chair for PLDI 2016.}
                     })

     (seminar
      "guha-networks"
      "Programming Languages Meets Programmable Networks"
      "Arjun Guha"
      "https://people.cs.umass.edu/~arjun/home/"
      "U. Massachusetts, Amherst"
      (datetime 2015 11 16 13 00)
      "WVH 366"
      @list{
            @p{Computer networks do not simply connect machines together, but run several
                        applications on network devices, such as load balancers, intrusion detection
                        systems, authentication portals, and more. Historically, these applications were
                        black-boxes running on proprietary hardware, but software-defined networking
                        (SDN) now allows anyone to write their own programs using open networking
                        protocols (e.g., OpenFlow). So, what are the right abstractions for programming networks? This talk will try
                        to address this question in three ways.}
                        @p{First, we present a syntactic theory of network forwarding called NetKAT, which supports equational reasoning about network-wide behavior. Using NetKAT, programmers can ask and answer questions like, "Can A communicate with B?",
                        "Does all traffic traverse my intrusion detection system?", "Is there a loop in
my network?", and so on.}
@p{Second, we present a fast and efficient compiler for NetKAT. Although several
network compilers already exist, they are unusable on even moderately sized
networks. Using new data structures and compilation algorithms, our new compiler
is two orders of magnitudes faster than prior work and scales to large
datacenter networks.}
@p{Finally, we consider the problem of building a reliable runtime system for
NetKAT. NetKAT abstracts away several low-level details of networking hardware.
Although this is a boon for the network programmer, the burden now shifts to us
to engineer abstractions correctly. We present a Coq-certified runtime system
that is proven correct with respect to a detailed operational model software-
defined networks.
}}
      @list{
            Arjun Guha is an assistant professor of Computer Science at UMass Amherst. He
                  enjoys tackling problems in systems using the tools and principles of
                  programming languages. Apart from network programming, he has worked on Web
                  security and system configuration languages. He received a PhD in Computer
                  Science from Brown University in 2012 and a BA in Computer Science from Grinnell
                  College in 2006.})

     (seminar
      "jagannathan-consistency"
      "Declarative Programming for Eventual Consistency"
      "Suresh Jagannathan"
      "https://www.cs.purdue.edu/homes/suresh/"
      "Purdue University"
      (datetime 2015 11 13 10 30)
      "WVH 366"
      @list{
            @p{In geo-replicated distributed data stores, the need to ensure responsiveness
                  in the face of network partitions and processor failures results in
                  implementations that provide only weak (so-called eventually consistent)
                  guarantees on when data updated by one process becomes visible to another.
                  Applications must be carefully constructed to be aware of unwanted
                  inconsistencies permitted by such implementations (e.g., having negative
                                                                           balances in a bank account, or having an item appear in a shopping cart
                                                                           after it has been removed), but must balance correctness concerns with
                                                                           performance and scalability needs.  Because understanding these tradeoffs
                                                                           requires subtle reasoning and detailed knowledge about the underlying data
                                                                           store, implementing robust distributed applications in such environments is
                                                                           often an error-prone and expensive task.}

                                                                           @p{To overcome these issues, this talk presents a declarative programming model
                                                                           for eventually consistent data stores called Quelea.  The model comprises a
                                                                           contract language, capable of defining fine-grained application-level
                                                                           consistency properties for replicated data types (and transactions over
                                                                                                                                 objects of these types), and a contract enforcement system to analyze
                                                                                                                                 contracts and automatically generate the appropriate consistency protocol
                                                                                                                                 for the method protected by the contract.  By doing so, Quelea enables
                                                                                                                                 programmers to reason compositionally about consistency from the perspective
                                                                                                                                 of high-level application requirements, not low-level implementation
                                                                                                                                 features.}

                                                                                                                                 @p{This is joint work with Gowtham Kaki and K.C. Sivaramakrishnan.}}
      @list{
            @p{Suresh Jagannathan is a Professor of Computer Science at Purdue University
                      where he has been on leave since September 2013, serving as a program
                      manager in the Information Innovation Office at DARPA.  He has also been a
                      visiting faculty at Cambridge University, where he spent a sabbatical year
                      in 2010; and, prior to joining Purdue, was a senior research scientist at
                      the NEC Research Institute in Princeton, N.J.  He received his Ph.D from
                      MIT.}

                      @p{His research interests are in programming languages generally, with specific
                      focus on compilers, functional programming, program verification, and
                      concurrent and distributed systems.  At DARPA, he manages programs on
                      probabilistic programming and machine learning (PPAML), program synthesis
                      and repair leveraging predictive analytics over large software corpora
                      (MUSE), and self-adaptive software through resource-aware analyses,
                      runtimes, and architectures (BRASS).}})

     (seminar
      "serrano-hop"
      "Hop.js: multitier programming in JavaScript"
      "Manuel Serrano"
      "http://www-sop.inria.fr/members/Manuel.Serrano/"
      "INRIA"
      (datetime 2015 11 3 10 30)
      "WVH 366"
      @list{
            Hop.js is a multitier extension of JavaScript. It allows a single
                   JavaScript program to describe the client-side and the server-side
                   components of a Web application. Its runtime environment ensures a
                   consistent execution of the application on the server and on the
                   client. This talk will shows its minimal set of JavaScript extensions
                   that makes Web programming easier. It will present its runtime
                   environment, with an emphasize on the handling of server-side
                   parallelism.}
      @list{
            Manuel is a researcher at INRIA Sophia Antipolis, he used to work on Scheme.})

     (seminar
       "kirsch-scalloc"
       "Scalloc and Selfie: Fast Memory Allocation and Self-referential Systems Software"
       "Christoph Kirsch"
       "http://cs.uni-salzburg.at/~ck/"
       "University of Salzburg"
       (datetime 2015 11 2 11 00)
       "WVH 366"
       @list{ @span{
         This talk is about scalloc, a fast, multicore-scalable, low-fragmentation memory allocator and selfie, a 4000-line implementation of a tiny self-compiling C compiler and a tiny self-executing MIPS emulator for teaching systems engineering. Scalloc is a typical example of a very complex, multi-year research effort while selfie is, at least for now, a purely educational, many-year effort in teaching compiler, operating system, and virtual machine design based on a single, highly principled software platform. So far scalloc and selfie only share the passion of their authors and are otherwise two distinct projects. Yet earlier versions of selfie, before they were even identified as such, were instrumental in bringing up the generation of students who did scalloc.
        The main ideas behind scalloc are: uniform treatment of small and big objects through so-called virtual spans, efficiently and effectively reclaiming free memory through fast and scalable global data structures, and constant-time (modulo synchronization) allocation and deallocation operations that trade off memory reuse and spatial locality without being subject to false sharing. The main ideas behind selfie are: a compiler written in and for a tiny subset of C called C* which uses the dereferencing * operator of C for memory access but lacks data structures and many other features and a MIPS emulator written in C* that can execute itself. Both are combined and extended by students to do very cool stuff.
              }}
       @list{ @span{
        Christoph Kirsch is Professor at the University of Salzburg, Austria. From
        1999 to 2004 he worked as Postdoctoral Researcher at UC, Berkeley. He later
        returned to Berkeley as Visiting Scholar (2008-2013) and Visiting Professor
        (2014) as part of a collaborative research effort in Cyber-Physical
        Systems. His most recent research interests are in concurrent data
        structures, memory management, and so-called spatial programming. Dr. Kirsch
        co-invented embedded programming languages and systems such as Giotto, HTL,
        and the Embedded Machine, and more recently co-designed high-performance,
        multicore-scalable concurrent data structures and memory management
        systems. He co-founded the International Conference on Embedded Software
        (EMSOFT) and served as ACM SIGBED chair from 2011 until 2013 and ACM TODAES
        associate editor from 2011 until 2014.
        }})

     (seminar
       "garnock-jones-syndicate"
       "Coordinated Concurrent Programming in Syndicate"
       "Tony Garnock-Jones"
       "http://www.ccs.neu.edu/home/tonyg/"
       "Northeastern University"
       (datetime 2015 10 28 12 00)
       "WVH 366"
       @list{
              @span{Most programs interact with the world: via graphical
user interfaces, networks, etc. This form of interactivity entails
concurrency, and concurrent program components must coordinate their
computations. This talk will present Syndicate, a design for a coordinated,
concurrent programming language. Each concurrent component in Syndicate is a
functional actor that participates in scoped conversations. The medium of
conversation arranges for message exchanges and coordinates access to common
knowledge. As such, Syndicate occupies a point in design space halfway
between actors and threads.
       }}
       @list{ @span{Tony is a graduate student working with Matthias.}
       })

     (seminar
       "payer-corruption"
       "Memory corruption: why protection is hard"
       "Mathias Payer"
       "https://nebelwelt.net"
       "Purdue University"
       (datetime 2015 10 23 13 30)
       "WVH 366"
       @list{
              @span{
  Memory corruption plagues systems since the dawn of computing. With the
rise of defense techniques like stack cookies, ASLR, and DEP, attacks
have become much more complicated, yet control-flow hijack attacks are
still prevalent. Attacks leverage code reuse attacks, often using some
form of information disclosure. Stronger defense mechanisms have been
proposed but none have seen wide deployment so far (i) due to the time
it takes to deploy a security mechanism, (ii) incompatibility with
specific features, and (iii) most severely due to performance overhead.
In the talk, we evaluate the security benefits and limitations of the
status quo and look into upcoming defense mechanisms (and their attacks).

Control-Flow Integrity (CFI) and Code-Pointer Integrity (CPI) are two of
the hottest upcoming defense mechanisms. CFI guarantees that the runtime
control flow follows the statically determined control-flow graph. An
attacker may reuse any of the valid transitions at any control flow
transfer. CPI on the other hand is a dynamic property that enforces
memory safety guarantees like bounds checks for code pointers by
separating code pointers from regular data. We will discuss differences
and advantages/disadvantages of both approaches, especially the security
benefits they give under novel attacks like Counterfeit Object-Oriented
Programming (COOP) and Control-Flow Bending (CFB).  COOP reuses complete
functions as gadgets and CFB bends the control flow along valid but
unintended paths in the control-flow graph of a program.

              }
       }
       @list{
              @span{
Mathias Payer is a security researcher and an assistant professor in
computer science at Purdue university. His interests are related to
system security, binary exploitation, user-space software-based fault
isolation, binary translation/recompilation, and (application)
virtualization. His research focuses on protecting applications even in
the presence of vulnerabilities, with a focus on memory corruption.
Before joining Purdue in 2014 he spent two years as PostDoc in Dawn
Song's BitBlaze group at UC Berkeley. He graduated from ETH Zurich with
a Dr. sc. ETH in 2012.
         }
       })

     (seminar
       "vitek-llvm"
       "Using LLVM as a backend for R"
       "Jan Vitek"
       "http://janvitek.org"
       "Northeastern University"
       (datetime 2015 10 2 12 00)
       "WVH 366"
       @list{@span{
        I will provide an update on the status of the Reactor project which aims to use LLVM as just-in-time compiler for the R language.  I will discuss early challenges such as integration in the environment and garbage collection support.
       }}
       @list{@span{
        Jan Vitek is a Professor at Northeastern, he works on programming language design and implementation.
       }})

     (seminar
       "ushey-rstudio"
       "Completions and Diagnostics in RStudio"
       "Kevin Ushey"
       "https://kevinushey.github.io/"
       "RStudio"
       (datetime 2015 10 2 11 00)
       "WVH 366"
       @list{@span{
        Kevin will discuss the details behind the implementation of completions +
        diagnostics, as well as some future goals re: enabling user extensibility of
        both the autocompletion and diagnostics systems.
       }}
       @list{@span{
        Kevin is a software engineer at RStudio. He graduated from the University of British Columbia with an MSc in Statistics, with a thesis focusing on the use of non-linear mixed effects models in the analysis of yeast growth curves in gene knockout studies. Shortly thereafter, he worked doing data analysis at St. Paul's hospital in Vancouver as part of the Daley lab, and later at the Fred Hutchinson Cancer Research Center in Seattle as part of the Gottardo lab. At RStudio, Kevin primarily works on the RStudio IDE, but also maintains the R package packrat.
       }})

     (seminar
       "tierney-rengine"
       "Some New Developments for the R Engine"
       "Luke Tierney"
       "http://homepage.stat.uiowa.edu/~luke/"
       "University of Iowa"
       (datetime 2015 10 2 10 00)
       "WVH 366"
       @list{@span{
              R is a dynamic language for statistical
        computing and graphics. In recent years R has become a major framework
        for both statistical practice and research. This talk present a very
        brief outline of the R language and its evolution and describe some
        current efforts on improvements to the core computational engine,
        including work on compilation of R code, efforts to take advantage of
        multiple processor cores, and modifications to support working with
        larger data sets.
       }}
       @list{@span{
        Luke Tierney is a Professor Statistics at the University of Iowa and one of the key contributors to the R project.
       }})

     (seminar
       "summers-permissions"
       "Verification Infrastructure for Permission-based Reasoning"
       "Alex Summers"
       "http://people.inf.ethz.ch/summersa/wiki/"
       "ETH Zürich"
       (datetime 2015 6 26 13 45)
       "WVH 366"
       @list{@span{
              Modern verification techniques are becoming
        ever-more powerful and sophisticated, and building tools to implement
        them is a time-consuming and difficult task. Writing a new verifier to
        validate each on-paper approach is impractical; for this reason
        intermediate verification languages such as Boogie and Why3 have become
        popular over the last decade. However, verification approaches geared
        around complex program logics (such as separation logic) have typically
        been implemented in specialised tools, since the reasoning is hard to
        map down to first-order automated reasoning. In practice, this means
        that a rich variety of modern techniques have no corresponding tool
        support.  In this talk, I will present the new Silver intermediate
        verification language, which has been designed to facilitate the
        lightweight implementation of a variety of modern methodologies for
        program verification. In contrast to lower-level verification languages,
        Silver provides native support for heap reasoning; modes of reasoning
        such as concurrent separation logic, dynamic frames and
        rely-guarantee/invariants can be simply encoded.  Silver has been
        developed as part of the Viper project, which provides two automated
        back-end verifiers for Silver programs. Since releasing our software in
        September last year, it has been used for (internal and external)
        projects to build tools for Java verification, non-blocking concurrency
        reasoning, flow-sensitive typing and reasoning about GPU and Linux
        kernel code.
       }}
       @list{@span{
        Alex Summers obtained his PhD from Imperial College
        London in 2009, in the area of type systems and
        classical logics. Since then he has worked in a
        variety of areas concerning software correctness and
        verification, at Imperial College London and ETH
        Zurich. His research interests include developing
        specification techniques for different (usually
        concurrent) programming paradigms, and implementing
        these in automatic verification tools. He was
        recently awarded the 2015 AITO Dahl-Nygaard Junior
        Prize for his work on type systems and the
        verification of object-oriented programs.
       }})

     (seminar
       "samanta-tracesets"
       "Concurrent Trace Sets for Synchronization Synthesis"
       "Roopsha Samanta"
       "http://pub.ist.ac.at/~rsamanta/"
       "IST Austria"
       (datetime 2015 6 26 13 30)
       "WVH 366"
       @list{@span{
              In this talk, I will first present a method and
        a tool TARA for generating succinct representations of sets of
        concurrent traces. In our work, we focus on trace sets that contain all
        correct or all incorrect permutations of events from a given trace. We
        represent such trace sets as Boolean combinations of happens-before
        ordering constraints between events.  Our trace set representations can
        drive diverse verification, fault localization, repair, and synthesis
        techniques for concurrent programs. In the remainder of the talk, I will
        focus on the use of our representation for synchronization synthesis.
        This work appears in POPL 2015 and CAV 2015, and is joint work with
        Pavol Cerny, Ed Clarke, Ashutosh Gupta, Tom Henzinger, Arjun
        Radhakrishna, Leonid Ryzhyk and Thorsten Tarrach.
       }}
       @list{@span{
       }})

))

@doctype{html}
@html[lang: "en"]{
  @header{Seminars}
  @body[id: "pn-top"]{
   @navbar{Seminars}
   @subpage-title{Seminars}

   @div[class: "pn-main-wrapper"]{
     @div[class: "content"]{
       @div[class: "container"]{
         @div[class: "row"]{
           @div[class: "col-md-12"]{
             @h2{Programming Language Seminar}
             @p{
              The PL seminar meets in WVH
              [@a[href: "http://www.ccs.neu.edu/home/wand/directions.html"]{directions}]. The @a[href: "http://lists.ccs.neu.edu/pipermail/pl-seminar"]{mailing
                list} is public. An @a[href: "https://calendar.google.com/calendar/embed?src=k4cg1vgb3l2n8r2ph4t01dmtpc@group.calendar.google.com&ctz=America/New_York"]{HTML calendar}
                and an @a[href: "https://calendar.google.com/calendar/ical/k4cg1vgb3l2n8r2ph4t01dmtpc%40group.calendar.google.com/public/basic.ics"]{ICAL calendar}
              are available for your convenience.}}}

         @br{}

        @div[class: "row"]{
          @|seminars|
        }

        @div[class: "pn-separator-img"]

        @div[class: "container"]{
          @div[class: "row"]{
            @div[class: "col-md-12"]{
              @h2{Programming Language Seminar, Junior}
              @p{The junior PL seminar (@a[href: "http://lists.ccs.neu.edu/pipermail/pl-sem-jr"]{mailing list}) (@a[href: "http://www.ccs.neu.edu/home/stamourv/pljr.html"]{home page}): a regular student-only seminar, suitable for those new to the study of programming languages.}
              @br{}
              @p{The PL Seminar, Jr. is a seminar where students new to the area can discuss the study of programming languages. This seminar is not intended to replace the main PL Seminar, but we hope that it can augment that by providing a place where junior students can discuss topics and ask questions at a pace better suited to our level of knowledge. Topics include:
                @ul{
                  @li{Design and analysis of programming languages: syntax, semantics, pragmatics (ie, how do you use a language feature in real programs?)}
                  @li{Implementation of programming languages}
                  @li{Program development, both large and small}
                  @li{Programming pedagogy}
                  @li{Programming tools and environments}
                  @li{and anything else that catches our interest.}
                }
                Topics are not limited to current research, but include older papers, textbook chapters, and surveys.
              }
              @br{}
              @h4{Why PL Seminar, Jr.?}
              @small{
                @p{In many ways, the existing PL Seminar doesn't serve junior students well. For us, it is intended to provide an environment where we can go to listen to current research. While nobody expects us to understand the entire presentation, we would ideally be able to say, "I didn't understand that, but it sounded really neat." By and large, that hasn't happened, for a number of reasons:
                  @ul{
                    @li{For many of us, once our level of comprehension drops below a certain threshold, we tend to get frustrated and tune out. It's very difficult to say that something sounds interesting when you don't even feel like you understand the language.}
                    @li{The discussion environment isn't really helpful to junior students. The questions from the audience are typically research-oriented; often, a great deal of background knowledge is required to follow the ensuing discussions.}
                    @li{To overuse a metaphor, the bandwidth available for discussion tends to be filled by a relatively small number of people; it's often difficult for a junior student to get a word in edgewise.}
                  }}}
              @br{}
              @h4{Seminar Culture}

              @small{@p{For this seminar to be as useful as possible, it is important we establish the right "seminar culture." This is, of course, something that is hard to describe, and it will shift and evolve. However, there are some guidelines for what we are trying to accomplish:
                @ul{
                  @li{Attendees are willing to learn and to help others learn. They should not come in order to demonstrate how much smarter they are than everyone else just because they know so much more about a specific topic.}
                  @li{Active listening is encouraged. Ask questions! In particular, while research questions are of course welcome, basic comprehension questions are especially encouraged. If you don't understand something, or if you don't believe a step in a proof, ask!}
                  @li{An important corollary: no question is too stupid to ask. This places an obligation on other members of the seminar: rather than express frustration at the basic nature of a question, look at it as an opportunity to practice your skills in explaining things.}
                  @li{Of course, while active listening is a good thing, we want to avoid having a few people fill the communications channel. So, please be considerate of the other people present and allow them to join the discussions as well.}
                }
                @p{Any attendee may propose a topic; we'll solicit volunteers to present a survey of the area. We expect that most of the presenters will be junior PL students, although outside presentations are welcome.}}}
    }}}}}}
    @footer{}
}}
