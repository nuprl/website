#lang scribble/html
@(require
   racket/match
   racket/date
   gregor
   "templates.rkt")

@;; The seminars are sorted every time this page is built, to separate
@;; future seminars from past seminars and order them differently.

@(define-struct seminar (anchor title speaker link aff date room abstract bio))

@(define (in-future? sem)
   (datetime<? (now #:tz "America/New_York") (seminar-date sem)))

@(define id 0)
@(define (render-seminar sem)
   (set! id (add1 id))
   (define finished (if (in-future? sem) "" "finished"))
   (match-define (seminar anchor title speaker link aff date room abstract bio) sem)
   @list[
     @div[id: @format["seminar-~a" id] class: @format["col-md-12 pn-seminar ~a" finished]]{
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
         @p[class: "pn-title-2"]{Abstract}
         @|abstract|
         @p[class: "pn-title-2"]{Bio}
         @|bio|}}
     @br[]
     "\n\n"])

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

@; TODO: Have seminars contain a datetime range, rather than just a start time.
@(define seminars
   (list
    ;; (seminar
    ;;  "IDENTIFIER"
    ;;  "TITLE"
    ;;  "AUTHOR"
    ;;  "WEBSITE"
    ;;  "INSTITUTION"
    ;;  (datetime 2018 9 19 10 00)
    ;;  "WVH 366"
    ;;  @list{@p{ABSTRACT}}
    ;;  @list{@p{BIO}})

    (seminar
     "maodeferro-pekoe"
     "Safe programming of IoT devices"
     "Carlos Mão de Ferro"
     "https://github.com/cajomferro/"
     "Faculdade de Ciências da Universidade de Lisboa // UMass Boston"
     (datetime 2019 5 29 10 00)
     "WVH 366"
     @list{@p{The Internet of Things (IoT) has the potential for being the next industrial revolution, envisioning billions of embedded devices. Because of their limited resources, IoT devices present specific challenges to their programming and deployment. For instance, limited battery lifetime imposes extreme energy-savings efforts, and low RAM and CPU processing power not only restrict computation power but can also lead to unexpected runtime errors (e.g, out of memory).}@p{C and C++ languages are often preferred when programming these devices. These languages deliver good performance but are notoriously unsafe and ill-suitable for proving high-level properties specific to HW usage (e.g., sending a message only when Wi-Fi module is connected). Invalid program behaviors can lead to silent errors and energy wasting, resulting in the high-cost of reprogramming thousands of devices.}@p{In this presentation, I will introduce Pekoe, a language for programming IoT devices that can determine, at compile time, the correct usage of hardware components. Pekoe introduces Harel Statecharts as types to model the hardware behavior. The compiler checks every possible execution trace of a program against the hardware model (the type) thus ensuring the correct order of system calls (e.g., sending a message via Wi-Fi, or suspending the CPU).}}
     @list{@p{Carlos Mão de Ferro is a visiting Fulbright scholar at UMass Boston (February to July 2019), and is a PhD candidate in Computer Science, University of Lisbon, Portugal.}@p{His research focuses on new programming languages techniques that deliver a safe and resource-aware programming of embedded devices, thus promoting a reliable and sustainable Internet of Things (IoT) environment.}@p{Carlos has been working on WSNs and embedded software for 10 years. After his master thesis conclusion in 2013, he alternated between the academia and the industry worlds. Carlos developed a smart irrigation system for the Green by Web startup that won the Smart Open Lisboa Startup contest in 2017.}@p{In general, Carlos is curious about Science and driven by the desire to understand how things work and how discoveries and inventions come to see the light of day. He enjoys reading topics about Physics, Astronomy and Biology. He is also passionate about music, having learned to play the flute and the piano as a child.}@p{During his stay at UMB, Carlos will be particularly interested in foster collaborations with researchers from IoT-related areas.}})
    
    
    (seminar
     "pfenning-message-passing-concurrency"
     "A rehabilitation of message-passing concurrency"
     "Frank Pfenning"
     "http://www.cs.cmu.edu/~fp/index.html"
     "Carnegie Mellon University"
     (datetime 2019 5 2 10 00)
     "WVH 366"
     @list{@p{Recently, there has been a lot of research on shared-memory concurrency. Nevertheless, programmers are discouraged from using it because of the difficulty of writing clear, correct programs. This is embodied for example in the Go Language slogan “Do not communicate by sharing memory; instead, share memory by communicating.” But do we have the right abstractions for message-passing concurrent programming? I argue that we do not (yet!) because concurrency constructs are usually bolted on to an existing language with an entirely different semantic foundation. I will review some recent progress on designing better abstractions based on strong logical and type-theoretic principles. Multiple themes from functional and object-oriented programming will re-emerge from these foundations in a new form, including (ha!) shared memory.}}
     @list{@p{Frank Pfenning studied Mathematics and Computer Science at the Technical University Darmstadt and then left for Carnegie Mellon University on a Fulbright scholarship where he obtained his Ph.D. in Mathematics in 1987 under the supervision of Professor Peter Andrews. He subsequently joined the Department of Computer Science at Carnegie Mellon University as research faculty where he became Professor in 2002 and served as Director of Graduate Programs from 2004 to 2008 and Associate Dean for Graduate Education from 2009 to 2010. He was the Joseph F. Traub Professor of Computer Science and Head of the Computer Science Department from 2013 to 2018. He has spent time as visiting scientist at the Max-Planck-Institute for Computer Science in Saarbrücken, as Alexander-von-Humboldt fellow at the Technical University Darmstadt, and as visiting professor at École Polytechnique and INRIA-Futurs. He has advised 24 completed Ph.D. theses and won the Herbert A. Simon Award for Teaching Excellence in the School of Computer Science in 2002. He served as trustee, vice president, and president of CADE, Inc., the governing body of the International Conference on Automated Deduction, and on advisory boards for INRIA, the Max-Planck-Institute for Computer Science, and Seoul National University. He has chaired several conferences and program committees, including CADE and LICS, and has been a member of the editorial boards for Theoretical Computer Science, Journal of Automated Reasoning, and the Journal of Symbolic Computation. He was named Fellow of the ACM in 2015. His research interests include programming languages, logic and type theory, logical frameworks, automated deduction, and computer security. In his spare time he enjoys playing squash, running, hiking, cooking, and reading.}})

    (seminar
     "bornholt-optimizing-the-automated-programming-stack"
     "Optimizing the Automated Programming Stack"
     "James Bornholt"
     "https://homes.cs.washington.edu/~bornholt/"
     "University of Washington"
     (datetime 2019 3 27 10 00)
     "WVH 366"
     @list{@p{The scale and pervasiveness of modern software poses a challenge for programmers: software reliability is more important than ever, but the complexity of computer systems continues to grow. Automated programming tools are powerful weapons for programmers to tackle this challenge: verifiers that check software correctness, and synthesizers that generate new correct-by-construction programs. These tools are most effective when they apply domain-specific optimizations, but doing so today requires considerable formal methods expertise. In this talk, I present a new application-driven approach to optimizing the automated programming stack underpinning modern domain-specific tools. I will demonstrate the importance of programming tools in the context of memory consistency models, which define the behavior of multiprocessor CPUs and whose subtleties often elude even experts. Our new tool, MemSynth, automatically synthesizes formal descriptions of memory consistency models from examples of CPU behavior. We have used MemSynth to synthesize descriptions of the x86 and PowerPC memory models, each of which previously required person-years of effort to describe by hand, and found several ambiguities and underspecifications in both architectures. I will then present symbolic profiling, a new technique we designed and implemented to help people identify the scalability bottlenecks in automated programming tools. These tools use symbolic evaluation, which evaluates all paths through a program, and is an execution model that defies both human intuition and standard profiling techniques. Symbolic profiling diagnoses scalability bottlenecks using a novel performance model for symbolic evaluation that accounts for all-paths execution. We have used symbolic profiling to find and fix performance issues in 8 state-of-the-art automated tools, improving their scalability by orders of magnitude, and our techniques have been adopted in industry. Finally, I will give a sense of the importance of future application-driven optimizations to the automated programming stack, with applications that inspire improvements to the stack and in turn beget even more powerful automated tools.}}
     @list{@p{James Bornholt is a Ph.D. candidate in the Paul G. Allen School of Computer Science & Engineering at the University of Washington, advised by Emina Torlak, Dan Grossman, and Luis Ceze. His research interests are in programming languages and formal methods, with a focus on automated program verification and synthesis. His work has received an ACM SIGPLAN Research Highlight, two IEEE Micro Top Picks selections, an OSDI best paper award, and a Facebook Ph.D. fellowship.}})

    (seminar
     "hobor-mechanized-verification-of-graph-manipulating-programs"
     "Mechanized Verification of Graph-manipulating Programs"
     "Aquinas Hobor"
     "https://www.comp.nus.edu.sg/~hobor/"
     "Yale-NUS College and the School of Computing, National University of Singapore"
     (datetime 2019 3 22 10 00)
     "WVH 366"
     @list{@p{Graph-manipulating programs such as garbage collectors are notoriously unpleasant to reason about, which is unfortunate since such programs are also used in critical algorithms and systems.  We show how to verify such programs written in real C in a fully machine-checked context.}}
     @list{@p{Aquinas Hobor is an Assistant Professor with a joint appointment at Yale-NUS College and the School of Computing, National University of Singapore.  From 2008-2011 he was a Lee Kuan Yew Postdoctoral Fellow after getting his PhD from Princeton University.  He grew up near Chicago and his Go ranking is approximately 1-dan.}})

    (seminar
     "francois-from-c-to-java-ee-to-node-js"
     "From C to Java EE to Node.js: A Journey in Industrial Program Analysis"
     "François Gauthier"
     "https://labs.oracle.com/pls/apex/f?p=labs:bio:0:2080"
     "Oracle Labs Australia"
     (datetime 2019 3 19 10 00)
     "WVH 462"
     @list{@p{I will divide my presentation in two short talks. In the first part of my presentation, I will describe how static analysis of C/C++, our lab's initial expertise, widely differs from static analysis of Java EE, and describe some of the challenges we encountered in the Wafer project. In the second part of my talk, I will describe how we scale dynamic security analysis for Node.js with taint inference, and how the Affogato project compares to state-of-the-art.}}
     @list{@p{François Gauthier graduated in 2014 from Polytechnique Montreal with a PhD in Software Engineering. The same year, he joined the program analysis team at Oracle Labs Australia, under the direction of Dr. Cristina Cifuentes, to start and lead the Wafer project for static vulnerability detection in Java EE. In June 2017, he transitioned to, and is now leading the Affogato project for dynamic security analysis of Node.js. Apart from application security, François is also exploring program analysis and machine learning approaches to detect document-based malware as well as fuzzing techniques to automatically generate test inputs.}})

    (seminar
     "pichardie-verification-of-constant-time-implementations-in-a-verified-compiler-toolchain"
     "Verification of constant-time implementations in a verified compiler toolchain"
     "David Pichardie"
     "https://people.irisa.fr/David.Pichardie/"
     "ENS Rennes"
     (datetime 2019 3 6 10 00)
     "WVH 366"
     @list{@p{Constant-time programming is an established discipline to secure programs against timing attackers. Several real-world secure C libraries such as NaCl, mbedTLS, or Open Quantum Safe, follow this discipline. We propose two advanced compile-time verification techniques to enforce this discipline. The first one operates at assembly level with help from alias informations that are computed at source level and transmitted by the compiler. The second one operates at source level, using advanced abstract interpretation techniques. For this last approach, we also study how to ensure that the compiler will not break the property during compilation. These techniques have been implemented in the CompCert compiler toolchain and tested on several crypto libraries. These works have been presented at CCS’14, CSF’17 and ESORICS’17.}}
     @list{@p{I am professor of Computer Science and head of the Department of Computer Science at ENS Rennes. My research activities take place in the Celtique team (joint team between University of Rennes, ENS Rennesand Inria Rennes, under the IRISA joint lab). I received a Ph.D. in Computer Science from the University of Rennes, France, in 2005, and a Habilitation à diriger les recherches in Computer Science from the ENS Cachan, France, in 2012. I joined ENS Cachan in September 2013 as full professor (in 2014, the brittany extension of ENS Cachan has becomed ENS Rennes). Between 2007 and 2013, I was a full research at INRIA Rennes research center. Earlier, I was holding a postdoc position at INRIA Sophia-Antipolis under the supervision of Gilles Barthe. In the 2011-13 academic years, I took a sabbatical and visited Jan Vitek's group at Purdue University, Indiana, USA, during the first year, and then Greg Morrisett's group at Harvard University, Cambridge, USA, during the second year. My research interests include formal methods, programming languages, program verification, software, and system security. I am a long time happy user of the Coq proof assistant and the theory of Abstract interpretation. More recently I have been conducting several researches about the verified C compiler CompCert. My research is currently funded by an ERC Consolidator Grant "VESTA: VErified STatic Analysis platform" (2018-2023).}})

    (seminar
     "thakur-compare-less-defer-more"
     "Compare Less, Defer More: Scaling Value-Contexts Based Whole-Program Heap Analyses"
     "Manas Thakur"
     "http://www.cse.iitm.ac.in/~manas/"
     "PACE Lab, Indian Institute of Technology Madras"
     (datetime 2019 2 21 10 0)
     "Ryder 431"
     @list{@p{The precision of heap analyses determines the precision of several associated optimizations, and has been a prominent area in compiler research. It has been shown that context-sensitive heap analyses are more precise than the insensitive ones, but their scalability continues to be a cause of concern. Though the value-contexts approach improves the scalability of classical call-string based context-sensitive analyses, it still does not scale well for several popular whole-program heap analyses. In this talk, I will discuss our three-staged approach that lets us scale complex whole-program value-contexts based heap analyses for large programs. The ideas involved are based on an important observation that we do not need to compare the complete value-contexts at each call-site. Apart from this, I will also give an overview of another work, named PYE (for "precise-yet-efficient”). PYE helps obtain precise results during just-in-time compilation efficiently, by splitting the work between the static Java compiler and the C2 compiler of the HotSpot JVM.}}
     @list{@p{Manas is a PhD student under V. Krishna Nandivada at PACE Lab, IIT Madras. His current research focuses on balancing the precision-scalability tradeoffs while analyzing programs written in object-oriented languages. He has worked with the Soot optimization framework for static analyses, and the HotSpot JVM for just-in-time analyses. Apart from work, he writes articles on GitHub and Medium (technical and otherwise!), and experiments with Vim plugins when feeling geeky.}})

    (seminar
     "hur-promising-arm-risc-v-a-simpler-and-faster-operational-concurrency-model"
     "Promising-ARM/RISC-V: a simpler and faster operational concurrency model"
     "Chung-Kil Hur"
     "https://sf.snu.ac.kr/gil.hur/"
     "Software Foundations Lab, Seoul National University"
     (datetime 2019 2 15 10 0)
     "Ryder 431"
     @list{@p{For ARMv8 and RISC-V, there are concurrency models in two styles, extensionally equivalent: axiomatic models, expressing the concurrency semantics in terms of global properties of complete executions; and operational models, that compute incrementally. The latter are in an abstract micro-architectural style: they execute each instruction in multiple steps, out-of-order and with explicit branch speculation. This similarity to hardware implementations has been important in developing the models and in establishing confidence, but involves complexity that, for programming and model-checking, one would prefer to avoid. In this talk, I will present new more abstract operational models for ARMv8 and RISC-V, and an exploration tool based on them. The models compute the allowed concurrency behaviours incrementally based on thread-local conditions and are significantly simpler than the existing operational models: executing instructions in a single step and (with the exception of early writes) in program order, and without branch speculation. We prove the models equivalent to the existing ARMv8 and RISC-V axiomatic models in Coq. The exploration tool is the first such tool for ARMv8 and RISC-V fast enough for exhaustively checking the concurrency behaviour of a number of interesting examples. We demonstrate using the tool for checking several standard concurrent datastructure and lock implementations such as Michael-Scott queue and Chase-Lev deque, compiled from C++ and Rust with GCC or Clang -O3, and for interactively stepping through model-allowed executions for debugging.}}
     @list{@p{Chung-Kil Hur is an associate professor in Department of Computer Science and Engineering at Seoul National University. Previously he worked as a postdoctoral researcher at Microsoft Research Cambridge,Max Planck Institute for Software Systems (MPI-SWS) and Laboratoire PPS. He obtained a Ph.D. from University of Cambridge and a B.Sc. in both Computer Science and Mathematics from Korea Advanced Institute of Science and Technology (KAIST). His current research interests are in semantics of low-level languages such as C,Rust,LLVM IR, relaxed memory concurrency, formal verification of software such as compiler and OS, and interactive theorem provers such as Coq.}})

    (seminar
     "gonzalez-path-based-function-embedding-and-its-application-to-error-handling-specification-mining"
     "Path-Based Function Embedding and Its Application to Error-Handling Specification Mining"
     "Cindy Rubio González"
     "http://web.cs.ucdavis.edu/~rubio/"
     "University of California, Davis"
     (datetime 2019 2 14 10 0)
     "Ryder 431"
     @list{@p{Identifying relationships among program elements is useful for program understanding, debugging, and analysis. One such kind of relationship is synonymy. Function synonyms are functions that play a similar role in code; examples include functions that perform initialization for different device drivers, and functions that implement different symmetric-key encryption schemes. Function synonyms are not necessarily semantically equivalent and can be syntactically dissimilar; consequently, approaches for identifying code clones or functional equivalence cannot be used to identify them. In this talk I will present our recent work Func2vec, a technique that learns an embedding that maps each function to a vector in a continuous vector space such that vectors for function synonyms are in close proximity. We compute the function embedding by training a neural network on sentences generated using random walks over the interprocedural control-flow graph. We show the effectiveness of Func2vec at identifying function synonyms in the Linux kernel, and its applicability to the problem of mining error-handling specifications in Linux file systems and drivers.}}
     @list{@p{Cindy Rubio-Gonzalez is an Assistant Professor of Computer Science at the University of California, Davis. Prior to that position, she was a Postdoctoral Researcher in the EECS Department at the University of California, Berkeley. She received her Ph.D. in Computer Science from the University of Wisconsin--Madison in 2012. Cindy's work spans the areas of Programming Languages and Software Engineering, with a focus on program analysis for automated bug finding, program optimization,and software reproducibility. She is particularly interested in the reliability and performance of systems software and scientific computing applications. She currently leads the BugSwarm project,which collects and automatically reproduces thousands of real-world bugs from public software repositories. Among other awards, Cindy is a recipient of an NSF CAREER award 2018, Hellman Fellowship 2017, and UC Davis CAMPOS Faculty Award 2014.}})

    (seminar
     "titzer-what-spectre-means-for-language-implementor"
     "What Spectre means for language implementors"
     "Ben L. Titzer"
     "http://research.google.com/pubs/BenTitzer.html"
     "Google"
     (datetime 2019 2 13 10 0)
     "Ryder 155"
     @list{@p{Until now, CPU designers and computer scientists have assumed Vegas rules at the hardware level: what happens in speculation stays in speculation. Yet in the wake of the Spectre and Meltdown attacks, it has become clear a new, massive class of security vulnerabilities awaits us at the microarchitectural level because this assumption is simply false. As language designers and implementors familiar with building towers of abstractions, we have assumed that virtualization through emulation made the worlds below the Turing machine undetectable, hidden behind a mathematically perfect mirror. This talk will explore how we have now learned to see through that mirror, into the very bizarre and alien world of microarchitectures, illuminating a tiny world of astounding complexity that holds profound implications for language security.}}
     @list{@p{Ben L. Titzer leads Google's WebAssembly team in Munich. Before starting the WebAssembly project with Luke Wagner from Mozilla, he led the team that built the TurboFan optimizing compiler which now powers JavaScript and WebAssembly in V8. He graduated with a BS from Purdue University in 2002 and MS and PhD from UCLA in 2004 and 2007. His interests include safe programming languages for systems programming, compilers, virtual machines, nature and playing guitar.}})

    (seminar
     "waye-whip-contract-service"
     "Whip: Higher-order Contracts for Modern Services"
     "Lucas Waye"
     "http://lucaswaye.com/"
     "Facebook"
     (datetime 2019 2 6 10 0)
     "WVH 366"
     @list{@p{Modern service-oriented applications forgo semantically rich protocols and middleware when composing services. Instead, they embrace the loosely-coupled development and deployment of services that communicate via simple network protocols. Even though these applications do expose interfaces that are higher-order in spirit, the simplicity of the network protocols forces them to rely on brittle low-level encodings. To bridge the apparent semantic gap, programmers introduce ad-hoc and error-prone defensive code. Inspired by Design by Contract, we choose a different route to bridge this gap. We introduce Whip, a contract system for modern services. Whip (i) provides programmers with a higher-order contract language tailored to the needs of modern services; and (ii) monitors services at run time to detect services that do not live up to their advertised interfaces. Contract monitoring is local to a service. Services are treated as black boxes, allowing heterogeneous implementation languages without modification to services' code. Thus, Whip does not disturb the loosely coupled nature of modern services.}}
     @list{@p{I'm currently a Software Engineer at Facebook thinking about privacy in the data warehouse. Previously I helped build TiVo's Targeted Audience Delivery platform. And before that I worked on the backend for Timeful (acquired by Google) and on Hulu's analytics platform. In graduate school I worked on Whip, a contract monitor for microservices. I received a Ph.D. and Master's degree from Harvard under the guidance of Stephen Chong and a Bachelor's degree from Cornell.}})

    (seminar
     "fare-better-stories-reframing-from-programs-to-programming"
     "Better Stories, Better Languages — Reframing from Programs to Programming"
     "François-René Rideau"
     "http://fare.tunes.org"
     "Alacris"
     (datetime 2019 1 30 10 0)
     "WVH 366"
     @list{@p{Software tools imply a story. New stories can help invent new tools. Explicit stories are a great meta-tool... and provide a systematic way to improve the design of programming language design. To illustrate my point, I will present a series of pairs of stories, each time approaching a same topic with very different consequences. Many of my stories will be familiar to most of you, some other stories less so. Hopefully they will be enlightening as well as entertaining... and so will the meta-story be. (Earlier versions of this talk were given at International Lisp Conference 2009, Lisp NYC 2014-10, and more recently LambdaConf 2017.)}}
     @list{@p{François-René Rideau is Co-Founder and Chief Architect at Alacris.io,a company developing a "Blockchain Layer 2 Operating System" using formal methods. Once founder of the TUNES Project, he left academic research on Programming Languages and Distributed Systems to build industrial software at ITA Software, then Google and Bridgewater Associates, before he finally became his own Entrepreneur. Long-standing member of the Lisp community, he runs the Boston Lisp Meeting and still co-maintains ASDF (the Common Lisp build system),though he recently jumped ship to Gerbil Scheme, and now uses OCaml at work.}})

    (seminar
     "chris-goblins-and-spritely"
     "Goblins and Spritely: from the actor model to distributed virtual worlds"
     "Christoper Lemmer Webber"
     "https://dustycloud.org/"
     "ActivityPub"
     (datetime 2018 12 5 10 0)
     "WVH 366"
     @list{@p{Goblins is an actor model implementation for Racket, inspired by object-capability secure distributed progamming languages such as E with a few new twists of its own thrown in. Goblins is the foundation for a project to build secure, player/user-programmable virtual worlds named Spritely. Spritely builds on the success of the ideas from the ActivityPub protocol, which has succeeded at connecting together various distributed social network projects, extending them to pull in rich interactive and game aspects. See what is built so far, what's coming next, and how the actor model and object capabilities tie together all these ideas to enable a robust and secure distributed system.}}
     @list{@p{Christopher Lemmer Webber is a user freedom advocate who works on distributed social network technology. They are most well known for their work on the W3C ActivityPub federated protocol, which connects over 1.5 million users across the distributed social web. Chris enjoys programming in lisp/scheme flavored languages, especially Racket.}})

    (seminar
     "vitaly-language-development-standardization-haskell"
     "Programming language development and standardization: How they (don’t) do in Haskell"
     "Vitaly Bragilevsky"
     "http://sfedu.ru/www/stat_pages22.show?p=RR/per_eng/D&params=(p_per_id=%3E2733)"
     "Southern Federal University"
     (datetime 2018 11 28 10 0)
     "WVH 366"
     @list{@p{Programming languages differ a lot in the ways of their development. Who is responsible for including new features and removing obsolete ones? What is a process of doing that? Is there any more or less formal language definition? How respected is that definition by the compilers? How is this changed if there is only one compiler? These questions are answered quite differently for various programming languages. Haskell takes its own way which was crystallized relatively recently. We have GHC Proposals in the form of the GitHub pull requests to suggest new language features. We also have the GHC Steering Committee with its own process to either accept or reject proposals. Finally, there is the Haskell 2020 Language (Haskell Prime) Committee with its own problem of being effectively dead. In this talk, I’d like to describe how this system works for Haskell and why it doesn’t work sometimes. I will discuss the most recent examples of transforming Haskell kinds system towards dependent types (started before the GHC Steering Committee was formed) and introducing linear types in Haskell (subject to the full-blown Committee discussion) among others. I will also characterize the current state and perspectives of the Haskell standardization process.}}
     @list{@p{Vitaly Bragilevsky serves as both the Haskell 2020 Language Committee and the GHC Steering Committee member. He works as a Senior Lecturer at the Southern Federal University in Rostov-on-Don, Russia where he teaches undergraduate students functional programming and theory of computations. He is currently a grantee of the Fulbright Faculty Development Program holding temporary Courtesy Research Assistant position at the University of Oregon under the supervision of Prof. Zena Ariola. Vitaly Bragilevsky translated into Russian and edited translations of the several books on Haskell and the theory of programming languages. He is the author of ‘Haskell in Depth’ (Manning Publications, available via Manning’s early access program).}})

    (seminar
     "heineman-synthesize-expression-problem"
     "Synthesizing Solutions to the Expression Problem"
     "George T. Heineman"
     "https://www.wpi.edu/people/faculty/heineman"
     "Worcester Polytechnic Institute"
     (datetime 2018 10 17 10 0)
     "WVH 366"
     @list{@p{The Expression Problem (EP) describes a common software issue that appears regardless of programming language or programming paradigm. As coined by Philip Wadler in 1998, EP is a new name for an old problem. Given a data type defined by cases, consider two independent extensions, namely, adding new cases to the data type and new functions over the datatype. Dozens of papers in the research literature present their solutions to the EP, using a variety of programming languages and approaches. In this talk I present a Scala-based framework that can synthesize full solutions to the EP problem. The first step is to model the domain of interest (such as mathematical expressions or geometric shapes) to ensure that our synthesized code can work with any desired application domain. Second, we demonstrate how to synthesize code that reproduces a dozen EP solutions from the literature, in C++, Java and Haskell. We designed a common API for all approaches, independent of programming language. While we do not introduce any new approach for EP, we explore the challenges that EP solutions face with binary methods and producer methods. An important contribution of this work is the scientific reproduction of known solutions from the literature.
This work is based on an ongoing collaboration with with Jan Bessai (TU Dortmund) and Boris Düdder (Univ. Copenhagen).}}
     @list{@p{George T. Heineman is an Associate Professor of Computer Science at WPI. His research interests are in Software Engineering, specifically synthesizing software systems from composable units. He is co-author of "Algorithms in a Nutshell (2ed)," published by O'Reilly Media in 2016. Aside from his professional pursuits, George is an avid puzzler. He invented Sujiken(R), a Sudoku variation played on a right-triangle arrangement of cells in which numbers cannot repeat in a horizontal row, vertical column or diagonal in any direction.}})

    (seminar
     "greenberg-posix-shell"
     "Rehabilitating the POSIX shell"
     "Michael Greenberg"
     "http://www.cs.pomona.edu/~michael/"
     "Pomona College"
     (datetime 2018 10 10 10 0)
     "WVH 366"
     @list{@p{We build intricate systems with complex algorithms and invariants, aiming for guarantees of correctness and performance... and then we maintain and deploy these systems with shell scripts! What *are* shell scripts? If the POSIX shell is a programming language, what are its syntax and semantics? Can we apply PL tools to reason about the shell? Why haven't prior PL attempts at understanding the shell redeemed it?}}
     @list{@p{Michael Greenberg is an assistant professor at Pomona College. He received his BA in Computer Science and Egyptology from Brown University (2007) and his PhD in Computer Science from the University of Pennsylvania (2013). His work has ranged from functional-reactive JavaScript (with Shriram Krishnamurthi at Brown) to runtime verification of higher-order programs using contracts (with Benjamin Pierce at Penn) to software-defined networking (with Dave Walker at Princeton) to present activities focused on Kleene algebra with tests and the POSIX shell. He is always looking for new climbing partners.}})

    (seminar
     "cifuentes-parfait-vulnerability-detection"
     "Oracle Parfait: The Flavour of Real-World Vulnerability Detection"
     "Cristina Cifuentes"
     "https://labs.oracle.com/pls/apex/f?p=labs:bio:0:21"
     "Oracle Labs"
     (datetime 2018 10 3 13 15)
     "WVF 010"
     @list{@p{The Parfait static code analysis tool focuses on detecting vulnerabilities that matter in C, C++, Java and PL/SQL languages. Its focus has been on key items expected out of a commercial tool that lives in a commercial organization, namely, precision of results (i.e., high true positive rate), scalability (i.e., being able to run quickly over millions of lines of code), incremental analysis (i.e., being able to run over deltas of the code quickly), and usability (i.e., ease of integration into standard build processes, reporting of traces to the vulnerable location, etc). Today, Parfait is used by thousands of developers at Oracle worldwide on a day-to-day basis. In this presentation we’ll sample a flavour of Parfait — we explore some real world challenges faced in the creation of a robust vulnerability detection tool, look into two examples of vulnerabilities that severely affected the Java platform in 2012/2013 and most machines in 2017/2018, and conclude by recounting what matters to developers for integration into today’s continuous integration and continuous delivery (CI/CD) pipelines.}}
     @list{@p{Cristina is the Director of Oracle Labs Australia and an Architect at Oracle. Headquartered in Brisbane, the Lab focuses on Program Analysis as it applies to finding vulnerabilities in software and enhancing the productivity of developers worldwide. Prior to founding Oracle Labs Australia, Cristina was the Principal Investigator of the Parfait bug tracking project at Sun Microsystems, then Oracle.}})

    (seminar
     "kammar-user-defined-effects"
     "On the expressive power of user-defined effects"
     "Ohad Kammar"
     "https://www.cs.ox.ac.uk/people/ohad.kammar/main.html"
     "University of Oxford"
     (datetime 2018 10 3 10 00)
     "WVH 366"
     @list{@p{Computational effects, such as mutable state, memory allocation, non-determinism, and I/O interaction, allow programs to implicitly exhibit complex behaviour. I will discuss three abstractions that allow programmers to extend a language with user-defined effects. Control operators (call-cc, shift/reset, etc.)  are a well-established abstraction for user-defined effects, and I will consider the specific shift/dollar without answer-type-modification. Since the 90s, functional languages have used monads for user-defined effects, and I will consider Filinski's monadic reflection. In the last decade, the community started looking into Plotkin and Pretnar's handlers for algebraic effects (extensible effects in Haskell, OCaml multicore, the Eff, Frank, Koka, and Links programming languages) as a programming abstraction, and I will consider deep handlers without forwarding. I will compare the relative expressive power of these three abstractions using Felleisen's notion of macro-translations.  This comparison demonstrates the sensitivity of relative expressiveness of user-defined effects to seemingly orthogonal language features. This talk is based on the paper: On the expressive power of user-defined effects: effect handlers,monadic reflection, delimited control. Yannick Forster, Ohad Kammar, Sam Lindley, and Matija Pretnar. Proceedings of the 22nd ACM SIGPLAN International Conference on Functional Programming, PACMPL 1(ICFP): 13:1-13:29 (2017),arXiv:1610.09161, DOI: 10.1145/3110257.}}
     @list{@p{Ohad Kammar is a postdoctoral research associate at the University of Oxford Department of Computer Science, a Career Development Fellow at Balliol College Oxford, and an incoming Royal Society University Research Fellow at the University of Edinburgh School of Informatics. His fields of interests include programming language modelling and design, logic, and the foundations of computer science.}})

    (seminar
     "pedersen-trashtreasure"
     "From trash to treasure: Timing-sensitive garbage collection"
     "Mathias Pedersen"
     "http://cs.au.dk/~mvp/"
     "Aarhus University"
     (datetime 2018 9 19 10 00)
     "WVH 366"
     @list{@p{We study information flows arising from timing channels in the presence of automatic memory management. We construct a series of example attacks that illustrate how garbage collectors form a shared resource that can be used to reliably leak sensitive information at a rate of up to 1 byte/sec on a contemporary general-purpose computer. The created channel is also observable across a network connection in a datacenter-like setting. We subsequently present a design of an automatic memory management system and prove that the presented attacks are impossible on garbage collectors satisfying this design. The guarantees provided by the language has been mechanized in the Coq proof assistant.}}
     @list{@p{Mathias is a third year PhD student in the Logic and Semantics group at Aarhus University in Denmark, advised by Aslan Askarov. His work is in the area of language-based security, with a focus on provable mitigation of side channels. In general, anything related to compilers and the semantics of programming languages will be on his list of interests.}})

    (seminar
     "pombrio-infer-type-sugar"
     "Inferring Type Rules for Syntactic Sugar"
     "Justin Pombrio"
     "http://justinpombrio.net"
     "Brown University"
     (datetime 2018 8 7 11 30)
     "WVH 366"
     @list{@p{Type systems and syntactic sugar are both valuable to programmers, but sometimes at odds. While sugar is a valuable mechanism for implementing realistic languages, the expansion process obscures program source structure. As a result, type errors can reference terms the programmers did not write (and even constructs they do not know), baffling them. The language developer must also manually construct type rules for the sugars, to give a typed account of the surface language. We address these problems by presenting a process for automatically reconstructing type rules for the surface language using rules for the core. We have implemented this theory, and show several interesting case studies.}}
     @list{@p{Justin Pombrio is a recent PhD graduate from Brown University. His research is mainly in programming languages, with a focus on syntactic sugar, but also includes CS education.}})


    (seminar
     "nardelli-debug-debug"
     "Debugging Debug Information and Beyond"
     "Francesco Zappa Nardelli"
     "http://www.di.ens.fr/~zappa/"
     "Inria Paris – Rocquencourt"
     (datetime 2018 6 15 10 00)
     "WVH 366"
     @list{@p{The spectacular results achieved by computer science in the recent years rely on hidden, obscure, and badly specified components that lurk at the very heart of our computing infrastructure. Consider DWARF debug information. Debug information is relied upon by debuggers and plays a key role in the in the implementation of program analysis tools. More surprisingly, debug information can be relied upon by the runtime of high-level programming languages, for instance by C++ to unwind the stack and implement exceptions. The debug information itself can be pervaded by subtle bugs, making the whole infrastructure unreliable. In this talk I will describe techniques and tools to perform validation and synthesis of the DWARF stack unwinding tables. I will also report on adventurous projects that we might build on top of reliable DWARF information.}}
     @list{@p{Francesco Zappa Nardelli is a Researcher at Inria Paris – Rocquencourt. His research interests focus on concurrent computation on top of relaxed memory models, ranging from hardware models of modern architectures to high-level programming language specification and compilation. He received his Ph.D. from Université Paris 7 in 2003. Since then, he has worked on language design for distributed computation, type systems for integrating typed and untyped code in scripting languages, and tool support for semantics (including the Ott tool).}})

    (seminar
     "new-cbn-gtt"
     "Call-By-Name Gradual Type Theory"
     "Max New"
     "http://maxsnew.github.io/"
     "Northeastern University"
     (datetime 2018 4 27 11 30)
     "WVH 366"
     @list{@p{I will present Call-by-name (CBN) Gradual Type Theory [1], an axiomatic semantics of CBN gradual typing that directly axiomatizes the extensionality (eta) principles of types and the "gradual guarantee" of [2]. By axiomatizing the "term precision" relation of [2] we can provide a specification for casts as meets and joins with respect to precision. Using our type theory, we then show that the classic function contract ([3]) is in fact the unique implementation of function casts that satisfies extensionality and the gradual guarantee. This shows that the combination of these properties is a very strong constraint on the design of a gradually typed language and any implementation that departs must sacrifice one of these properties (typically extensionality).}@p{[1] Call-by-Name Gradual Type Theory, New and Licata, to appear FSCD '18,https://arxiv.org/abs/1802.00061}@p{[2] Refined Criteria for Gradual Typing, Siek, Vitousek, Cimini and Boyland, SNAPL '15, http://drops.dagstuhl.de/opus/volltexte/2015/5031/}@p{[3] Contracts for Higher-Order Functions, Findler and Felleisen, ICFP '02,https://dl.acm.org/citation.cfm?id=581484}}
    @list{@p{Max New is a PhD student at Northeastern University working on the semantic foundations of programming languages. He hates proving the same theorem twice and you should too.}})

    (seminar
     "tristan-probabilistic"
     "Compilation of Simple Probabilistic Programs to Gibbs Sampling."
     "John Tristan"
     "https://jtristan.github.io/"
     "Oracle Labs"
     (datetime 2018 4 10 11 45)
     "WVH 366"
     @list{@p{One of the several interesting challenges of probabilistic programming is that of compiling probabilistic programs to inference algorithms. One of these inference algorithms, Gibbs sampling, is particularly relevant because it is often statistically efficient, but unfortunately, it is difficult to derive and therefore compile to.} @p{In this talk, after a brief explanation of probabilistic programming and why its relevance to data science, I will explain some of the ideas behind the design of a compiler from (very) simple probabilistic programs to Gibbs sampling. I will also attempt to explain what it would mean for such a compiler to be correct.}}
     @list{@p{Jean-Baptiste Tristan is a researcher in the machine learning group at Oracle Labs. He holds a Ph.D. in Computer Science from the French Institute for Research in Computer Science and Automation (INRIA) and a M.Sc. in Computer Science from the Ecole Normale Superieure of Paris.}})

    (seminar
     "alexandrescu-introspection"
     "Design by Introspection using the D Language"
     "Andrei Alexandrescu"
     "http://erdani.com"
     "D Language Foundation"
     (datetime 2018 2 6 11 45)
     "WVH 366"
     @list{@p{Years ago, D started modestly as an improved offering in the realm of systems programming languages, sharing a good deal of philosophy with C and C++. With time, however, D became a very distinct language with unique features (and, of course, its own mistakes).}@p{One angle of particular interest has been D's ability to perform compile-time introspection. Artifacts in a D program can be "looked at" during compilation. Coupled with a full-featured compile-time evaluation engine and with an ability to generate arbitrary code during compilation, this has led to a number of interesting applications.}@p{This talk shares early experience with using these features of the D language. Design by Introspection is a proposed programming paradigm that assembles designs by performing introspection on components during compilation.}}
     @list{@p{Andrei Alexandrescu is a researcher, software engineer, and author. He wrote three best-selling books on programming (Modern C++ Design, C++ Coding Standards, and The D Programming Language) and numerous articles and papers on wide-ranging topics from programming to language design to Machine Learning to Natural Language Processing. Andrei holds a PhD in Computer Science from the University of Washington and a BSc in Electrical Engineering from University "Politehnica" Bucharest. He currently works on the D Language Foundation. http://erdani.com}})
    (seminar
     "aref-declarative"
     "Solver-Aided Declarative Programming"
     "Molham Aref"
     "#"
     "Relational AI"
     (datetime 2017 12 14 10 00)
     "WVH 366"
     @list{@p{I will summarize our work on a declarative programming language that offers native language support for model (or instance) finding. This capability can be used to express predictive (e.g. machine learning) and prescriptive (e.g. combinatorial optimization) analytics. The presentation gives an overview of the platform and the language. In particular, it focuses on the important role of integrity constraints,which are used not only for maintaining data integrity, but also, for the formal specification of complex optimization problems and probabilistic programming.}}
     @list{@p{Mr. Molham Aref is the Chief Executive Officer of Relational AI. Mr. Aref has more than 25 years of experience in developing and implementing enterprise-grade analytic, predictive, optimization and simulation solutions for the demand chain, supply chain, and revenue management across various industries. Relational AI combines the latest advances in Artificial Intelligence with a understanding of business processes to develop solutions that shape better decisions, improve agility, and reduce risk. Prior to Relational AI, he was co-founder and CEO of LogicBlox where he led the company from inception through a successful sale to Infor. Previously, he was CEO of Optimi (acquired by Ericsson),a leader in wireless network simulation and optimization, and co-founder of Brickstream (renamed Nomi and then acquired by FLIR), a leading provider of computer-vision-based behavior intelligence solutions.}})

    (seminar
     "martens-compositional"
     "Compositional Creativity"
     "Chris Martens"
     "https://sites.google.com/ncsu.edu/cmartens"
     "North Carolina State University"
     (datetime 2017 12 11 11 00)
     "WVH 366"
     @list{@p{Creativity is seen as a core cognitive ability for tasks like storytelling, conversation, and aesthetic design. Simulating creative processes with computers enables them to amplify humans' natural creativity and to support applications like interactive narrative, chatbots, and procedural level generators for games. By now, a large number of techniques exist for simulating creativity, such as search, planning, production grammars, logic programming, stochastic modeling from data, and hybrid approaches. However, even the simplest methods are commonly considered programming tasks for "AI experts." High-level programming languages have the potential to broaden participation in authorship of these systems, but only if they are designed with compositionality -- the mathematical principle that's also functional programming's secret weapon -- in mind. This talk will discuss what compositionality has achieved so far for creative computing and what we can do with it next.}}
     @list{@p{Chris Martens is an assistant professor in the Computer Science (CSC) Department at NC State, where she is affiliated with the Digital Games research group. Her interests span artificial intelligence, programming languages, and formal methods.}})

    (seminar
     "muehlboeck-nom"
     "Efficient Nominal Gradual Typing"
     "Fabian Muehlboeck"
     "http://www.cs.cornell.edu/~fabianm/"
     "Cornell University"
     (datetime 2017 12 5 14 00)
     "WVH 366"
     @list{@p{Gradual Typing is the idea that we can support both static and dynamic type checking in different parts of the same program in the same language. Most existing work on gradual typing aims to add gradual typing to an existing language. However, this severely constrains some key decisions in creating gradually typed languages, often leading designers to trade off either soundness or efficiency.}
           @p{I will talk about designing a nominal object-oriented language with gradual typing as a core component from the start. This affects key design decisions for the other type system features, as well as the design of the runtime. Experiments with a prototype language I implemented suggest that this is a viable approach to achieve sound, yet efficient gradual typing. I will present those results and talk about some key design challenges that have yet to be solved.}}
     @list{@p{Fabian Muehlboeck is a Ph.D. student working with Ross Tate at Cornell University.}})

    (seminar
     "siskand-cps"
     "What does CPS have to do with deep learning?"
     "Jeffrey M. Siskind"
     "https://engineering.purdue.edu/~qobi/"
     "Purdue University"
     (datetime 2017 9 11 13 30)
     "WVH 366"
     @list{@p{Deep learning is formulated around backpropagation, a method for computing gradients of a particular class of data flow graphs to train models to minimize error by gradient descent.  Backpropagation is a special case of a more general method known as reverse mode automatic differentiation (AD) for computing gradients of functions expressed as programs.  Reverse mode AD imposes only a small constant-factor overhead in operation count over the original computation, but has storage requirements that grow, in the worst case, in proportion to the time consumed by the original computation.  This storage blowup can be ameliorated by check pointing, a process that reorders application of classical reverse-mode AD over an execution interval to tradeoff space vs. time.  Application of check pointing in a divide-and-conquer fashion to strategically chosen nested execution intervals can break classical reverse-mode AD into stages which can reduce the worst-case growth in storage from linear to logarithmic.  Doing this has been fully automated only for computations of particularly simple form, with checkpoints spanning execution intervals resulting from a limited set of program constructs.  We show how the technique can be automated for arbitrary computations.  Doing so relies on implementing general purpose mechanisms for counting the number of instructions executed by a program, interrupting the execution after a specified number of steps, and resuming the execution with a nonstandard interpretation.  We implement these general purpose mechanisms with a compiler that converts programs to continuation-passing style (CPS).  The process of efficiently computing gradients with check pointing requires running, and rerunning, little bits of the program out of order.  This is made easier by applying the technique to functional programs. There is a deeper and higher-level message in this talk: machine learning can benefit from advanced techniques from programming language theory.}}
     @list{@p{Jeffrey M. Siskind received the B.A. degree in Computer Science from the Technion, Israel Institute of Technology, Haifa, in 1979, the S.M. degree in Computer Science from the Massachusetts Institute of Technology (M.I.T.), Cambridge, in 1989, and the Ph.D. degree in Computer Science from M.I.T. in 1992.  He did a postdoctoral fellowship at the University of Pennsylvania Institute for Research in Cognitive Science from 1992 to 1993.  He was an assistant professor at the University of Toronto Department of Computer Science from 1993 to 1995, a senior lecturer at the Technion Department of Electrical Engineering in 1996, a visiting assistant professor at the University of Vermont Department of Computer Science and Electrical Engineering from 1996 to 1997, and a research scientist at NEC Research Institute, Inc. from 1997 to 2001.  He joined the Purdue University School of Electrical and Computer Engineering in 2002 where he is currently an associate professor.  His research interests include computer vision, robotics, artificial intelligence, neuroscience, cognitive science, computational linguistics, child language acquisition, automatic differentiation, and programming languages and compilers.}})

    (seminar
     "jamner-gradual"
     "Relational Parametricity for Polymorphic Blame Calculus"
     "Dustin Jamner"
     "https://github.com/DIJamner"
     "Northeastern University"
     (datetime 2017 6 23 12 00)
     "WVH 366"
     @list{@p{The polymorphic blame calculus integrates static typing, including universal types, with dynamic typing. The primary challenge with this integration is preserving parametricity: even dynamically-typed code should satisfy it once it has been cast to a universal type. Ahmed et al. (2011) employ runtime type generation in the polymorphic blame calculus to preserve parametricity, but a proof that it does so has been elusive. Matthews and Ahmed (2008) gave a proof of parametricity for a closely related system that combines ML and Scheme, but later found a flaw in their proof. In this work we prove that the polymorphic blame calculus satisfies relational parametricity. The proof relies on a step-indexed Kripke logical relation. The step-indexing is required to make the logical relation well-defined in the case for the dynamic type. The possible worlds include the mapping of generated type names to their concrete types and the mapping of type names to relations. We prove the Fundamental Property of this logical relation and that it is sound with respect to contextual equivalence.}}
     @list{@p{Dustin Jamner is an undergraduate at Northeastern University in Computer Science, working with Amal Ahmed.}})

    (seminar
     "tinelli-cocospec"
     "CoCoSpec: A Mode-aware Contract Language for Reactive Systems"
     "Cesare Tinelli"
     "http://homepage.cs.uiowa.edu/~tinelli/"
     "University of Iowa"
     (datetime 2017 6 1 11 00)
     "WVH 366"
     @list{@p{Contract-based software development is a leading methodology for the construction of safety- and mission-critical embedded systems. Contracts are an effective way to establish boundaries between components and can be used efficiently to verify global properties by using compositional reasoning techniques. A contract specifies the assumptions a component makes on its context and the guarantees it provides. Requirements in the specification of a component are often case-based, with each case describing what the component should do depending on a specific situation (or mode) the component is in.}@p{This talk introduces CoCoSpec, a mode-aware assume-guarantee-based contract language for embedded systems. CoCoSpec is built as an extension of the Lustre language and lets users specify mode behavior directly, instead of encoding it as conditional guarantees, thus preventing a loss of mode-specific information. Mode-aware model checkers supporting CoCoSpec can increase the effectiveness of the compositional analysis techniques found in assume-guarantee frameworks and improve scalability. Such tools can also produce much better feedback during the verification process, as well as valuable qualitative information on the contract itself. I will presents the CoCoSpec language and illustrate the benefits of mode-aware model-checking on a case study involving a flight-critical avionics system. The evaluation uses Kind 2, a collaborative, parallel, SMT-based model checker developed at the University of Iowa that provides full support for CoCoSpec.}}
     @list{@p{Cesare Tinelli received a Ph.D. in Computer Science from the UIUC in 1999 and is currently a professor in Computer Science at the University of Iowa. His research interests include automated reasoning and formal methods. He has done seminal work in automated reasoning, in particular in Satisfiability Modulo Theories (SMT), a field he helped establish through his research and service activities. His work has been funded both by governmental agencies (AFOSR, AFRL, DARPA, NASA, and NSF) and corporations (Intel, General Electric, Rockwell Collins, and United Technologies) and has appeared in more than 70 refereed publications. He is a founder and coordinator of the SMT-LIB initiative, an international effort aimed at standardizing benchmarks and I/O formats for SMT solvers. He has led the development of the award winning Darwin theorem prover and the Kind model checker. He co-leads the development of CVC4,a widely used and award winning SMT solver, and StarExec, a cross community web-based service for the comparative evaluation of logic solvers.}})


    (seminar
     "linear-experiment"
     "Multi-language programming system: a linear experiment"
     ""
     ""
     ""
     (datetime 2017 5 5 12 00)
     "WVH 366"
     @list{@p{Programming language research keeps inventing more powerful dynamic and static features for programming language. Ideally, thorough language design should be able to propose a single, unified, powerful yet simple view of them as orthogonal aspects of a single programming language that would remain usable by everyone. In practice, it always takes much more time and effort to find the simple/orthogonal presentation than to come up with the desirable features, and we become emotionally attached to programming language monoliths (C++, Scala, GHC Haskell, OCaml, Common Lisp...) that suffer from feature creep. An alternative is to design programming systems as multi-language environment instead. This point of view makes it easier to detect flaws in a proposed programming system design: there is no clear objective definition of what "orthogonal features" really mean, but we have clear ideas of how multi-language systems should work. The Racket Manifesto says: "Each language and component must be able to protect its specific invariants".}@p{In this work we give a formal specification, in terms of full abstraction, for what it means for a new language added to the system to not introduce "abstraction leaks" in the other languages, and discuss scenarios where this translates in (informal) usability properties: having a nice teachable language subset, or gracefully introducing an advanced language for experts. We present a multi-language design that exhibits this formal property by adding a language L with linear types and linear state to a typical ML-style programming language U. L allows to write programs with resource usage guarantees (space consumption, typestate-style safety guarantees) that were not possible in U, without introducing abstraction leaks. We will demonstrate several useful examples, some of them that crucially rely on the fine-grained use of language boundaries, at a subterm level rather than whole-module level.}}
     @list{@p{}})


    (seminar
     "madsen-flix"
     "From Datalog to Flix: A Declarative Language for Fixed Points on Lattices"
     "Magnus Madsen"
     "plg.uwaterloo.ca/~mmadsen/"
     "University of Waterloo"
     (datetime 2017 4 14 13 30)
     "WVH 366"
     @list{@p{We present FLIX, a declarative programming language for specifying and solving least fixed point problems, particularly static program analyses. FLIX is inspired by Datalog and extends it with lattices and monotone functions. Using FLIX,implementors of static analyses can express a broader range of analyses than is currently possible in pure Datalog, while retaining its familiar rule-based syntax. We define a model-theoretic semantics of FLIX as a natural extension of the Datalog semantics. This semantics captures the declarative meaning of FLIX programs without imposing any specific evaluation strategy. An efficient strategy is semi-naïve evaluation which we adapt for FLIX. We have implemented a compiler and runtime for FLIX, and used it to express several well-known static analyses, including the IFDS and IDE algorithms. The declarative nature of FLIX clearly exposes the similarity between these two algorithms.} @p{This work has previously been presented at PLDI 2016. The talk will cover some of that material as well as recent developments.}}
   @list{@p{The speaker is a post doctoral fellow at the University of Waterloo.}})


    (seminar
     "sagiv-protocols"
     "Simple Invariants for proving the safety of  distributed protocols and networks"
     "Mooly Sagiv"
     "http://www.cs.tau.ac.il/~msagiv/"
     "Tel Aviv University"
     (datetime 2017 3 31 12 00)
     "WVH 366"
     @list{@p{Safety of a distributed protocol means that the protocol never reaches a bad state, e.g., a state where two nodes become leaders in a leader-election protocol. Proving safety is obviously undecidable since such protocols are run by an unbounded number of nodes, and their safety needs to be established for any number of nodes. I will describe a deductive approach for proving safety, based on the concept of universally quantified inductive invariants --- an adaptation of the mathematical concept of induction to the domain of programs. In the deductive approach, the programmer specifies a candidate inductive invariant and the system automatically checks if it is inductive. By restricting the invariants to be universally quantified, this approach can be effectively implemented with a SAT solver.}@p{This is a joint work with Ken McMillan (MSR), Oded Padon (TAU), Aurojit Panda(Berkeley) , and Sharon Shoham(TAU) and was integrated into the IVY system: http://www.cs.tau.ac.il/~odedp/ivy/. The work is inspired by Shachar Itzhaky's thesis available from http://people.csail.mit.edu/shachari/}}
     @list{@p{Mooly Sagiv is a professor in the School of Computer Sciences at Tel-Aviv University. He is a leading researcher in the area of large scale (inter-procedural) program analysis, and one of the key contributors to shape analysis. His fields of interests include programming languages, compilers, abstract interpretation, profiling, pointer analysis, shape analysis, inter-procedural dataflow analysis, program slicing, and language-based programming environments. Sagiv is a recipient of a 2013 senior ERC research grant for Verifying and Synthesizing Software Composition. Prof. Sagiv served as Member of the Advisory Board of Panaya Inc acquired by Infosys. He received best-paper awards at PLDI'11 and PLDI'12  for his work on composing concurrent data structures and a ACM SIGSOFT Retrospective Impact Paper Award (2011) for program slicing. He is an ACM fellow and a recipient of Microsoft Research Outstanding Collaborator Award 2016.}})

    (seminar
     "chaudhuri-automatic"
     "Learning to Program and Debug, Automatically"
     "Swarat Chaudhuri"
     "https://www.cs.rice.edu/~sc40/"
     "Rice University"
     (datetime 2017 3 24 12 00)
     "WVH 366"
     @list{@p{Automating programming and debugging are long-standing goals in computer science. In spite of significant progress in formal methods over the years, we remain very far from achieving these goals. For example, a freshman CS major will typically program circles around today's best program synthesizers. Debugging and verification tools rely on formal specifications, which are hard to provide in many important applications.}@p{Two critical components of the gap between human and machine programmers are that humans learn from experience, i.e., data, and can easily generalize from incomplete problem definitions. In this talk, I will present a general framework for formal methods, based on Bayesian statistical learning, that aims to eliminate these differences. In our framework, descriptions of programming tasks are seen to be "clues" towards a hidden (probabilistic) specification that fully defines the task. Large corpora of real-world programs are used to construct a statistical model that correlates specifications with the form and function of their implementations. The framework can be implemented in a variety of ways, but in particular, through a neural architecture called Bayesian variational encoder-decoders. Inferences made using the framework can be used to guide traditional algorithms for program synthesis and bug-finding.}@p{I will show that this data-driven approach can lead to giant leaps in the scope and performance of automated program synthesis and debugging algorithms.  Specifically, I will give a demo of Bayou, a system for Bayesian inductive synthesis of Java programs that goes significantly beyond the state of the art in program synthesis. I will also describe Salento, a debugging system based on our framework that can find subtle violations of API contracts without any kind of specification.}}
     @list{@p{Swarat Chaudhuri is an Associate Professor of Computer Science at Rice University. His research lies at the interface of programming systems and artificial intelligence. Much of his recent work is on program synthesis, the problem of automatically generating computer programs from high-level specifications.}})

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
          (@url{http://noamz.org/oplss16/refinements-notes.pdf}, especially Ch.3).}}
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

@(define-values (future-sems past-sems)
   (let ()
     (define-values (f-unsorted p-unsorted) (partition in-future? seminars))
     (values
         ;; future seminars in chronological order, so next seminar is first
         (map render-seminar
              (sort f-unsorted datetime<? #:key seminar-date))
         ;; past seminars in reverse chronological order, so most recent seminar is first
         (map render-seminar
              (sort p-unsorted datetime>? #:key seminar-date)))))

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
          @div[class: "col-md-12"]{
            @h3{Upcoming Seminars}
            @|future-sems|
            @hr[]
            @h3{Past Seminars}
            @|past-sems|
          }
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


