#lang scribble/html
@require["templates.rkt"]

@doctype{html}
@html[lang: "en"]{
  @header{Software}
  @body[id: "pn-top"]{
    @navbar{Software}
    @subpage-title{Software}

    @div[class: "pn-main-wrapper"]{
      @div[class: "content"]{
        @div[class: "container"]{
          @div[class: "row"]{
            We design, implement, and maintain software systems.
            @div[class: "col-md-12"]{
              @h1[id: "pn-anchor-home"]{@a[href: "http://racket-lang.org/"]{
                @img[height: 40 src: "http://racket-lang.org/logo-and-text.png"]}}}
            @div[class: "col-md-12"]{
              @p{
                The Racket language is a vehicle for most of our research and teaching.
              }
              @br{}
              @strong{Racket} is a full-spectrum programming language. It goes beyond Lisp and Scheme with dialects that support objects, types, laziness, and more. Racket enables programmers to link components written in different dialects, and it empowers programmers to create new, project-specific dialects. Racket's libraries support applications from web servers and databases to GUIs and charts.
            }

            @div[class: "col-md-12"]{
              @h1[id: "pn-anchor-home"]{@a[href: "http://fiji-systems.com/"]{
                @img[height: 60 src: "http://janvitek.github.io/img/fiji.gif"]}}}
            @div[class: "col-md-12"]{
              @p{The Fiji real-time JVM and Fiji C1 compiler run on on a broad range of HW/OS (ARM and ERC32 to PowerPC and x86/x86_64, from RTEMS to Linux or Darwin), execute Java with deterministic garbage collection, or safe GC-less allocation.}}
            @div[class: "col-md-12"]{
              @h1[id: "pn-anchor-home"]{@a[href: "http://plg.uwaterloo.ca/~dynjs/"]{
                @img[height: 40 src: "http://janvitek.github.io/img/dynjs.png"]}}}
            @div[class: "col-md-12"]{
              @p{This project intends to analyze the dynamic behavior of JavaScript programs and its implications on analyses and security.}}


            @div[class: "col-md-12"]{
              @h1[id: "pn-anchor-home"]{@a[href: "https://github.com/allr/"]{Reactor}}}
            @div[class: "col-md-12"]{
              @p{Tools related to the R programming language including the Purdue implementation of the FastR virtual machine, timeR, testR and benchR.}}

            @div[class: "col-md-12"]{
              @h1[id: "pn-anchor-home"]{
                @a[href: "http://larcenists.org"]{
                  @img[style: "background:white;" height: 60 src: "http://larcenists.org/images/larceny.png"]}}}
            @div[class: "col-md-12"]{
              @p{Larceny is an implementation of the Scheme programming language written by
                 Will Clinger and his students.}}

    }}}
    @footer{}
}}}
