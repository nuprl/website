#lang scribble/html
@require["templates.rkt"]

@doctype{html}
@html[lang: "en"]{
  @header{Home}
  @body[id: "pn-top"]{
    @navbar{"Home"}
    @div[class: "jumbotron"]{
      @div[class: "container"]{
        @div[class: "row"]{
          @div[class: "col-md-12"]{
            @img[src: "img/prl.png" alt: "Programming Research Laboratory"]{
            @h1{Programming Research Laboratory}
            @h2{College of Computer and Information Science @a[href: "http://www.ccs.neu.edu/" target: "_blank"]{@span[class: "glyphicon glyphicon-link" style: "font-size: 64%;" aria-hidden: "true"]}}
            @h3{Northeastern University @a[href: "http://www.neu.edu/" target: "_blank"]{@span[class: "glyphicon glyphicon-link" style: "font-size: 62%;" aria-hidden: "true"]}}
            @h4{Boston}}}}}}

    @div[class: "pn-main-wrapper"]{
      @div[class: "container "]{
        @div[class: "content"]{
        @div[class: "row"]{
          @div[class: "col-md-10 col-md-offset-1"]{
            @p{@span[class: 'pn-highlight' style: "font-size:xx-large"]{@em{We believe that writing computer programs is the fundamental act of computer science, and that programming languages are therefore our fundamental tool.}}}
            @p{We seek a deeper understanding of this fundamental tool and how it should be used, and we seek to apply this understanding to the program design process, as well as to novel applications.}
            @p{We take a multi-faceted approach to the study of programming languages and programming methodology, incorporating elements of design, mathematics, experimental science, and engineering.}
            @p{We conduct research on all aspects of programming, including:}
            @ul[
              @li{the development of small and large programs}
              @li{the design, implementation, and analysis of programming languages}
              @li{programming environment tools}
            ]

            @p{Our research program is intertwined with our mission to train undergraduates and graduate students. We routinely exploit research results for our undergraduate courses, and we routinely find research challenges in our teaching.}
  }}}}}

  @footer{}
}}
