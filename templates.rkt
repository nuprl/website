#lang scribble/html

@provide[
  header
  navbar
  subpage-title
  person
  comment
  footer
  url
]

@require[
  (only-in racket/list add-between)
]

@; -----------------------------------------------------------------------------

@(define (comment . content)
   @literal[@list[" <!-- " content " --> "]])

@(define <!-- comment)

@(define (header title-str)
   @head{
     @meta[charset: "utf-8"]
     @meta[http-equiv: "X-UA-Compatible" content: "IE=edge"]
     @meta[name: "viewport" content: "width=device-width, initial-scale=1"]
     @title{@|title-str| - Programming Research Laboratory - Northeastern University}

     @<!--{Bootstrap}
     @link[href: "css/bootstrap.min.css" rel: "stylesheet"]
     @<!--{Custom css}
     @link[href: "css/custom.css" rel: "stylesheet"]
     @<!--{Fonts}
     @link[href: "http://fonts.googleapis.com/css?family=Ubuntu:300"
           rel: "stylesheet"
           type: "text/css"]
     @link[href: "http://fonts.googleapis.com/css?family=PT+Sans"
           rel: "stylesheet"
           type: "text/css"]

     @<!--{For IE 9 and below. ICO should be 32x32 pixels in size}
     @<!--{[if IE]><link rel="shortcut icon" href="img/favicon.ico"><![endif]}

     @<!--{Firefox, Chrome, Safari, IE 11+ and Opera. 196x196 pixels in size.}
     @link[rel: "icon" href: "img/favicon.png"]

     @<!--{HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries}
     @<!--{WARNING: Respond.js doesn't work if you view the page via file://}
     @<!--{[if lt IE 9]}
       @script[src: "https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"]
       @script[src: "https://oss.maxcdn.com/respond/1.4.2/respond.min.js"]
     @<!--{<![endif]} })

@(define (footer)
   @list{
     @element/not-empty['footer class: "footer"]{
       @p{© Copyright Programming Research Laboratory 2015-2016 | made by @a[href: "http://www.catchexception.cz/" target: "_blank"]{Catchexception s.r.o.}}
       @a[class: "pn-top pn-dark" href: "#pn-top"]{
         @img[src: "img/up-arrow.png" alt: "top"]}}
     @<!--{jQuery (necessary for Bootstrap's JavaScript plugins)}
     @script[src: "https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"]
     @<!--{Include all compiled plugins (below), or include individual files as needed}
     @script[src: "js/bootstrap.min.js"]
     @<!--{Custom scripts}
     @script[src: "js/custom.js"]})

@(define (navbar current-page)
   @element/not-empty['nav class: "navbar navbar-inverse"]{
     @div[class: "container"]{
       @div[class: "row"]{
         @div[class: "navbar-header"]{
           @button[type: "button" class: "navbar-toggle collapsed" data-toggle: "collapse" data-target: "#navbar" aria-expanded: "false" aria-controls: "navbar"]{
             @span[class: "sr-only"]{Toggle navigation}
             @span[class: "icon-bar"]
             @span[class: "icon-bar"]
             @span[class: "icon-bar"]}}
         @div[id: "navbar" class: "navbar-collapse collapse"]{
           @ul[class: "nav navbar-nav"]{
             @(for/list ([title-pair (in-list '(
                                       ("./" . "Home")
                                       ("people.html" . "People")
                                       ("teaching.html" . "Teaching")
                                       ("seminars.html" . "Seminars")
                                       ("software.html" . "Software")
                                       ("publications.html" . "Publications")
                                       ("new-members.html" . "New Members")
                                       ("contact.html" . "Contact")
                                       ("blog/index.html" . "Blog")))])
               (if (string=? (cdr title-pair) current-page)
                 @li[role: "presentation" class: "active"]{
                   @a[href: "#"]{@cdr[title-pair]}}
                 @li[role: "presentation"]{
                   @a[href: @car[title-pair]]{@cdr[title-pair]}}))}} }}})

@(define (subpage-title title)
   @div[class: "jumbotron subpages"]{
     @div[class: "container"]{
       @div[class: "row"]{
         @div[class: "col-md-12"]{
           @h1{@|title|}}}}})

@; website should include https or http
@;   should figure out a nice interface so it doesn't display the protocol on the page.
@(define (person #:name name
                 #:title title
                 #:e-mail e-mail
                 #:website [website #f]
                 #:history history
                 #:img     img-file
                 . bio)
  @div[class: "row pn-person"]{
    @div[class: "col-md-12 pn-row-eq-height"]{
      @div[class: "col-md-3 pn-photo"]{
        @div[class: "img-wrapper"]{
          @img[src: @format["img/~a" img-file] title: name alt: name]}}
      @div[class: "col-md-9"]{
        @div[class: "col-md-4 pn-contact"]{
          @span[class: "pn-name" name]
          @br{}
          @|title|
          @br{}
          @a[href: "mailto:@|e-mail|" e-mail]
          @br{}
          @a[href: website website]}
        @div[class: "col-md-5 pn-muted col-md-offset-3"]{
          @add-between[history @br{}]}
      @div[class: "col-md-12 pn-bio"]{
        @p{@|bio|}}}}})

@(define (url www)
   @a[href: www www])
