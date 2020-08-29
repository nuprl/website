#lang scribble/html

@provide[
  header
  navbar
  subpage-title
  person
  comment
  footer
  url
  mailto
  NuPRL-TWITTER
  NuPRL-GITHUB
]

@require[
  (only-in racket/list add-between)
]

@; -----------------------------------------------------------------------------

@(define NuPRL-TWITTER "https://twitter.com/neu_prl")
@(define NuPRL-GITHUB "https://github.com/nuprl")

@(define (comment . content)
   @literal[@list[" <!-- " content " --> "]])

@(define <!-- comment)

@(define (header title-str)
   @head{
     @meta[charset: "utf-8"]
     @meta[http-equiv: "X-UA-Compatible" content: "IE=edge"]
     @meta[name: "viewport" content: "width=device-width, initial-scale=1"]

     @; Be friendly to embedding on social media platforms
     @meta[property: "og:title" content: "NEU PRL"]
     @meta[property: "og:description"
           content:  "Programming Research Laboratory @ Northeastern&nbsp;University"]
     @meta[property: "og:image" content: "img/prl-bg.png"]

     @title{@|title-str| - Programming Research Laboratory - Northeastern University}

     @<!--{Bootstrap}
     @link[href: "css/bootstrap.min.css" rel: "stylesheet"]
     @<!--{Custom css}
     @link[href: "css/custom.css" rel: "stylesheet"]
     @<!--{Fonts}
     @link[href: "https://fonts.googleapis.com/css?family=Ubuntu:300"
           rel: "stylesheet"
           type: "text/css"]
     @link[href: "https://fonts.googleapis.com/css?family=PT+Sans"
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
       @p[@twitter-follow-button["neu_prl" "Follow the PRL"]]
       @p{© Copyright Programming Research Laboratory 2015-2019 | made by Catchexception s.r.o. | source on @a[href: "https://github.com/nuprl/website"]{GitHub}}
       @a[class: "pn-top pn-dark" href: "#pn-top"]{
         @img[src: "img/up-arrow.png" alt: "top"]}}
     @<!--{jQuery (necessary for Bootstrap's JavaScript plugins)}
     @script[src: "https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"]
     @<!--{Include all compiled plugins (below), or include individual files as needed}
     @script[src: "js/bootstrap.min.js"]
     @<!--{Custom scripts}
     @script[src: "js/custom.js"]
     @script[src: "js/analytics.js"]
  })

@; Copied from `frog/widgets.rkt`
@(define (twitter-follow-button name label)
   @list[
     @a[href: (string-append "https://twitter.com/" name)
        class: "twitter-follow-button"
        data-show-count: "false"
        data-lang: "en" label]
     @script[type: "text/javascript" @literal|{
       !function(d,s,id){
           var js,fjs=d.getElementsByTagName(s)[0];
           if(!d.getElementById(id)){
               js=d.createElement(s);
               js.id=id;
               js.src="https://platform.twitter.com/widgets.js";
               fjs.parentNode.insertBefore(js,fjs);
           }
       }(document,"script","twitter-wjs");
     }|]])

@(define nav-template ; (Listof (List String String))
   '(("./" . "Home")
     ("people.html" . "People")
     ("teaching.html" . "Teaching")
     ("seminars.html" . "Seminars")
     ("software.html" . "Software")
     ("publications.html" . "Publications")
     ("new-members.html" . "New Members")
     ("contact.html" . "Contact")
     ("blog/index.html" . "Blog")))

@(define (navbar current-page)
   (define rendered-nav-elements
     (for/list ([title-pair (in-list nav-template)])
       (if (string=? (cdr title-pair) current-page)
         @li[role: "presentation" class: "active"]{@a[href: "#"]{@cdr[title-pair]}}
         @li[role: "presentation"]{@a[href: @car[title-pair]]{@cdr[title-pair]}})))
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
           @ul[class: "nav navbar-nav" rendered-nav-elements] }}}})

@(define (subpage-title title)
   @div[class: "jumbotron subpages"]{
     @div[class: "container"]{
       @div[class: "row"]{
         @div[class: "col-md-12"]{
           @h1[title]}}}})

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
          @a[href: (format "mailto:~a" e-mail) e-mail]
          @br{}
          @a[href: website website]}
        @div[class: "col-md-5 pn-muted col-md-offset-3"]{
          @add-between[history @br{}]}
      @div[class: "col-md-12 pn-bio"]{
        @p{@|bio|}}}}})

@(define (url www)
   @a[href: www www])

@(define (mailto email)
   @a[href: (string-append "mailto:" email) (tt email)])
