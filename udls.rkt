#lang scribble/html
@require["templates.rkt"]
@(define (udlsfooter)
   @list{
     @element/not-empty['footer class: "footer"]{
       @p{© Stolen from the Programming Research Laboratory 2018-2019 | made by @a[href: "http://www.catchexception.cz/" target: "_blank"]{Catchexception s.r.o.} (not us) | source on @a[href: "https://github.com/nuprl/website"]{GitHub}}
       @a[class: "pn-top pn-dark" href: "#pn-top"]{
         @img[src: "img/up-arrow.png" alt: "top"]}}
  })

@doctype{html}
@html[lang: "en"]{
  @header{UDLS}
   @body[id: "pn-top"]{
          @div[class: "container"]{
       @div[class: "row"]{
         @div[class: "navbar-header"]{@br{}@br{}@br{}}}}
       @subpage-title{Northeastern CCIS's Undistinguished Lecture Series (UDLS)}

     @div[class: "pn-main-wrapper"]{
       @div[style: "position:absolute;right:40px;top:20px;z-index:-1" class: "hidden-sm-down"]{
         @img[src: "udls/img/ufo.png"]
   }
       @div[class: "content"]{
         @div[class: "container"]{
           @div[class: "row"]{
            @h1{What is UDLS?}
             @p{@a[href: "https://www.ccis.northeastern.edu/" title: "Northeastern CCIS" "Northeastern CCIS"]'s UDLS is a semimonthly social activity for graduate students and postdocs. At UDLS, we believe public speaking is a skill that can be improved with practice. On alternating Fridays, we improve our horrific public speaking skills by giving presentations about topics not related to our research. Instead, we talk in great depth about things like rocketships and pugs.}
             @p{Then we eat free pizza and play games.}
            @h2{How do I Volunteer to Speak?}
             @p{Contact the hosts at @a[href: (format "mailto:~a" "hollenbeck.c@husky.neu.edu") title: "Warden of Lecturers" "hollenbeck.c@husky.neu.edu"] or @a[href: (format "mailto:~a" "miller.josh@husky.neu.edu") title: "Master of Games" "miller.josh@husky.neu.edu"].}
            @h1{Schedule}
             @h3{Spring 2018}
              @p{02/09/2018 Ming-Ho Yee: @a[href: "udls/ppt/A_History_of_Classical_Music.pdf" download:"true" title: "A History of Classical Music (PDF)" "A History of Classical Music"]}
              @p{01/26/2018 Celeste Hollenbeck: @a[href: "udls/ppt/UDLS_How_to_UDLS.pdf" download:"true" title: "How to UDLS (PDF)" "How to UDLS"] & How to Throw a Punch (a UDLS Double Feature)}
             
     }
    }
    @udlsfooter{}
   }
  }
 }
}