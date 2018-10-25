#lang scribble/html
@require["templates.rkt"]

@doctype{html}
@html[lang: "en"]{
  @header{Contact}
  @body[id: "pn-top"]{
    @navbar{Contact}
      @subpage-title{Contact}

    @div[class: "pn-main-wrapper"]{
      @div[class: "content"]{
        @div[class: "container"]{
          @center{
            @div[class: "row"]{
              @h2{Who's in charge?}
              @div[class: "col-xs-3"]{
                @h4{Webmaster} @a[href: "mailto:leif@leifandersen.net"]{Leif Andersen}
                @br{}
                @br{}
              }

              @div[class: "col-xs-3"]{
                @h4{PL Seminar} @a[href: "mailto:goel.aviral@gmail.com"]{Aviral Goel}
                @br{}
                @br{}
              }

              @div[class: "col-xs-3"]{
                @h4{PL Seminar, Junior} @a[href: "mailto:shin.hy@husky.neu.edu"]{Hyeyoung Shin}
                @br{}
                @br{}
              }

              @div[class: "col-xs-3"]{
                @h4{Happy Hour} ???
                @br{}
                @br{}
              }
            }

            @div[class: "row"]{
              @div[class: "col-xs-3"]{
                @h4{Coffee Czar} @a[href: "mailto:alexi@ccs.neu.edu"]{Alexi Turcotte}
                @br{}
                @br{}
              }

              @div[class: "col-xs-3"]{
                @h4{Twitter Tsar} @a[href: "mailto:mhyee@ccs.neu.edu"]{Ming-Ho Yee}
                @br{}
                @br{}
              }

              @div[class: "col-xs-3"]{
                @h4{Blog Maintainer} @a[href: "mailto:samc@ccs.neu.edu"]{Sam Caldwell}
                @br{}
                @br{}
              }

              @div[class: "col-xs-3"]{
                @h4{People Page Maintainer} @a[href: "mailto:julbinb@gmail.com"]{Julia Belyakova}
                @br{}
                @br{}
              }
            }

            @div[class: "row"]{
              @div[class: "col-xs-4"]{
                @h4{Publication Page Maintainer} @a[href: "mailto:artem@ccs.neu.edu"]{Artem Pelenitsyn}
                @br{}
                @br{}
              }

              @div[class: "col-xs-4"]{
                @h4{Software Page Maintainer} @a[href: "mailto:leif@ccs.neu.edu"]{Leif Andersen}
                @br{}
                @br{}
              }

              @div[class: "col-xs-4"]{
                @h4{Teaching Page Maintainer} @a[href: "mailto:andrew.cobb@gmail.com"]{Andrew Cobb}
                @br{}
                @br{}
              }
            }

            @div[class: "row"]{
              @div[class: "col-xs-6"]{
                @h4{Applications for internship, PhD, Postdoc} @a[href: "mailto:j.vitek@neu.edu"]{Jan Vitek}
                @br{}
                @br{}
              }

              @div[class: "col-xs-6"]{
                @h4{Mailing List Maintainers} @a[href: "mailto:julbinb@gmail.com"]{Julia Belyakova} (@tt{prl-students}) @br{} @a[href: "mailto:ckfinite@gmail.com"]{Benjamin Chung} (@tt{prl-staff})
                @br{}
                @br{}
              }
            }
          }
          

          @h2[id: "directions"]{How to find us}
          @p{
            @div[class: "pull-left" style: "width: 200px"]{
              @pre[style: "margin: 10px"]{
                440 Huntington Ave,
                Boston, MA 02115
              }
            }
            @p{
              The @b{College of Computer and Information Science}
              is located in Building WVH
              (aka West Village "H" -- don't forget the H,
              otherwise you won't be able to distinguish
              it from WV "A" through WV "G").
            }

            @p{
              We are diagonally across the street from the Museum of Fine Arts.
            }

            @p{
              To get to all CCIS offices, take the elevators
               opposite the big glassed-in lab on the first floor.
            }

            @p{
              The administrative offices and mail boxes are located
              in Room 202 WVH, to your right as you get off the elevator
              on the second floor.
            }

            @p{
              The PRL lab is in room 308 behind the glass walls
              with Racket code on them.
            }

            @p{
              Talks are often in room 366, located to your right as
              you get off the elevator on the third floor.
            }

            @br{}
        }
      }
      @footer{}
}}}}
