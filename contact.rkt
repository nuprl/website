#lang scribble/html
@require["templates.rkt"]

@(define (linksym name link) 
  @a[href: link]{@name @span[class: "glyphicon glyphicon-link"]})

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

              @div[class: "row"]{
                @h3{Inside PRL}

                @div[class: "col-xs-3 pad-contact"]{
                  @h4{PL Seminar} @a[href: "mailto:yazdani.n@husky.neu.edu"]{Nathaniel Yazdani}
                }

                @div[class: "col-xs-3 pad-contact"]{
                  @h4{PL Seminar, Junior @br{} 
                    @small{@linksym["Link" "https://github.com/nuprl/prl-seminar-junior"]}} 
                  @a[href: "mailto:artem@ccs.neu.edu"]{Artem Pelenitsyn}, @a[href: "mailto:julbinb@gmail.com"]{Julia Belyakova}
                }

                @div[class: "col-xs-3 pad-contact"]{
                  @h4{ZED-talks} @a[href: "mailto:alexi@ccs.neu.edu"]{Alexi Turcotte}, @a[href: "mailto:ellen@ccs.neu.edu"]{Ellen Arteca}
                }

                @div[class: "col-xs-3 pad-contact"]{
                  @h4{Applications for internship, PhD, Postdoc} @a[href: "mailto:j.vitek@neu.edu"]{Jan Vitek}
                }
              }

              @div[class: "row"]{
                @h3{Website}

                @div[class: "col-xs-4 pad-contact"]{
                  @h4{Webmaster} @a[href: "mailto:michael.ballantyne@gmail.com"]{Michael Ballantyne}
                }

                @div[class: "col-xs-4 pad-contact"]{
                  @h4{Blog Maintainer} @a[href: "mailto:moy.cam@husky.neu.edu"]{Cameron Moy}
                }

                @div[class: "col-xs-4 pad-contact"]{
                  @h4{Publication Page Maintainer} @a[href: "mailto:artem@ccs.neu.edu"]{Artem Pelenitsyn}
                }

                @div[class: "col-xs-6 pad-contact"]{
                  @h4{People Page Maintainer} @a[href: "mailto:julbinb@gmail.com"]{Julia Belyakova}
                }

                @div[class: "col-xs-6 pad-contact"]{
                  @h4{Teaching & Software Pages Maintainer} @a[href: "mailto:julbinb@gmail.com"]{Julia Belyakova}
                }
              }

              @div[class: "row"]{
                @h3{Social}

                @div[class: "col-xs-3 pad-contact"]{
                  @h4{Happy Hour} @a[href: "mailto:weiss.a@husky.neu.edu"]{Aaron Weiss}
                }

                @div[class: "col-xs-3 pad-contact"]{
                  @h4{Coffee Czar} @a[href: "mailto:types@ccs.neu.edu"]{Ben Greenman}
                }

                @div[class: "col-xs-3 pad-contact"]{
                  @h4{Tea Czar} @a[href: "mailto:gierczak.o@husky.neu.edu"]{Olek Gierczak}
                }

                @div[class: "col-xs-3 pad-contact"]{
                  @h4{Twitter Tsar @br{} 
                    @small{@linksym["@neu_prl" "https://twitter.com/neu_prl"]}} 
                    @a[href: "mailto:mhyee@ccs.neu.edu"]{Ming-Ho Yee}
                }
              }

              @div[class: "row"]{
                @h3{Contact Manager}
                @a[href: "mailto:julbinb@gmail.com"]{Julia Belyakova} @br{}
                Mailing Lists: @tt{prl-students}, @tt{prl-staff} @br{}
                GitHub Organization: @a[href: "https://github.com/nuprl"]{@tt{NuPRL}}
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
              The @b{Khoury College of Computer Sciences}
              is located in Building WVH
              (aka West Village "H" — don't forget the H,
              otherwise you won't be able to distinguish
              it from WV "A" through WV "G").
            }

            @p{
              We are diagonally across the street from the Museum of Fine Arts.
            }

            @p{
              To get to all Khoury offices, take the elevators
               opposite the big glassed-in lab on the first floor.
            }

            @p{
              The administrative offices and mail boxes are located
              in Room 202 WVH, to your right as you get off the elevator
              on the second floor.
            }

            @p{
              The PRL lab is spread across rooms 308 and 330. 
              Room 308 is behind the glass walls with Racket code on them.
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
