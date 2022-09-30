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

                @div[class: "row"]{
                  @div[class: "col-xs-6 col-md-4 pad-contact"]{
                    @h4{PL Seminar and Internal Talks} @a[href: "mailto:a.guha@northeastern.edu"]{Arjun Guha}, @a[href: "mailto:gouwar.j@northeastern.edu"]{John Gouwar}, @a[href: "mailto:johnli0135@gmail.com"]{John Li}
                  }

                  @div[class: "col-xs-6 col-md-4 pad-contact"]{
                    @h4{PL Seminar, Junior @br{} 
                      @small{@linksym["Website" "https://github.com/nuprl/prl-seminar-junior"]}} 
                    @a[href: "mailto:johnli0135@gmail.com"]{John Li}
                  }

                  @div[class: "col-xs-6 col-md-4 pad-contact"]{
                    @h4{ZED-talks} @a[href: "mailto:gokhale.sa@northeastern.edu"]{Satyajit Gokhale}, @a[href: "mailto:gouwar.j@northeastern.edu"]{John Gouwar}
                  }
                }

                @div[class: "col-xs-6 col-md-12 pad-contact"]{
                  @h4{Applications for Internship, PhD, Postdoc} @a[href: "mailto:j.vitek@neu.edu"]{Jan Vitek}
                }
              }

              @div[class: "row"]{
                @h3{Website}

                @div[class: "row"]{
                  @div[class: "col-xs-6 col-md-4 pad-contact"]{
                    @h4{Webmaster} @a[href: "mailto:phipps-costin.l@northeastern.edu"]{Luna Phipps-Costin}
                  }

                  @div[class: "col-xs-6 col-md-4 pad-contact"]{
                    @h4{Blog Editor} @a[href: "mailto:amidon.p@northeastern.edu"]{Lucy Amidon}
                  }

                  @div[class: "col-xs-12 col-md-4 pad-contact"]{
                    @h4{Publication Page Maintainer} @a[href: "mailto:katiehough19@gmail.com"]{Katie Hough}
                  }
                }

                @div[class: "row"]{
                  @div[class: "col-xs-12 col-md-6 pad-contact"]{
                    @h4{People Page Maintainer} @a[href: "mailto:ahwagner@ccs.neu.edu"]{Andrew Wagner}
                  }

                  @div[class: "col-xs-12 col-md-6 pad-contact"]{
                    @h4{Teaching & Software Pages Maintainer} @a[href: "mailto:ahwagner@ccs.neu.edu"]{Andrew Wagner}
                  }
                }
              }

              @div[class: "row"]{
                @h3{Social}

                @div[class: "row"]{
                  @div[class: "col-xs-12 col-md-4 pad-contact"]{
                    @h4{PRL Social} @a[href: "mailto:michelledt@ccs.neu.edu"]{Michelle Thalakottur}, @a[href:"mailto: mhyee@ccs.neu.edu"]{Ming-Ho Yee} @br{}
                    PRL parties, tea time, bingewatching
                  }

                  @div[class: "col-xs-12 col-md-4 pad-contact"]{
                    @h4{Twitter @br{} 
                      @small{@linksym["@neu_prl" "https://twitter.com/neu_prl"]}} 
                      @a[href: "mailto:a.pelenitsyn@gmail.com"]{Artem Pelenitsyn}, @a[href: "mailto:sam@stites.io"]{Sam Stites}
                  }
                }

                @div[class: "row"]{
                  @div[class: "col-xs-12 col-md-6 pad-contact"]{
                    @h4{Coffee Czar} @a[href: "mailto:cho.mins@northeastern.edu"]{Minsung Cho}
                  }

                  @div[class: "col-xs-12 col-md-6 pad-contact"]{
                    @h4{Tea Tzar} @a[href: "mailto:gierczak.o@northeastern.edu"]{Olek Gierczak}
                  }
                }
              }

              @div[class: "row"]{
                @h3{Contact Managers}

                @div[class: "col-xs-12 col-md-6 pad-contact"]{
                  @a[href: "mailto:mail@camoy.name"]{Cameron Moy} @br{}
                  Mailing Lists: @tt{prl-all}, @tt{prl-students}@br{}
                  GitHub Organization: @a[href: "https://github.com/nuprl"]{@tt{NuPRL}}
                }

                @div[class: "col-xs-12 col-md-6 pad-contact"]{
                  @a[href: "mailto:vitekj@me.com"]{Jan Vitek} @br{}
                  Mailing List: @tt{prl-staff}
                }
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
