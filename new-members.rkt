#lang scribble/html
@require["templates.rkt"]

@doctype{html}
@html[lang: "en"]{
  @header{New Members}
  @body[id: "pn-top"]{
    @navbar{New Members}
    @subpage-title{New Members}
    @div[class: "pn-main-wrapper"]{
      @div[class: "content"]{
        @div[class: "container"]{
          @div[class: "row"]{
            @div[class: "col-md-10 col-md-offset-1"]{
              @p{Welcome to the PRL! The instructions below will help you get situated in the lab.}
              @ol[
                @li{
                  Get your student ID (a.k.a. Husky Card) from
                  @a[href: "https://www.northeastern.edu/huskycard/about/pick-up-locations/"]{a pick-up location}.
                  Visit WVH 202 and/or send mail to @mailto|{operations@ccs.neu.edu}| to register your card to unlock WVH 308, WVH 330, WVH 366, and your private office (if you have one).
                }
                @li{
                  Apply for a Khoury account using
                  @a[href: "https://www.ccis.northeastern.edu/systems/getting-started/"]{these instructions}.
                  This account comes with a @tt{ccs.neu.edu} email address and
                  allows you to access various computers around the college.
                  Make sure you are able to print using the @tt{gaugin} (in WVH 308)
                  or @tt{renoir} (outside WVH 366) printers; see @a[href: "https://its.northeastern.edu/services/network-printing/"]{this page}
                  for help, or visit the Khoury Systems Help Desk in WVH 312.
                }
                @li{
                  Coordinate with the lab member in charge of
                  @a[href: "people.html"]{the people page}
                  (see the @a[href: "contact.html"]{Contact} page) to add your
                  picture and bio. Contact also maintainers of the lab mailing lists to
                  add yourself to appropriate lists:
                  @a[href: "mailto:prl-all@googlegroups.com"]{@strong{prl-all}} for everyone,
                  @a[href: "mailto:prl-students@googlegroups.com"]{@strong{prl-students}} for students,
                  and @a[href: "mailto:prl-staff@googlegroups.com"]{@strong{prl-staff}}
                  for faculty, visiting faculty, and postdocs.
                }
                @li{
                  Join the lab's channel on matrix, @tt{#prl:@a[href: "https://matrix.org"]{matrix.org}}.
                  @strong{Prospective students are welcome to
                  ask questions on matrix, too!}
                }
                @li{
                  For grad students and postdocs: join the lab's slack channel, @url{https://prl-students.slack.com},
                  by contacting the contact manager.
                }
                @li{
                  Sign up for the mailing lists for the PL Seminar and the PL Jr.
                  seminar (links on the @a[href: "seminars.html"]{Seminars} page),
                  and check the @a[href: "https://calendar.google.com/calendar/embed?src=k4cg1vgb3l2n8r2ph4t01dmtpc%40group.calendar.google.com&ctz=America%2FNew_York"]{calendar} for upcoming talks.
                  Sign up for the Reading Group @a[href: "https://groups.google.com/forum/#!forum/prl-reading-group"]{mailing list}
                  (send a request with a Google account or contact the reading group maintainer).
                  You may also wish to subscribe to the
                  @a[href: "https://groups.google.com/a/eecs.harvard.edu/d/forum/programming"]{Harvard PL list} (@a[href: "https://calendar.google.com/calendar/embed?src=harvardplseminar@gmail.com"]{calendar})
                  and
                  @a[href: "https://lists.csail.mit.edu/mailman/listinfo/pl"]{MIT PL list}
                  to be notified about their talks.
                }
                @li{
                  If you need a private place to store code, there is both a
                  @a[href: "https://github.ccs.neu.edu/"]{Khoury-managed GitHub instance}
                  and a @a[href: NuPRL-GITHUB]{NuPRL organization}
                  on GitHub. To get access to the @tt{nuprl}
                  organization, ask the contact manager to add you as a member.
                }
                @li{
                  Follow @a[href: NuPRL-TWITTER @tt|{@neu_prl}|] on Twitter.
                }
                @li{For grad students: more information about Northeastern's Khoury College of Computer Sciences can be found on
                  @a[href: "https://wiki.ccs.neu.edu/display/phdhub"]{the PhD Hub wiki}, a successor of @a[href: "https://wiki.ccs.neu.edu/display/GRADWIKI/Home"]{the grad wiki} (login required).
                  Students are also welcome to join @a[href: "https://khouryphds.slack.com/"]{Khoury PHDs Slack}
                  (use your @tt{husky.neu.edu} email address)
                  and check out @a[href: "https://khoury-gsa.github.io/"]{Khoury GSA website}.
                }
                @li{
                  @p{
                    Finally, if you need a place to live, this map (compiled by
                    Northeastern CS grad students circa 2010) may help. Colors
                    on shaded regions indicate roughly how good the area is for
                    grad students (green = good, yellow = okay, red = bad), and
                    you can click on the regions and pins for more information.
                    Click @a[href: "https://drive.google.com/open?id=13h-W6XWFql35Cs6Xk_c9MnTEsRbAE1qP&usp=sharing"]{here} to edit the map.
                  }
                  @iframe[src: "https://www.google.com/maps/d/u/0/embed?mid=13h-W6XWFql35Cs6Xk_c9MnTEsRbAE1qP" width: "640" height: "480"]
                }
              ]
  }}}}}
  @footer{}
}}
