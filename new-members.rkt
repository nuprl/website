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
                  Apply for a CCIS account using
                  @a[href: "https://www.ccis.northeastern.edu/systems/getting-started/"]{these instructions}.
                  This account comes with a @tt{ccs.neu.edu} email address and
                  allows you to access various computers around the college.
                  Make sure you are able to print using the @tt{gaugin} (in WVH 308)
                  or @tt{renoir} (outside WVH 366) printers; see @a[href: "https://its.northeastern.edu/services/network-printing/"]{this page}
                  for help, or visit the CCIS Help Desk in WVH 312.
                }
                @li{
                  Coordinate with the lab member in charge of
                  @a[href: "people.html"]{the people page}
                  (see the @a[href: "contact.html"]{Contact} page) to add your
                  picture and bio. Contact also the @strong{prl-students} mailing list 
                  maintainer and add yourself to
                  that mailing list (and contact the @strong{prl-staff} mailing list
                  maintainer to add yourself there, if appropriate.)
                }
                @li{
                  Join the lab's IRC channel, @tt{#prl}, on Freenode. Current
                  lab members can help you get started on IRC if you've never
                  used it before. @strong{Prospective students are welcome to
                  ask questions on IRC, too!}
                }
                @li{
                  Sign up for the mailing lists for the PL Seminar and the PL Jr.
                  seminar (links on the @a[href: "seminars.html"]{Seminars} page),
                  and check the @a[href: "https://calendar.google.com/calendar/embed?src=k4cg1vgb3l2n8r2ph4t01dmtpc%40group.calendar.google.com&ctz=America%2FNew_York"]{calendar} for upcoming talks.
                  You may also wish to subscribe to the
                  @a[href: "https://groups.google.com/a/eecs.harvard.edu/d/forum/programming"]{Harvard PL list} (@a[href: "https://calendar.google.com/calendar/embed?src=harvardplseminar@gmail.com"]{calendar})
                  and
                  @a[href: "https://lists.csail.mit.edu/mailman/listinfo/pl"]{MIT PL list}
                  to be notified about their talks.
                }
                @li{
                  If you need a private place to store code, there is both a
                  @a[href: "https://github.ccs.neu.edu/"]{CCIS-managed GitHub instance}
                  and a @a[href: NuPRL-GITHUB]{NuPRL organization}
                  on GitHub. To get access to the @tt{nuprl}
                  organization, ask the webmaster to add you as a member.
                }
                @li{
                  Follow @a[href: NuPRL-TWITTER @tt|{@neu_prl}|] on Twitter.
                }
                @li{
                  @p{
                    Finally, if you haven't already found a place to live, this
                    map (compiled by Northeastern CS grad students) can help
                    you narrow down where to live in Boston. Colors on shaded
                    regions indicate roughly how good the area is for grad
                    students (green = good, yellow = okay, red = bad), and you
                    can click on the regions and pins for more information
                  }
                  @iframe[src: "https://www.google.com/maps/d/u/0/embed?mid=zNp1Yae6mers.kT4sztNf9NgU" width: "640" height: "480"]
                  @p{
                    You might also find these maps (not compiled by grad students, or affiliated in any way with Northeastern University or the Programming Research Lab) helpful:
                    @ul{
                      @li{@url{https://www.jefftk.com/apartment_prices/index#2018-08-18&2}}
                      @li{@url{https://www.trulia.com/local/boston-ma/tiles:1%7Cpoints:1_crime}}}
                  }
                }
              ]
              @p{More grad-student-specific information about Northeastern's College of Computer and Information Science can be found on @a[href: "https://wiki.ccs.neu.edu/display/GRADWIKI/Home"]{the grad wiki}.}
  }}}}}
  @footer{}
}}
