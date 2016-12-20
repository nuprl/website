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
                  @a[href: "http://www.northeastern.edu/huskycard/about/locations.html"]{a pick-up location}.
                  Your ID unlocks the door into the lab.
                }
                @li{
                  Apply for a CCIS account using
                  @a[href: "http://howto.ccs.neu.edu/howto/accounts-homedirs/how-to-sign-up-for-a-ccis-account/"]{these instructions}.
                  This account comes with a @tt{ccs.neu.edu} email address and
                  allows you to access various computers around the college.
                }
                @li{
                  Coordinate with the lab member in charge of
                  @a[href: "people.html"]{the people page}
                  (see the @a[href: "contact.html"]{Contact} page) to add your
                  picture and bio, as well as to add yourself to the
                  @strong{prl-students} and/or @strong{prl-staff} mailing list,
                  as appropriate.
                }
                @li{
                  Join the lab's IRC channel, @tt{#prl}, on Freenode. Current
                  lab members can help you get started on IRC if you've never
                  used it before. @strong{Prospective students are welcome to
                  ask questions on IRC, too!}
                }
                @li{
                  Sign up for the mailing lists for the PL Seminar and the PL Jr.
                  seminar (links on the @a[href: "seminars.html"]{Seminars} page).
                  You may also wish to subscribe to the
                  @a[href: "https://lists.eecs.harvard.edu/mailman/listinfo/programming"]{Harvard}
                  and
                  @a[href: "https://lists.csail.mit.edu/mailman/listinfo/pl"]{MIT}
                  PL mailing lists to be notified about their talks.
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
                }
              ]
              @p{More grad-student-specific information about Northeastern's College of Computer and Information Science can be found on @a[href: "https://wiki.ccs.neu.edu/display/GRADWIKI/Home"]{the grad wiki}.}
  }}}}}
  @footer{}
}}
