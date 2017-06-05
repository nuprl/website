    Title: Report: PLISS 2017
    Date: 2017-06-05T15:47:59
    Tags: pliss, event, by Ming-Ho Yee

Two weeks ago, I attended the first [Programming Language Implementation Summer
School][PLISS], held in beautiful Bertinoro, Italy.

The goal of PLISS was "to prepare early graduate students and advanced
undergraduates for research in the field," and I think it successfully
accomplished that. There were many talks in a variety of areas, such as
just-in-time compilers, garbage collection, static analysis, and distributed
systems. But PLISS was more than just a series of talks: PLISS provided an
environment for interacting with other students as well as senior researchers.

<!-- more -->

## The Talks

With the amount of technical content at PLISS, there was easily something for
everyone. [Jan Vitek][jv] and [Laurence Tratt][lt] gave lectures that included
hands-on exercises where we worked on JITs. [Suresh Jagannathan][sj] dived into
the operational semantics of a distributed system, so we could reason about
different weak consistency models. Francesco Logozzo gave us a whirlwind tour of
abstract interpretation.

Most of my favorite talks included some form of extra content, such as
exercises, live-coding presentations, or demos. I found it really helpful to
write actual code and apply what I had just learned, or to look at some concrete
examples. The examples and exercises also helped with the pacing, as actively
listening to four 90-minute talks every day is exhausting!

Off the top of my head, these were some of my favorite talks:

  - **Dynamic Programming Language Implementation with LLVM**, by Petr Maj, Oli
    Flückiger, and [Jan Vitek][jv]. As the first talk of the summer school, this
    was a gentle introduction for the rest of the week. We had [exercises][rift]
    (with intentional bugs to make us think!), and also brief overviews of
    intermediate languages, static analysis, and garbage collection. These three
    topics would later show up in more detail.

  - **Micro Virtual Machines**, by [Steve Blackburn][sb]. This talk covered
    background information on virtual machines, and also the [Micro VM][microvm]
    project that Steve's group has been working on. A lot of the material was
    already familiar to me, but I still enjoyed the talk, and even got a few
    ideas for the project I'm working on!

  - **Static Analysis**, by [Matt Might][mm]. Matt's talk was based on one of
    his [articles][mm-sa] and an older talk he's given. Impressively, the entire
    example was live-coded, with only a single mistake!

  - **Testing Language Implementations**, by [Alastair Donaldson][ad]. This was
    an entertaining talk, since Ally showed multiple examples of crashing
    compilers, and causing other kinds of mischief by triggering compiler bugs.

If you're disappointed that you couldn't see these talks, don't worry! The talks
were recorded and will be posted very shortly.

## The People

But there's more to PLISS than the talks. I'm referring to *networking*, or the
opportunity to get out and talk to other people about research.

As an early graduate student, I've been given a lot of advice about talking to
people at conferences and the importance of the "hallway track." I still have
difficulty doing this at an actual conference, like [PLDI][] or [ECOOP][]. When
there are hundreds of attendees, or when people already know each other and are
in conversation groups, I find it difficult to approach them.

This was not the case at PLISS. There were fewer attendees: about fifty students
and a dozen speakers. There was a good mix of undergraduate, master's,
first-year PhD, and more senior PhD students. All our breakfasts, lunches, and
breaks were together, so we would see the same people again and again, and
inevitably start to learn each other's names. The speakers would also be among
us, and there was a good ratio of speakers to students for discussions and
mealtime mentoring.

I had many opportunities to practice my "research pitch." I talked to senior
students and got advice. I talked to junior students and gave advice. Two
different people I talked to about my research pointed me to the same paper to
read. I found another student who was working with [IFDS][], an algorithm I have
spent much time trying to understand. And, one day at lunch, my table discovered
that we were all working on static analysis!

As much as I enjoyed the talks, I think the best part of PLISS was meeting and
talking to other people. You can replace talks with videos (but you lose the
speaker-audience interaction), and you can replace conversations with other
forms of communication. But there isn't really anything that can replace the
serendipity of bumping into someone with a shared interest.

## The Location

Actually, the *other* best part of PLISS was the location. Italy is a beautiful
country with delicious food. And Bertinoro is a small town on the top of a hill,
with a breathtaking view of the surrounding area. The lectures were held in
a [castle at the top of the hill][castle] (photo credit: Steve Blackburn). The
speakers lived in the castle for the week, while the students lived in the
former monastery (seems fitting), which has been renovated into a university
residence.

Here are my two favorite pictures I took (click for full size):

[![View from the castle](/img/pliss2017-1-thumb.jpg)](/img/pliss2017-1.jpg)

[![Panorama](/img/pliss2017-2-thumb.jpg)](/img/pliss2017-2.jpg)

Steve Blackburn has more pictures posted on the [PLISS website][PLISS].

## Final Thoughts

PLISS was a wonderful event. Many thanks need to be given to the speakers,
organizers, and sponsors, for making this possible!

If and when there is a second PLISS, I highly encourage students to apply! You
will learn a lot from the lectures, from talking to the speakers, and meeting
other students. And if it's in Bertinoro again, you can enjoy the weather and
nice view!


[PLISS]: https://pliss2017.github.io/
[jv]: http://janvitek.org/
[lt]: http://tratt.net/laurie/
[sj]: https://www.cs.purdue.edu/homes/suresh/
[rift]: https://github.com/PRL-PRG/pliss-rift/
[sb]: http://users.cecs.anu.edu.au/~steveb/
[microvm]: http://microvm.github.io/
[mm]: http://matt.might.net/
[mm-sa]: http://matt.might.net/articles/intro-static-analysis/
[ad]: http://multicore.doc.ic.ac.uk/
[PLDI]: http://pldi17.sigplan.org/home
[ECOOP]: http://2017.ecoop.org/
[IFDS]: http://research.cs.wisc.edu/wpis/papers/popl95.pdf
[castle]: https://pliss2017.github.io/images/pics/7.jpg
