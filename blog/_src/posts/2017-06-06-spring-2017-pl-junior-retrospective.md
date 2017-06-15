    Title: Spring 2017 PL Junior Retrospective
    Date: 2017-06-06T11:38:25
    Tags: DRAFT, PL Junior, by Ben Chung, by Milo Davis, by Ming-Ho Yee, by Matt Kolosick, by Dustin Jamner, by Artem Pelenitsyn, by Julia Belyakova, by Sam Caldwell

The [PL Junior Seminar][pljunior] is for beginning PhD as well interested
undergrad and masters students to understand the foundations of
programming languages research as well as investigate the different
areas that modern research focuses on. The seminar provides an
opportunity to fill in background knowledge one is often assumed to
already have when starting a PhD or to acquire during the course of a
PhD.

For the spring 2017 instance of PL Junior we chose program synthesis,
the sequence calculus, and logic programming as topics we wanted to
learn more about. We also did two group paper readings for Luca
Cardelli's [Typeful Programming][typeful] and Alan Kay's
[Early History of Smalltalk][smalltalk]. At the same time, we changed
up the format from the previous semester.

<!-- more -->


## Format

As discussed in [last fall's retrospective][fall16retro], we wanted to
move from group reading and discussion towards weekly presentations.
Reading a paper or some other material in order to understand it well
enough to present is quite a different experience compared to the
effort that goes in when it is just for a group discussion (in our
experience). With any luck, the presentation will convey some of this
deeper knowledge to the rest of the group, with the result being that
the speaker has a better understanding and everyone else has about the
same understanding compared to when everyone just reads the paper.

One idea from last semester that we decided to keep is to spend a
length of time (possibly an entire semester) on a topic rather than
having a new topic each week. Staying on the same theme helps with
retention as well as allowing for deeper investigation.

In that spirit, we chose three themes for the semester: program
synthesis, the sequent calculus, and logic programming. Mostly by
chance, these topics have interesting connections to each other, and
we even had several PL Grown-Up Seminars this semester on program
synthesis!

## Synthesis

The first paper on program synthesis that we looked at was
[A Deductive Approach to Program Synthesis][deductive] by Zohar and
Manna. We chose this paper because it's old and has a lot of citations
so it's probably Important. It was interesting and provided an OK
introduction to proof search but the method presented seems far
removed from modern synthesis techniques.

The next paper was
[Programmatic and Direct Manipulation, Together][progdirect] by Chugh
et al which presents the [Sketch-n-Sketch][sketchnsketch] system.
Sketch-n-Sketch is a cool system. It demonstrates that a narrow
application of synthesis - trying to fill in the constant values in a
program (sketching) - can be used for great effect. We were left
wondering, however, if it was too narrow an application of synthesis
to give much of an indication of what the entire field is like.

We concluded our program synthesis segment with
[Type-and-Example-Directed Program Synthesis][typeandexample] by Osera
and Zdancewic, another relatively recent paper. This seems like a
relevant paper because we are under the impression that using examples
to do synthesis is a big thing right now. Using types to constrain the
search is another interesting perspective on techniques for synthesis.

While each of theses papers had merits, none was so comprehensive as
to be a necessary inclusion in any future look at program synthesis
for pl junior

## Sequent Calculus

We followed up the program synthesis unit with a week on the sequent
calculus. The seminar presentation was based on a paper by
[Herbelin][Herbelin]. [Gabriel’s thesis][gab] (chapter 4) includes
maybe a more suitable modern introduction to the sequent calculus.

It might have been better to do sequent calculus first because there
is a modern branch of proof search based on the sequent calculus.
Presenting this first would have allowed us to look into proof search
for program synthesis.

An additional problem is that it was insufficiently motivated. Either
skipping the topic or spending more time on it would be preferable,
since one week was just enough to describe the sequent calculus but
not enough to apply it.

## Logic Programming

The topic was presented over two weeks. The first session
presented/demoed Prolog as a language, and we got a sense of what
logic programming could do. But it was a whirlwind tour, and we were
left wondering about specific details (how proof search runs, what
`cut` does).

The second session presented the paper
[The Semantics of Predicate Logic as a Programming Language][predlogic].
It was interesting and insightful but left wondering how it relates to
the implementation of real logic programming languages.

In hindsight this was about as far as we could have gotten in just two
weeks.

## Bonus Rounds

We also used a few weeks to read and discuss specific papers as a
group.

The first paper we read was Cardelli's [Typeful Programming][typeful].
We picked typeful programming because Matthias has mentioned on
occasion how important he thinks it is.

It was an interesting read; more of an essay than a paper. It really
stood out as different from the other academic publications that we
have looked at. It’s a walk through of a language design motivating
each design decision in practical terms, as in things that actually
help the programmer.

Cardelli thinks polymorphism (subtyping in addition to parametric) is
important as well as features for programming in the large such as
modules and interfaces. Several features are interesting in their
omission, like type inference and macros.

After reading it it’s not clear why Matthias thinks it’s so important.
Maybe for historical reasons?

The other paper we read as a group was Alan Kay's
[The Early History of Smalltalk][smalltalk]. It seems like the
Smalltalk project investigated a plethora of interesting ideas about
designing programming languages and environments. This article seems
to confirm that but does not delve in to many particulars.

## Final Thoughts

Overall this semester of pl junior went well enough that we think it
makes a reasonable template for future semesters. The topics were
interesting and relevant, and we mostly picked appropriate material
for presentations. One downside is that we didn’t quite ‘fill out’ the
semester with presentations due to scheduling and not wanting to make
some people present twice. Here’s a lesson: recruit more people to the
phd program (or get more undergrads) so you don’t have this problem!

Having papers in a theme helped a lot over previous paper-presentation
iterations of pl junior. It helped each week being able to build on
what we learned last week, as opposed to having a whirlwind of
unrelated topics.

Writing this retrospective has proven to be a beneficial exercise.


[typeful]: http://www.lucacardelli.name/Papers/TypefulProg.pdf
[deductive]: https://www.sri.com/sites/default/files/uploads/publications/pdf/725.pdf
[progdirect]: https://arxiv.org/abs/1507.02988
[sketchnsketch]: https://ravichugh.github.io/sketch-n-sketch/index.html
[typeandexample]: http://www.cis.upenn.edu/~stevez/papers/OZ15.pdf
[Herbelin]: https://hal.inria.fr/inria-00381525/document
[gab]: http://www.ccs.neu.edu/home/gasche/phd_thesis/scherer-thesis.pdf
[predlogic]: http://www.doc.ic.ac.uk/~rak/papers/kowalski-van_emden.pdf
[smalltalk]: http://worrydream.com/EarlyHistoryOfSmalltalk/
[pljunior]: http://prl.ccs.neu.edu/seminars.html
[fall16retro]: http://prl.ccs.neu.edu/blog/2017/01/02/fall-2016-pl-junior-retrospective/
