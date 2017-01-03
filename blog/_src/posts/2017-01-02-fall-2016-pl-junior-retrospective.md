    Title: Fall 2016 PL Junior Retrospective
    Date: 2017-01-02T16:39:37
    Tags: PL Junior, by Ben Chung, by Milo Davis, by Ming-Ho Yee, by Sam Caldwell

The [Programming Language Seminar, Junior] [10] (or “PL Junior”), is a
seminar for junior students to learn and discuss topics at a pace more
suitable to our background. This semester, we decided to study
dependent types. We chose this topic because

1. working from the [TAPL]
[1] presentation of type systems, dependent types are a step up in
difficulty (excepting F-omega-sub), and
2. they represent a
significant increase in the reasoning power of types over programs.

<!-- more -->

There was a preference for learning how to implement a dependent type
system, instead of spending a significant amount of time reading
papers, especially dense type-theoretic papers suggested by [posts]
[4] like [these][5]. So we followed the [pi-for-all] [2] lecture
series given by Stephanie Weirich at [OPLSS] [9], which focuses on
implementing a simple dependently-typed programming language.

After the pi-for-all lectures, we read chapter two of Edwin Brady’s
[dissertation on implementing dependently typed languages] [3]. The
thesis includes a relatively approachable introduction to TT, the core
dependent type theory of Epigram.

Along the way, we became sidetracked by [Girard’s paradox] [6]. In the
first pi-for-all lecture, we came across the Type-in-Type rule. (In a
dependent type system the term and the type languages are the same.
However, we still need to distinguish what is a “program” and what is
a “type,” for instance, so that we can determine that the annotation
of a function’s argument is valid. So a construct in the term language
is Type, which is meant to describe those things that are valid in
programs where we expect to find a type). In the lecture, this
prompted the comment that this (“of course”) makes our system
inconsistent as a logic, but there was no further elaboration, and we
could not figure out how to use this fact to show inconsistency.

It turns out the reason Type-in-Type is inconsistent is quite
complicated. It is explained in a [paper] [7] that we had difficulty
understanding. So we turned to the students in our lab that have
expertise in the area. The answer we received is that, intuitively, it
is inconsistent for the same reason as Russell’s paradox (or the
Burali-Forti paradox), but the actual proof is actually quite
involved. The lesson we drew is that despite being “obvious,”
Type-in-Type being inconsistent is not easy to prove. The way people
seem to throw around this conclusion is confusing from a beginner’s
point of view.

The best thing about the pi-for-all series is that it demystified
dependent types for us. We gained confidence in being able to
whiteboard a dependent type system with the ease of System-F or STLC.
If we had one complaint, the presentation of the material relied
heavily on Haskell details. The unbound library to handle variables in
the implementation results in a somewhat “magicy” representation of
binding; it’s not clear that the benefits are so great as to outweigh
the cost of just implementing alpha-equivalence and
capture-avoiding-substitution. Overall they were high-quality
lectures. As hinted above, we didn’t particularly care for the second
lecture that was mostly a code walk-through. One advantage of watching
videos was that we could speed through parts we were already
comfortable with.

With Edwin Brady’s dissertation, we got a glimpse of how quickly the
details of a dependently typed language get hairy. Looking at you,
inductive data definitions and eliminations. There were some extremely
large type signatures. While this exercise boosted our confidence that
we could read Serious Dependent Types™ papers, it also gave evidence
that our fears of incomprehensibility were not completely unfounded.

This issue appeared before in our discussion of Girard’s Paradox. In
the discussion of the paradox, we got stuck when we tried to
understand the very complex term that inhabited the bottom type.
Dependent typing, and discussions thereof, allow very rich,
meaningful, and complex types that are as complex as the code that
they abstract over. While we are used to understanding these
structures in code, parsing a complex type and its fundamental meaning
gave us considerable difficulty.

## Thoughts on the format

Our meetings this semester were all of the form “we’ll watch this
lecture or read this chapter, and then discuss it next week.” Next
semester we would like to go back to presenting each week. We feel
doing presentations forces the presenter to reach a deeper
understanding of the material. This semester we got a relatively
shallow understanding of a broad area. A deeper understanding with a
narrower focus may be more beneficial (or complementary).

[[Sam’s defense as Grand Convener of PL Junior: Once we picked dependent
types as a topic, doing presentations was not an option. We didn’t
have the expertise in the area to pick out different sub-topics and
papers suitable for presentations. And, since we were short-handed (4
people each week), we would be presenting once a month!]]

If we continue learning more about dependent types it would be by: 1)
Doing more reading, such as the [ATTAPL] [9] chapter or the programming
in Martin-Löf’s type theory material. 2) Actually trying to implement
some of the things we’ve learned this semester 3) Playing around with
more of the various dependent type systems and theorem provers out
there

For future pl junior cohorts or anyone else in learning about
dependent types: Pi-for-all is useful material, but could be condensed
into two weeks (for example, by watching the lectures at 1.5x speed)
instead of four. Don’t worry about Type-in-Type. The Epigram material
is OK but you might be better served looking at ATTAPL first. At some
point, you will have to read the dense papers, but pi-for-all is a
good introduction.

[1]: https://mitpress.mit.edu/books/types-and-programming-languages
[2]: https://github.com/sweirich/pi-forall
[3]: https://eb.host.cs.st-andrews.ac.uk/writings/thesis.pdf
[4]: http://purelytheoretical.com/sywtltt.html
[5]: http://jozefg.bitbucket.org/posts/2015-08-14-learn-tt.html
[6]: https://en.wikipedia.org/wiki/System_U#Girard.27s_paradox
[7]: https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf
[8]: https://www.cs.uoregon.edu/research/summerschool/
[9]: https://www.cis.upenn.edu/~bcpierce/attapl/
[10]: http://prl.ccs.neu.edu/seminars.html
