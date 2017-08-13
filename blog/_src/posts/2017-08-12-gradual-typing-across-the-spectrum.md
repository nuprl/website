    Title: Gradual Typing Across the Spectrum, part II
    Date: 2017-08-12T17:59:58
    Tags: gradual typing, PI meeting, by Ben Greenman

Last week, Northeastern hosted a PI meeting for the [Gradual Typing Across the
 Spectrum](http://prl.ccs.neu.edu/gtp/) NSF grant.
The meeting was made of 20+ researchers from four institutions,
 and 12 technical talks.
Schedule:

<http://prl.ccs.neu.edu/gtp/pi2017/pi2017.html>

A common thread among the talks was the question:
 _how to convert a research idea into a tool for software developers?_

<!-- more -->

In my mind, gradual typing _is_ an answer to one instance of this question.
The research idea is strong static type systems, and the software developers
 are the millions using dynamically typed languages.
I know that static typing can make programs easier to write and maintain.
The developers know that dynamic typing has benefits; moreover they know better
 than to migrate their code from one language to another on a whim.
Gradual typing is a linguistic solution to the problem of _adding_ the benefits
 of static typing to a dynamically typed language.

Enough opinions, let's talk about the talks.

The morning session consisted of four talks:

- [Milod Kazerounian](https://www.cs.umd.edu/people/milod) ([UMD])
  spoke about upgrading the [RDL](https://github.com/plum-umd/rdl) type checker
  for Ruby with support for refinement types.
  The idea is to compile Ruby code and types to [Rosette](https://emina.github.io/rosette/),
  and profit from [SMT](http://yices.csl.sri.com/papers/cav2007.pdf)-assisted type checking.

- [Ambrose Bonnaire-Sergeant](http://ambrosebs.com/) ([IU], [slides](http://ambrosebs.com/talks/squash-work-boston-pi-2017.pdf))
  has been inferring _useful_ [Typed Clojure](http://typedclojure.org/) types through dynamic analysis of
  Clojure programs. His tool observes how values flow through a program at run-time,
  then lifts these observations into possibly-recursive, possibly-incorrect type
  annotations. The surprising result is that the tool quickly (1-2 seconds per
  unit test, I think) infers types that can help a developer start annotating a program.

- [Ben Greenman](http://ccs.neu.edu/~types/) ([NEU], [slides](http://homedirs.ccs.neu.edu/types/resources/talks/preservation-types.pdf))
  explained why he is implementing a semantics for [Typed Racket](https://github.com/racket/typed-racket)
  inspired by Michael Vitousek's work on [Reticulated Python](http://homes.soic.indiana.edu/mvitouse/papers/popl17.pdf).
  The "why" is "performance". The Reticulated semantics will enforce a
  notion of tag soundness in kind of [devils contract](https://en.wikipedia.org/wiki/Deal_with_the_Devil)
  to improve performance.

- [Preston Tunnell-Wilson](https://cs.brown.edu/~ptunnell/) ([Brown], [ONWARD 2017](http://cs.brown.edu/~sk/Publications/Papers/Published/tpk-crowdsource-lang-design/))
  recently sent questions about programming language design to
  [Mechanical Turk](https://www.mturk.com/mturk/welcome) workers.
  Survey says, developers have extremely diverse opinions about what they _expect_
  and what they _want_ regarding scope, inheritance, and infix operators.

In the early afternoon, we had two talks on similar themes as the morning session:

- [Andre Kuhlenschmidt](https://github.com/akuhlens) ([IU])
  is exploring the design space of efficient implementations for run-time type checks.
  The main challenge is how to _monitor_ higher-order data in a way that efficiently
  performs type checks and can help the programmer debug any failed checks.
  This talk presented data comparing two approaches to the program; I believe
   the latter, improved approach is based on [coercions](http://homepages.inf.ed.ac.uk/wadler/papers/coercions/coercions.pdf).

- [Zeina Migeed](https://zeinamigeed.com/) ([NEU])
  explained that there are many ways to adapt type soundness to a gradually
  typed language, and presented some data comparing Typed Racket's _generalized soudness_
  to Reticulated Python's _tag soundness_. The data suggests that tag soundness
  never adds an order-of-magnitude slowdown.

Next on the schedule were two talks about implementing advanced type systems
 in Racket's macro expander (think: meta-level linguistic re-use, capture-avoiding
 substitution for free!)

- [Milo Turner](https://github.com/iitalics) ([NEU])
  first showed how to implement [linear and affine](https://gankro.github.io/blah/linear-rust/#definitions-and-the-state-of-rust)
  type systems using [syntax-parse](http://docs.racket-lang.org/syntax/Parsing_Syntax.html),
  and second presented a simpler implementation using the [Turnstile](http://docs.racket-lang.org/turnstile/index.html) library.

- [David Christiansen](http://www.davidchristiansen.dk/) ([IU])
  is building [a proof assistant](https://github.com/david-christiansen/pudding)
  in Racket. This talk focused on the design and implementation of proof
  tactics.

After a short break, we heard about something completely different:

- [Justin Pombrio](http://justinpombrio.net/) ([Brown], [ICFP 2017](http://cs.brown.edu/research/plt/dl/icfp2017/))
  taught us to interpet the scoping rules of a "core" language as a preorder.
  Using the preorder, he then showed how to _infer_ the scoping rules of
  any "surface" language based on its translation to the "core".

Last summer and fall, Jeremy Siek hosted two REUs ([research experience for undergraduates](https://www.nsf.gov/funding/pgm_summ.jsp?pims_id=5517&from=fund)) at Indiana University.
The two students gave the next talks:

- Di Zhong ([IU])
  talked about implementing interpreters in Racket, Python, and Haskell.
  As I understand, this was a hands-on experience through [TAPL](https://www.cis.upenn.edu/~bcpierce/tapl/)
  and [the Redex book](https://redex.racket-lang.org/).

- [Zeina Migeed](https://zeinamigeed.com/) ([IU])
  demonstrated her implementation of [conditional types](http://theory.stanford.edu/~aiken/publications/papers/popl94.pdf)
  for [Reticulated](https://github.com/mvitousek/reticulated).

Finally,

- [Niki Vazou](https://nikivazou.github.io/) ([UMD])
  presented a theory of gradual refinement types.
  Any "holes" in the refinements introduce a search problem; type checking
  attempts to solve the problem by finding a predicate that unifies a function
  definition and its callers.

This meeting was a great opportunity to reflect on the recent past and
 share opinions on what's worth pursuing in the future.
Many thanks to the participants, and to the NSF for the support!

> If you want to know about the future, you need to ask the young people who
> will create it. Young people don't know what can't be done, and so they go
> ahead and do it. -- [Ivan Sutherland](https://www.youtube.com/watch?v=sM1bNR4DmhU)


[Brown]: http://cs.brown.edu/
[IU]: https://www.cs.indiana.edu/
[NEU]: http://www.ccis.northeastern.edu/
[UMD]: https://www.cs.umd.edu/
