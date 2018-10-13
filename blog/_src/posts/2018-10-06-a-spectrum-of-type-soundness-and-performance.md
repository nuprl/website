    Title: A Spectrum of Type Soundness and Performance
    Date: 2018-10-06T11:23:35
    Tags: migratory typing, gradual typing, by Ben Greenman

The literature on mixed-typed languages presents (at least) three fundamentally
different ways of thinking about the integrity of programs that combine 
statically typed and dynamically typed code.
Recently, we have been sorting them out.

<!-- more -->

> Note: this post is an extended abstract for the paper _A Spectrum of Type
  Soundness and Performance_ by Ben Greenman and Matthias Felleisen.
  For the full paper, slides, code, and a video presentation, visit
  <http://www.ccs.neu.edu/home/types/publications/publications.html#gf-icfp-2018>

A dynamically-typed language runs any program that "looks good" (i.e.,
 passes some basic syntactic criteria. In Python a program cannot mix
 indentation levels. In Racket a program cannot refer to unbound variables).
A statically-typed language runs any program that both "looks good" and
 is well-typed according to a type checker.

A _mixed-typed_ language allows some combination of static and dynamic typing.
There are many languages that fall in the mixed-typed category; figure 1 lists
 a few, roughly arranged left-to-right by the year they first provided a way to
 mix.

![Figure 1: Some mixed-typed languages](/img/mixed-typed-systems-by-year.png)

These languages all try to combine static and dynamic typing in a useful way,
 but they take VERY different approaches.
For example:

- **MACLISP** defines a syntax for type annotations but does not say how a compiler
  should interpret the types; see section 14.2 of the [Moonual](http://www.softwarepreservation.org/projects/LISP/MIT/Moon-MACLISP_Reference_Manual-Apr_08_1974.pdf).
  For example, a compiler may use types to generate specialized code that assumes
  the type annotations are correct (and has undefined behavior otherwise).
- **Strongtalk** includes a static type checker and DOES NOT use types to change the
  behavior of a program.
  For rationale, see the [Pluggable Type Systems](http://bracha.org/pluggableTypesPosition.pdf) position paper.
- **Typed Racket** lets a program combine statically-typed modules and dynamically-typed
  modules. The compiler inserts run-time checks at the boundaries between such
  modules to detect any mismatches between the static types and incoming dynamically-typed
  values.
- **Thorn** requires that every value in a program has a type, but allows
  dynamically-typed contexts to manipulate values. In other words, every Thorn
  value is an instance of a statically-declared class and classes may contain
  dynamically-typed methods.
- **Reticulated** lets a program combine static and dynamic _expressions_ and
  guarantees that the combination has a well-defined semantics (Vitousek, Swords, and Siek [POPL 2017](https://dl.acm.org/citation.cfm?id=3009849)).

That makes five different systems.
There are 15 other systems in the figure, and many more in the world.
How can we make sense of this space?
We claim: by understanding each system's protocol for checking
 dynamically-typed values at a _type boundary_ (between static and dynamic code).


### Main Contribution

In the paper [_A Spectrum of Type Soundness and Performance_](http://drops.dagstuhl.de/opus/volltexte/2015/5031/),
 we define a tiny mixed-typed language and show three ways to define the
 behavior of programs --- based on three protocols for checking
 dynamically-typed values that cross a boundary into statically-typed code.

The three behaviors are inspired by existing languages.
A **higher order** behavior ensures that dynamically-typed
 values match the static type at a boundary --- by checking the value when possible,
 and by monitoring the value's future interactions when necessary.
A **first order** behavior performs a yes-or-no check on dynamically-typed values
 and never monitors their future behavior.
An **erasure** behavior does no checking whatsoever.

> Example (monitors): if typed code expects a function from numbers to numbers
> and receives an untyped function `f`, then one way to enforce the type
> boundary is to wrap `f` in a proxy to assert that every future call to `f`
> returns a number.  In this case, the proxy monitors the behavior of `f`.

Concretely, the paper defines three formal semantics with the same names.
The **higher-order** semantics enforces full types at the boundaries (Section 2.3).
The **first-order** semantics enforces type constructors at the boundaries, and
 furthermore enforces type constructors on every "selector" operation in typed
 code, e.g., function application, data structure access (Section 2.5).
The **erasure** semantics simply ignores the types (Section 2.4).

Each semantics satisfies a _different_ notion of soundness for mixed-typed
 programs, and each notion is slightly weaker than soundness for fully-typed
 programs.
The paper states these theorems (Section 2) and the
 [online supplement](https://repository.library.northeastern.edu/files/neu:cj82rk279)
 gives full proofs.

The paper has more to say about the meta-theory. See section 2 and section 4.

> To the best of our knowledge, this paper is the first to explicitly acknowledge
> that different approaches to a mixed-typed language lead to different notions
> of soundness. Other papers state type soundness theorems for
> [subset of the language](https://dl.acm.org/citation.cfm?id=2676971)
> (in the spirit of [soundiness](http://soundiness.org/))
> or use the name "type soundness" to describe [a different property](https://dl.acm.org/citation.cfm?id=2676971).

Next, we used the three semantics as a guide to arrive at three compilers for
 Typed Racket.
The higher-order compiler is the standard Typed Racket.
The first-order compiler is something we built, based on the semantics.
The erasure compiler simply ignores the type annotations --- similar to Typed Racket's
 [no-check](http://docs.racket-lang.org/ts-reference/Typed_Racket_Syntax_Without_Type_Checking.html) language.

Using this set-up, we measured the performance of mixed-typed programs via
 each compiler using the method suggested by Takikawa et. al ([POPL 2016](https://www2.ccs.neu.edu/racket/pubs/popl16-tfgnvf.pdf)).
The programs we measured are the non-object-oriented ones from our [benchmark suite](http://docs.racket-lang.org/gtp-benchmarks/index.html).

To some extent, the performance results confirm conjectures from the literature.
The full results, however, include many surprises --- see section 3 of the paper,
 section B of the [supplement](https://repository.library.northeastern.edu/files/neu:cj82rk279),
 and/or the [slides](http://www.ccs.neu.edu/home/types/publications/apples-to-apples/gf-icfp-2018-slides.pdf).


### Implications

1. The model in the paper is one way to understand the different approaches
   to mixed-typed languages. See section 5 of the paper,
   section D of the [supplement](https://repository.library.northeastern.edu/files/neu:cj82rk279),
   or [slide 114](http://www.ccs.neu.edu/home/types/publications/apples-to-apples/gf-icfp-2018-slides.pdf).
2. Programmers using mixed-typed languages need to know what guarantees their
   types provide.
   (It is [not safe to assume that TypeScript types give the same guarantees as OCaml types](https://twitter.com/jbandi/status/965005464638541825)!)
   Section 4 of the paper contains many examples of how the different guarantees
   may affect practice.
3. The relative performance of different approaches is more nuanced than the
   literature suggests. Our paper gives a first systematic comparison based on
   implementations that have clear areas for improvement. The question is:
   can we find improvements that lead to asymptotic differences, or is it a
   battle for constant factors?


> Note: in this post, a _mixed-typed language_ is one that allows any combination
> of static and dynamic typing. A _gradually-typed language_ is one that
> allows a certain kind of mixing that satisfies properties defined by Siek,
> Vitousek, Cimini, and Boyland ([SNAPL 2015](http://drops.dagstuhl.de/opus/volltexte/2015/5031/)).
