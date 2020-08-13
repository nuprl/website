    Title: Lexical and Dynamic Scope
    Date: 2019-09-05T10:00:00
    Tags: scope, definitions, history, by Ming-Ho Yee

This all started with a simple question about the R programming language: _is R
lexically or dynamically scoped?_

To answer that question, we need to understand what _scope_ is, along with
_lexical scope_ and _dynamic scope_.

<!-- more -->

In this blog post, I'd like to explain the differences between lexical scope
and dynamic scope, and also explore some of the history behind those ideas. In
a [subsequent post](/blog/2019/09/10/scoping-in-r/), I'll discuss
scoping in R and why it can be confusing.

## What is scope?

_Scope_ refers to the places in a program where a variable is visible and can
be referenced.

An interesting situation is when a function has free variables. Consider the
example below:

```r
x <- 1
f <- function(a) x + a
g <- function() {
  x <- 2
  f(0)
}
g() # what does this return?
```

On line 1, we create a mapping for `x` with value `1`. On line 2, we define a
function `f` whose body uses the parameter `a`, but also the free variable `x`.
On line 3, we define a function `g`, whose body creates a new mapping for `x`
with value `2`, and then calls `f(0)`. (Note that line 4 does not update the
mapping created on line 1.) Finally, on line 7, we call `g()`.

What value does `g` return when it is called? What mapping does the free
variable `x` on line 2 refer to? Does it refer to the mapping on line 1 that
was visible when `f` was defined? Or does it refer to the mapping on line 4
that was created just before `f` was called?


### Lexical scoping

Under _lexical scoping_ (also known as _static scoping_), the scope of a
variable is determined by the lexical (_i.e._, textual) structure of a program.

In the example above, the definition of `x` on line 1 creates a scope that
starts after its definition and extends _into_ the bodies of `f` and `g`.
However, the second definition of `x` on line 4 creates a new scope that (1)
shadows the previous definition of `x`, and (2) does not extend into the call
`f(0)` on line 5. Looking at this from another direction, the use of `x` on
line 2 is within the scope created by the definition on line 1, and thus refers
to that definition.

Therefore, under lexical scoping, the example program returns `1`.

Most programming languages we use today are lexically scoped. Intuitively, a
human (or compiler) can determine the scope of a variable by just examining the
source code of a program. In other words, a compiler can determine which
_definition_ each variable refers to---but it may not be able to determine the
_values_ of each variable.


### Dynamic scoping

Under _dynamic scoping_, a variable is bound to the most recent value assigned
to that variable, _i.e._, the most recent assignment _during the program's
execution_.

In the example above, the free variable `x` in the body of `f` is evaluated
when `f(0)` is called on line 5. At that point (during program execution), the
most recent assignment was on line 4.

Therefore, under dynamic scoping, the example program returns `2`.

Dynamically scoped programming languages include bash, LaTeX, and the original
version of Lisp. Emacs Lisp is dynamically scoped, but allows the programmer to
select lexical scoping. Conversely, Perl and Common Lisp are lexically scoped
by default, but allow the programmer to select dynamic scoping.

(**Edited 2020/08/13:** As of [Emacs
27.1](https://www.gnu.org/savannah-checkouts/gnu/emacs/news/NEWS.27.1),
"lexical binding is now used by default when evaluating interactive Elisp.")


## Now for a digression

These are the definitions I learned from my classes and textbooks, and should
be similar to other definitions and explanations you might find online.

However, it took me many drafts and attempts before arriving at the current
version. I had difficulty writing an explanation that I was satisfied with---a
definition that was not circular, did not appeal to some intuition or
familiarity, and did not conflate terms. Even some of the resources I consulted
had these issues.[^0]

[^0]: For example, at one point I defined lexical/dynamic scoping in terms of a
"lexical environment" and a "dynamic environment." But (1) that's a circular
definition, (2) it assumes the reader has some intuition of how a "lexical
environment" is different from a "dynamic environment," and (3) it conflates
two different kinds of "environment."

I am much happier with my current version, but it still bothers me slightly. If
lexical scope and dynamic scope are related concepts, then why are the
definitions so different? Why does the definition for _dynamic scope_ not
mention scope at all? If _scope_ is about "where a variable is visible," and
that definition is with respect to a _variable definition_, then why do so many
explanations and examples define lexical and dynamic scope in terms of
_variable use_?


## Scope and Extent

I found some answers in Guy Steele's _Common Lisp the Language, 2nd
Edition_,[^1] which Matthias Felleisen recommended to me.

[^1]: G. Steele. "Scope and Extent," in _Common Lisp the Language_, 2nd ed.
1990. \[[Available online](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node43.html#SECTION00700000000000000000)\]

In chapter 3, Steele introduces the concepts of _scope_ and _extent_:

> _Scope_ refers to the spatial or textual region of the program within which
> references may occur. _Extent_ refers to the interval of time during which
> references may occur.

In addition, there are four interesting cases of scope and extent, with respect
to Common Lisp:

  - _Lexical scope_: a reference can only occur within certain textual regions
    of the program, which are determined by the establishing construct, _e.g._,
    the body of a variable definition.

  - _Indefinite scope_: a reference can occur anywhere in the program.

  - _Dynamic extent_: a reference can occur during the time between an entity's
    creation and its explicit destruction, _e.g._, when a local variable is
    created upon entering a function and destroyed when returning from that
    function.

  - _Indefinite extent_: an entity may exist as long as it is possible to be
    referenced. (Note that this is the idea behind garbage collection: an
    entity can be destroyed once references to it are impossible.)

Steele points out that _dynamic scope_ is a misnomer, even though it is both a
traditional and useful concept. It can be defined as _indefinite scope and
dynamic extent_. In other words, references to a variable may occur anywhere in
a program, as long as that variable has been initialized and has not yet been
explicitly destroyed. Furthermore, a later initialization hides an earlier one.


### Discussion

I found this approach very informative, because it explicitly distinguishes
between space (scope) and time (extent), which further implies a separation
between compile time and run time. This explains my unease with the definition
of "dynamic scope"---it is nominally about textual regions in a program, but
also requires consideration of run-time behaviour. Dynamic scope is a misnomer!

The above definitions are specifically for Common Lisp, but I believe we can
learn from them and adapt them for other programming languages.


## A brief and incomplete history of lexical scope

During my research of different definitions of lexical scope, I began to wonder
if there was an "original" definition of lexical scope. I did not find one, but
I was able to trace some of the connections between Lisp, Scheme, and ALGOL 60.
This history is certainly incomplete, but I hope it is somewhat useful and
interesting.

  * **1960**. John McCarthy publishes the original paper on Lisp.[^2] In
    _History of Lisp_,[^3] McCarthy writes that he borrowed the λ-notation from
    Alonzo Church's lambda calculus, but none of the other ideas. He also
    recounts an incident where a programmer desired lexical scoping, but Lisp
    used dynamic scoping. McCarthy considered this to be a bug, which Steve
    Russell later fixed by developing the "FUNARG device."

  * **1963**. After a few years of work, the _Revised Report on Algorithm
    Language ALGOL 60_ is published.[^4] While "lexical scope" is not explicitly
    mentioned, it is recognizable in the specification.

  * **1964**. Peter Landin shows how expressions in programming languages can
    be modelled in Church's λ-notation.[^5] He also introduces the concept of a
    _closure_, which pairs a lambda expression with the environment it was
    evaluated in.

  * **1970**. Joel Moses describes the problem of free variables in
    functions.[^6] He considers both the "downward" case (where a function is
    passed to another function) and the "upward" case (where a function returns
    a function), and remarks on the correspondence between Lisp's FUNARG device
    and Landin's closures.

  * **1975**. Gerald Sussman and Guy Steele publish the first Scheme paper.[^7]
    They describe their goal of a Lisp-like language that is based on the
    lambda calculus. As a consequence, they implement lexical scoping with
    closures, to preserve the substitution semantics of the lambda calculus.
    They compare this scoping discipline to ALGOL's.

  * **1978**. Steele and Sussman describe various programming language design
    choices, by developing an interpreter for each programming language
    variation.[^8] In particular, they provide a detailed discussion on
    lexical and dynamic scoping.

[^2]: J. McCarthy. "Recursive Functions of Symbolic Expressions and Their
Computation by Machine, Part I," _Communications of the ACM_, vol. 3, no. 4,
April 1960. \[[DOI](https://doi.org/10.1145/367177.367199)\]\[[Available
online](http://jmc.stanford.edu/articles/recursive/recursive.pdf)\]

[^3]: J. McCarthy. "History of LISP," in _History of Programming Languages_,
1978. \[[DOI](https://doi.org/10.1145/800025.1198360 )\]\[[Available
online](http://jmc.stanford.edu/articles/lisp/lisp.pdf)\]

[^4]: P. Naur (ed.). "Revised Report on Algorithmic Language ALGOL 60,"
_Communications of the ACM_, vol. 6, no. 1, January 1963.
\[[DOI](http://dx.doi.org/10.1145/366193.366201)\]\[[Available
online](https://www.masswerk.at/algol60/report.htm)\]

[^5]: P. Landin. "The mechanical evaluation of expressions," _The Computer
Journal_, vol. 6, no. 4, January 1964.
\[[DOI](https://doi.org/10.1093/comjnl/6.4.308)\]\[[Available
online](https://www.cs.cmu.edu/~crary/819-f09/Landin64.pdf)\]

[^6]: J. Moses. "The Function of FUNCTION in LISP or Why the FUNARG Problem
Should be Called the Environment Problem," _SIGSAM Bulletin 15_, July 1970.
\[[DOI](https://doi.org/10.1145/1093410.1093411)\]\[[Available
online](https://dspace.mit.edu/handle/1721.1/5854)\]

[^7]: G. Sussman and G. Steele. "SCHEME: An Interpreter for Extended Lambda
Calculus." 1975. \[[Available
online](https://dspace.mit.edu/handle/1721.1/5794)\]

[^8]: G. Steele and G. Sussman. "The Art of the Interpreter or, The Modularity
Complex (Parts Zero, One, and Two)." 1978. \[[Available
online](https://dspace.mit.edu/handle/1721.1/6094)\]


## Next stop, R

Now that we have examined the definitions of lexical and dynamic scope, and
also explored some history, we are ready to return to the original question.
_Is R lexically or dynamically scoped?_

In the [next blog post](/blog/2019/09/10/scoping-in-r/), we'll answer that
question, and also see how R can be very confusing.

_I would like to thank Sam Caldwell, Ben Greenman, and Artem Pelenitsyn for
their comments and feedback on this blog post._

---
