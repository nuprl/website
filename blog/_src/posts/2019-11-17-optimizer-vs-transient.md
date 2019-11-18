    Title: The Typed Racket Optimizer vs. Transient
    Date: 2019-11-17T21:32:56
    Tags: Offsite, WIP, by Ben Greenman

What type-directed optimizations does Typed Racket perform
 and do any require full types?

<!-- more -->

> This post is based on a short talk. Slides from the talk are here:
> <http://ccs.neu.edu/home/types/resources/talks/prl-offsite-2019.pdf>

Lately, I've been working on a [transient](https://dl.acm.org/citation.cfm?id=3009849)
 back-end for Typed Racket.
The goal of this project is to trade guarantees for performance.
Standard Typed Racket guarantees full type soundness, but uses higher-order
 contracts to make sure that interactions between Typed Racket and untyped
 Racket obey the types.
These contracts can be very expensive [[JFP 2019](https://doi.org/10.1017/S0956796818000217)].
Transient Typed Racket provides a weaker guarantee --- only that typed code
 cannot get "stuck" --- via simpler run-time checks.
Early data shows that these simple checks are sometimes better and sometimes
 worse than the standard boundary checks [[ICFP 2018](https://doi.org/10.1145/3236766)],
 hence we want both options for Typed Racket programmers.

Typed Racket comes with three major components:

1. a static type checker,
2. a compiler from types to contracts, and
3. a type-driven optimizer [[PADL 2012](https://www2.ccs.neu.edu/racket/pubs/padl12-stff.pdf), [OOPSLA 2012](https://doi.org/10.1145/2384616.2384629)].

Transient Typed Racket can re-use all of the type checker
 and parts of the type-to-contract compiler.
The question for this post is: can Transient re-use the optimizer?

The answer requires some thought because Standard Typed Racket and Transient
 Typed Racket preserve different amounts of type information.

- In Standard Typed Racket, if an expression `e` has type `T` and reduces
  to a value `v` (for short, `e : T -->* v`), then the result `v` definitely
  matches the full type `T`.
- In Transient Typed Racket, if `e : T -->* v` then the result `v` matches
  the toplevel "shape" of `T` but (maybe) nothing more.

A "shape" roughly corresponds to the outermost constructor of a type.
The only requirement is that every shape check is decidable.
Finding the best shapes is an engineering challenge.
On one hand, deeper checks give stronger guarantees.
On the other hand, shallower checks give better performance.

Here are a few shapes according to the current Transient prototype.
These shapes try to balance between guarantees and performance;
 for example, the shape of a function describes its arity.

```
  Shape(Natural)           = Natural
  Shape(Listof String)     = Listof Any
  Shape(Symbol -> Boolean) = Any -> Any
  Shape(Vector Void Void)  = Vector Any Any
  Shape(U Void Symbol)     = U Void Symbol
```

For the current shapes, can we re-use the Typed Racket optimizer?


## Optimizations

Typed Racket implements 15 kinds of type-directed transformation.
Below, each gets: a short description, an example, and a verdict of "safe"
 or "unsafe" for Transient.

### PASS

Keep it brief

#### Example

Wepa

**Verdict**: safe/not.
Explain why.


## Summary

The Typed Racket optimizer implements 15 kinds of transformations.
Two are definitely unsafe for Transient as-is.
One may limit our ability to reduce the number of run-time checks in a program.
Two others require transient checks whose cost depends on the size of the input values.

There may be other issues that I missed while reading the optimizer code.
If so, I'll try to remember to update this post.

