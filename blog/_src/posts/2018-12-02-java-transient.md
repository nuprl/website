    Title: Java and Migratory Typing
    Date: 2018-12-02T14:41:53
    Tags: migratory typing, gradual typing, java, by Ben Greenman

The _transient_ approach to migratory typing (circa 2014)
 is similar to type erasure in Java (circa 2004).
<!-- TODO weaken 'is similar' -->

<!-- more -->

## Migratory typing

The goal of _migratory typing_ is to enrich the type system of a language
 without breaking backwards compatibility.
Ideally, code that uses the enriched types:

- (G1) benefits from new ahead-of-time checks,
- (G2) benefits from stronger run-time guarantees, and
- (G3) may interact with all kinds of existing code.

There are tradeoffs involved in the implementation of a migratory typing
 system, however, and different implementations may focus on different goals
 that the three above.

A typical migratory typing system adds a static type checker to a dynamically
 typed language ([examples](/blog/2018/10/06/a-spectrum-of-type-soundness-and-performance/index.html)),
 but one could also extend the type system of a statically-typed language.
In this sense, Java 1.5.0 is a migratory typing system for pre-generics Java.
Adding generics enabled new ahead-of-time checks and maintained backwards
 compatibility with existing Java code.

Java's implementation of migratory typing has some interesting things in common
 with the _transient_ implementation strategy recently proposed by
 Michael Vitousek and collaborators.
The goal of this post is to spell out the connections.
<!-- TODO awkward ^ -->


## Erasure migratory typing

Before we compare Java 1.5.0 to _transient_, lets review a strategy that
 predates and informs them both: the _erasure_ approach to migratory typing.

[TypeScript](CITE) is a great example of the erasure approach.
TypeScript is a migratory typing system for JavaScript.
A TypeScript module gets validated by an ahead-of-time type checker and
 compiles to JavaScript.
After compilation, any JavaScript program may import bindings
 from the generated code.
Conversely, a TypeScript module may import bindings from a JavaScript module
 by declaring a static type for each binding [link](CITE).

The TypeScript compiler erases types; in other words,
 every type `T` in the source code translates to the universal "JavaScript type".
For instance, a TypeScript function declaration compiles to an untyped
 JavaScript function:

<!-- TODO use real TypeScript -->
```
(function (number n0, number n1) { return n0 + n1; })

// ==(compiles to)==>

(function (n0, n1) { return n0 + n1; })
```

TypeScript satisfies goals **G1** and **G3** for a migratory typing system
 because its type checker adds ahead-of-time checks and its
 compiler outputs JavaScript.
TypeScript does not satisfy goal **G2** because the compiler erases types.
In terms of the example above, the compiled function may be invoked with any
 pair of JavaScript values; the variable `n0` is not guaranteed to point
 to a `number` at run-time.
On one hand, this means the type annotations have no effect on the behavior
 of a program --- and in particular, cannot be trusted for debugging.
On the other hand, it means that an experienced JavaScript programmer can
 re-use their knowledge to predict the behavior of a TypeScript program.

More generally, the run-time guarantees of TypeScript are the same
 as the run-time guarantees of JavaScript (in an ordinary program):

- if a TypeScript expression `e` has the static type `T` and evaluates to
 a value `v`,
- then the only guarantee is that `v` is a valid JavaScript value
  (e.g., `T` could be `number` and `v` could be an object).


## Transient migratory typing

[Reticulated]() is a migratory typing system for Python that follows a
 so-called _transient_ implementation strategy.
A Reticulated module gets type-checked and compiles to a Python module that
 defends itself from _certain_ type-invalid inputs through the use of
 assertions that run in near-constant time.

> These _certain_ inputs are the ones that would cause a standard typed
> operational semantics to "get stuck" [TODO](CITE).
> For a discussion of _near-constant_, see
> [On the Cost of Type-Tag Soundness](http://www.ccs.neu.edu/home/types/publications/publications.html#gm-pepm-2018)
> section 2.

For example, here is a Reticulated function
 that computes the average of a list of numbers:

```
# Reticulated (commit e478343)
def average(nums : List(Float)) -> Float:
  if ns:
    return sum(ns) / len(ns)
  else:
    raise ValueError("average: expected non-empty list")
```

and here is the Python code it compiles to:

```
from retic.runtime import *
from retic.transient import *
from retic.typing import *

def average(nums):
    check_type_list(nums)
    if ns:
        return check_type_float((check_type_function(sum)(ns) / check_type_function(len)(ns)))
    else:
        raise check_type_function(ValueError)('average: expected non-empty list')
```

> Note: the Reticulated syntax for type annotations is similar to the one
> proposed in [PEP 484](TODO), but not identical. For example, Reticulated does not
> require forward references to be embedded in strings.

The Reticulated compiler removes all type annotations and inserts `check_type`
 assertions throughout the code.
In `average`, these assertions check that: (1) the input is a list,
 (2) the output is a `float`, (3) and the names `sum` `len` and
 `ValueError` point to callable values.
That's all; the assertions **do not check** that `nums` contains only floating-point
 numbers.

> They also do not check that the function bound to `sum` is defined
> for a single argument, which is arguably a bug.
> Scaling a model to an implementation is always challenging.
<!-- TODO java null link? -->

If `nums` contains something other than floating point numbers, then the
 call to `average` may cause `sum` to raise an exception or it may silently
 compute a nonsense result.
The behavior depends on the implementation of `sum` in the same way that
 the behavior of a TypeScript function depends on the JavaScript functions
 that it depends on.

Reticulated does not erase types, nor does it fully enforce types.
Every type in a Reticulated module translates to its
 top-level type constructor `C(T)`, e.g.:

```
  C(Float)                = Float
  C(List(Float))          = List
  C(List(Float) -> Float) = ->
```

Consequently, Reticulated has a slightly stronger run-time guarantee than Python:

- if `e` is an expression with static type `T` that evaluates to a value `v`,
- then `v` is guaranteed to have a top-level shape that matches the `C(T)`
  constructor.


## Java migratory typing

Java 1.5.0 added [generic types](https://www.jcp.org/en/jsr/detail?id=14)
 to the Java 1.4.0 type system, without breaking backwards compatibility.

From the [Java Language Specification, SE 11 Edition, Section 4.7](https://docs.oracle.com/javase/specs/jls/se11/html/jls-4.html#jls-4.7):

> The decision not to make all generic types reifiable is one of the most crucial, and controversial design decisions involving the type system of the Java programming language.
>
> Ultimately, the most important motivation for this decision is compatibility with existing code.


<!-- OUTLINE
  added generics,
  motivation = better type checks more reusable,
  did it without breaking backwards compatibility --- pre-generics code can run alongside
   which means they compile to the same language,
  strategy = use existing casts to enforce generic types,
  compiler inserts casts (example),
  generics not around at runtime (reified) so the compiler has an erasure,
X details of erasure
  guarantee

  NOTE: downwards binary-compat is a non-goal
-->


Erasure where:
 `G<T_1 ... T_n>` is a parameterized type,
 `T.C` is a nested type,
 `T[]` is an array type, and
 `A<T_lo T_hi>` is a type variable `A` with left bound `T_lo` and right bound `T_hi`.

```
  |G<T_1 ... T_n>| = |G|
  |T.C|            = |T|.C
  |T[]|            = |T|[]
  |A<T_lo T_hi>|   = |T_lo|
  |T|              = T       otherwise
```

The Java 1.5.0 run-time guarantee is thus:

- if `e` is an Java 1.5.0 expression with static type `T` that evaluates to a
  bytecode value `v`,
- then `v` is guaranteed to match the bytecode type `|T|`

The guarantee is weak because legacy code can interact with a generic value
 in a type-incorrect way.

Gilad gives an example and says its dangerous.

From [Generics in the Java Programming Language, Section 6.1](https://www.oracle.com/technetwork/java/javase/generics-tutorial-159168.pdf):

> Calling legacy code from generic code is inherently dangerous; once you mix
> generic code with non-generic legacy code, all the safety guarantees that the
> generic type system usually provides are void.


## Discussion

<!--
  summarize Java ~ Transient ~ Erasure,
  each has static checks, but each loses something at runtime,
  still, compatible,
  also, performance,

  surely there is more to say
-->

## Links

Erasure pioneered in common lisp / maclisp, Strongtalk for manifesto

SNAPL 17 for an MT retrospective

POPL 17 for transient <!-- TODO add Mike's dissertation when finished -->

(and [gradual typing](SNAPL15)) is related

dependent interoperability


## Acknowledgments

Thank you to Ryan Culpepper and Jesse Tov for noticing the similarity between
 Java type erasure and transient migratory typing.
