    Title: Java and Migratory Typing
    Date: 2018-12-02T14:41:53
    Tags: migratory typing, gradual typing, java, by Ben Greenman

The _transient_ approach to migratory typing (circa 2014)
 is similar to type erasure in Java (circa 2004).

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

The compiler works by erasing types.
Every type `T` in the source code translates to the universal "JavaScript type".
For instance, a TypeScript function declaration compiles to an untyped
 JavaScript function:

<!-- TODO use real TypeScript -->
```
(function(number n0, number n1) { return n0 + n1; })
// ==(compiles to)==>
(function(n0, n1) { return n0 + n1; })
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
 as the run-time guarantees of JavaScript (in an ordinary program).
If a TypeScript expression `e` has the static type `T` and evaluates to
 a value `v`, the only guarantee is that `v` is a valid JavaScript value
 --- `T` could be `number` and `v` could be an object.


## Transient migratory typing

[Reticulated]() is a migratory typing system for Python that follows a
 so-called _transient_ implementation strategy.
Reticulated/transient first appeared in the academic literature in 2014,
 and was introduced as an approach that provides run-time guarantees (goal **G2**)
 without using proxy values.
<!-- link to proxies -->

> 





## Links

Erasure pioneered in common lisp / maclisp, Strongtalk for manifesto

SNAPL 17 for an MT retrospective

POPL 17 for transient <!-- TODO add Mike's dissertation when finished -->

(and [gradual typing](SNAPL15)) is related

dependent interoperability


## Acknowledgments

Thank you to Ryan Culpepper and Jesse Tov for noticing the similarity between
 Java type erasure and _transient_ migratory typing.
