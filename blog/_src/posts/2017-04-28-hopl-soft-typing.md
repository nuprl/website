    Title: What is Soft Typing?
    Date: 2017-04-28T12:25:17
    Tags: HOPL, by Ben Greenman

<!-- more -->

A soft type system rewrites programs and meets a few _design criteria_.

- - -

## What are the Design Criteria?

According to Mike Fagan's 1991 [dissertation](https://github.com/nuprl/hopl-s2017/tree/master/soft-typing/papers),
 a soft type system must:

- accept all _syntactically correct_ programs as input;
- produce equivalent, _memory-safe_ programs as output; and
- be _unobtrusive_


### Important details:

- In this context, _memory safe_ basically means "no segfaults".
  Programs output by a soft type system should be as safe as statically-typed
  Java programs or dynamically-typed Python programs.
- Fagan characterizes _unobtrusive_ with two general principles:
  * _minimal text principle_ : the type checker should work without any programmer-supplied annotations
  * _minimal failure principle_ : the type checker should assign _useful_ types to _idiomatic_ programs
    (basically, don't just say that every expression has "unknown" or "top" type)


## Why would I want to use a soft type system?

If you:

- like dynamic typing
- want some _benefits_ of static typing
- refuse to (or _cannot_!) change your code to satisfy a type checker

then Soft Typing is a perfect fit.
You just need to find/build a soft type checker.

### Clarification

The _benefits_ of static typing that a soft type system can give are:

- early detection of typos and simple logical errors
- documentation, through (inferred) type signatures
- speed, when the types can justify removing a runtime safety check

See Andrew Wright's 1994 [dissertation](https://github.com/nuprl/hopl-s2017/tree/master/soft-typing/papers) for proof.


## Can I use Andrew Wright's soft type system?

Not sure, but you may download the code for it:

- <https://github.com/nuprl/softscheme>


## Please explain Fagan's / Wright's soft types

Types `t` are made of constructors `k`, flags `f`, and type variables `a`.
The grammar for types is basically:

```
  t ::= a | (k f t ...) U t
  k ::= Int | Pair | ->
  f ::= ++ | -- | b
  a ::= a0 | a1 | a2 | a3 | ....
  b ::= b0 | b1 | b2 | b3 | ....
```

where:

- `U` is just a symbol, represents "union"
- `a` is a type variable; there are infinitely many type variables
- `b` is a flag variable; the set of flag variables is also infinte

There are also some rules for types to be well-formed.

Here are two well-formed types:

```
(Int ++) U a0

(-> ++ ((Int b0) U a1)
       ((Int ++) U a2)) U a3
```

Here are two types that match the grammar, but are **NOT** well-formed:

```
(Int ++ a0) U a1

(-> --) U a2
```

Finally, some intuition:

- A constructor `k` is like a behavior,
- a type _describes_ the behaviors a value can have.
- The description is like a bitvector of "yes", "no", or "maybe" for each possible behavior.
- A flag variable is the way to say "maybe".
- Every type ends with a type variable because every typed expression might
  flow to a context that expects a more general type.

The type and flag variables let Fagan and Wright encode subtyping using
 polymorphism.
It's a very cool idea, introduced in Didier Remy's
 [POPL 1989 paper](https://github.com/nuprl/hopl-s2017/tree/master/soft-typing/papers).
But it adds a learning curve, and has some drawbacks for type inference.


## Stream-of-consciousness notes from the HOPL lecture

- [Local copy](/blog/static/soft-typing.pdf)
- [Source of Truth](https://github.com/nuprl/hopl-s2017/tree/master/soft-typing)
