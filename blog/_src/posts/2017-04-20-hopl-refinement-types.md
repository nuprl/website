    Title: Refinement Types
    Date: 2017-04-20T23:38:23
    Tags: HOPL, by Kevin Clancy

<!-- more -->

Roughly, a refinement type system is an extra layer of precision, enforced
through subtyping, added onto an existing type system. A base type is
decomposed into a set of _base refinements_, each of which denotes a subset of
the values belonging to the base type. A subtyping relation respecting set
inclusion can then be defined among the refinements of the base type. These
subtyping relations can be lifted onto a subtyping relation for compound types
using a standard arrow subtyping rule.

Extra type-checking precision sounds great, but what in practical terms does
this precision look like? Freeman and Pfenning's '92 paper _Refinement Types
for ML_ proposes extending ML's type definition language with constructs for
decomposing a discriminated union type into a lattice of subtypes. For example,
it allows the decomposition of a list type into a lattice including base
refinements for empty lists, non-empty lists, and singletons. Those with
experience in functional programming will realize this alleviates the dreaded
and inescapable “non-exhaustive pattern match” warning, which tends to crop up
in situations where the programmer understands that an exhaustive pattern match
is not necessary.

In the late 90's Xi and Pfenning advanced the state of refinement types by
introducing a dependent refinement type system, implemented as a tool called
Dependent ML. Their approach identifies a base refinement using a tuple of
terms drawn from some computationally tractable constraint language called an
_index language_. A list datatype can then be refined with a term of the _linear
integer arithmetic_ index language, denoting the subset of all lists having a
specific length. One list refinement is then considered a subtype of another
when a constraint solver can prove their index terms equal. Vazou et. al.'s
recent project Liquid Haskell is another dependent refinement type system which
decides subtyping among base types by invoking an SMT solver under a
context-dependent set of constraints. It differs significantly from Dependent
ML in that it refines base types with certain well-behaved program terms rather
than indices.

- - -

Resources:

- [Full Notes](/blog/static/refinement_types_lecture.pdf)
- [Annotated Bibliography](/blog/static/refinement_types_bib.pdf)
- [GitHub](https://github.com/nuprl/hopl-s2017/tree/master/refinement-types)

