    Title: Complete Monitoring for Gradual/Migratory Typing
    Date: 2019-07-18T14:33:47
    Tags: migratory typing, gradual typing, complete monitoring, extended abstract, by Ben Greenman

Complete monitoring is a key semantic property for languages that mix typed
and untyped code.
If a semantics is a complete monitor, then untyped code can trust the type
annotations and error messages can pinpoint the first mismatch between a
(possibly incorrect) type annotation and an untyped value.

<!-- This post explains -->
<!-- **why** complete monitoring matters and -->
<!-- **what** technical concepts are needed to state a complete monitoring theorem. -->

<!-- more -->

- - -

<!-- OUTLINE -->
<!-- - [ ] types are static claims about runtime values -->
<!-- - [ ] in a typed language, type soundness is ox -->
<!--   + beware the runtime library -->
<!-- - [ ] in a mixed language, soundness is not enough -->
<!--   + lambda-calculus example, for soundness with different behavior ? -->
<!-- - [ ] example ... what went wrong? bad boundary-crossing -->
<!-- - [ ] so, agreeing with the judgment "wrong" implies a few assumptions about the -->
<!--   structure of a program: made of components, boundaries typed, value not -->
<!--   allowed to cross without a full check. Express with static + dynamic axioms. -->
<!-- - [ ] now can say when "wrong" happens --- its when value gets multiple owners -->
<!-- - [ ] semantics is a complete monitor if never raises single-owner-policy errors -->
<!-- - [ ] STOP reflect ... ownership explicit + new error, and done -->
<!-- -  -->
<!-- - [ ] example, int/pair language need to check boundaries else clearly bad -->
<!-- - [ ] CM = monitor all channels of communication ... with full checks here -->
<!--   + full not always possible, and formal CM does not guarantee the right checks -->
<!-- -  -->
<!-- - [ ] example 2, add functions, cannot do full check but can delay -->
<!--   + proxy is a new value -->
<!-- -  -->
<!-- - [ ] reflection, why to not be complete -->

The goal of [migratory typing]() is to add static types to an existing
untyped language.
The goal of [gradual typing](), similarly, is to add dynamic typing to an
existing static type system.
Both hope to end up with a programming language that combines the benefits
of static and dynamic typing.

In other words, the common end-goal is a [multi-language]() that combines
two independently-useful languages.
The dynamic part of the language must run any program that satisfies
a basic well-formedness condition (perhaps "no free variables", or simply
"no mismatched delimiters").
The static part of the language must check code ahead-of-time and classify
the kind of output the code will produce.
Crucially, a program must be able to combine dynamic and static parts ---
first by connecting pieces statically, and second by allowing values to
flow between parts at runtime.

<!-- > Assumptions: (1) if the dynamic code attempts an undefined operation, the -->
<!-- > language will catch the error (e.g. raise a Python `ValueError` rather than -->
<!-- > a C segfault); (2) the type system allows partial functions -->
<!--  -->
<!-- TODO does a blog post need to be so precise? Goal is attention to paper. -->

In pictures, we can think of a program as a collection of cicle-shaped pieces.
A green circle represents a dynamically-typed component,
and a blue circle represents a statically-typed component.

<img src="/img/complete-monitoring-0.png" alt="Nodes (of two colors), no edges"/>

If one cicle directly depends on another, we draw a solid line between them.
Think of a solid line as a line of code that imports some bindings.

<img src="/img/complete-monitoring-1.png" alt="Nodes and solid edges"/>

When a program runs, values flow across the edges.
In a higher-order language, these values can accept input and lead to new
interactions between new pairs of components; the shaded edges in the figure
represent these new interactions.

<img src="/img/complete-monitoring-2.png" alt="Nodes, solid edges, and dashed edges"/>

The question is, what do the runtime interactions mean for the static types?


## Types and Type Soundness

A type is a claim.
If an expression has the static type **Number** then the claim is that running
the expression will produce a number (or diverge, or an acceptable error).
Similarly for other types and expressions.

A type-sound language comes with a proof that these claims are always true.

In a fully-typed language, ... soundness statically. Mostly.


## Type Soundness in a Mixed-Typed Language

In a mixed language, more challenging for soundness.


