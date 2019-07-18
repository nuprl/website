    Title: Complete Monitoring for Migratory Typing
    Date: 2019-07-18T14:33:47
    Tags: by Ben Greenman

Complete monitoring is a key semantic property for languages that mix typed
and untyped code.
If a semantics is a complete monitor, then untyped code can trust the type
annotations and error messages can pinpoint the first mismatch between a
(possibly incorrect) type annotation and an untyped value.

This post explains
**why** complete monitoring matters and
**what** technical concepts are needed to state a complete monitoring theorem.

<!-- more -->

OUTLINE
- types are static claims about runtime values
- in a typed language, type soundness is ox
  + beware the runtime library
- in a mixed language, soundness is not enough
  + lambda-calculus example, for soundness with different behavior ?
- example ... what went wrong? bad boundary-crossing
- so, agreeing with the judgment "wrong" implies a few assumptions about the
  structure of a program: made of components, boundaries typed, value not
  allowed to cross without a full check. Express with static + dynamic axioms.
- now can say when "wrong" happens --- its when value gets multiple owners
- semantics is a complete monitor if never raises single-owner-policy errors
- STOP reflect ... ownership explicit + new error, and done
- 
- more? lifting?
