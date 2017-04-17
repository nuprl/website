    Title: Type-Directed Compilation, Parts I and II
    Date: 2017-04-17T12:00:17
    Tags: HOPL, by Leif Andersen, by William J. Bowman

<!-- more -->

### Part I: _Type-Directed Compilation_, by Leif Andersen.

In this talk we discuss the history of type directed compilation. We start with
Xavier Leroy's seminal paper: [_Unboxed Objects and Polymorphic
Typing_](http://gallium.inria.fr/~xleroy/publi/unboxed-polymorphism.pdf),
continue to [TIL](https://www.cs.cmu.edu/~rwh/papers/til/pldi96.pdf) (Typed
Intermediate Language), and finish up with
[TAL](https://dash.harvard.edu/handle/1/2797451) (Typed Assembly Language). We
talk about what it means for a compiler to be typed preserving, and give
examples of optimizations that are enabled by types.

Discussion summary:

- [https://github.com/nuprl/hopl-s2017/blob/master/lecture_notes/2017-03-24.md](https://github.com/nuprl/hopl-s2017/blob/master/lecture_notes/2017-03-24.md)


### Part II: _Dependent Type-Directed Compilation_, by William J. Bowman

A certifying compiler is not verified, but it produces a proof of correctness
for each binary.
This proof can be independently checked to show that the binary was compiled
correctly, removing the compiler from the trusted code base.
Certifying compilation has its roots in preserving type-preserving compilation,
and in particular in preserving dependent types.
We start the history of dependent-type-preserving compilation with a compiler
from C to Assembly.
We'll see a result showing that preserving dependent types isn't possible, and
then we'll do it anyway.

Discussion summary:

- [https://github.com/nuprl/hopl-s2017/blob/master/lecture_notes/2017-03-28.md](https://github.com/nuprl/hopl-s2017/blob/master/lecture_notes/2017-03-28.md)


Notes (to appear here, eventually):

- [https://github.com/nuprl/hopl-s2017/blob/master/dependent-type-preserving-compilation](https://github.com/nuprl/hopl-s2017/blob/master/dependent-type-preserving-compilation)
