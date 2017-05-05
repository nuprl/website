    Title: Rank Polymorphism
    Date: 2017-05-04T18:26:48
    Tags: by Justin Slepak, array language, 


Rank polymorphism gives you code reuse on arguments of different dimensions.
Take a linear interpolation function (let's just call it `lerp`) for scalars:

    (λ ((lo 0) (hi 0) (α 0)) (+ (* lo (- 1 α)) (* hi α)))

The number marks on each argument indicate the expected "rank" of the argument:
how many dimensions it should have.
In this case, each one is marked `0`,
indicating a scalar (*i.e.*, 0-dimensional) argument.
The function is usable as-is for

* α-blending two RGB pixels

* dimming or brightening an image

* fade transition between video scenes

<!-- more -->

Each of these use cases mixes the argument dimensions a little differently.
A pixel is a vector (a rank-1 structure) of numbers representing color channel values,
so the α-blending case uses two vector arguments and one scalar argument.

The only real difference between these use cases is the iteration space:
they're all effectively loop nests around the same basic scalar operation.
In a rank-polymorphic language,
the iteration space is derived automatically from the data,
so you don't need to write out the control structure yourself.

The fundamental idea behind function application here is
breaking the argument arrays into lower-ranked pieces called "cells."
Each cell has the rank expected by the function being applied.
In the case of `lerp`, the pixels, images, videos, etc.
are all broken up into rank-0 (scalar) cells
because `lerp` expects rank-0 arguments.
Other expected ranks are possible as well—
a vector dot product function `dot-prod` would call for rank-1 cells,
and a matrix inversion function `minv` would call for rank-2 cells.

The structure built up around the cells is called the "frame."
A matrix array is a rank-2 frame containing rank-0 cells for `lerp`,
but it would be a rank-1 frame containing rank-1 cells for `dot-prod`
and a rank-0 frame containing a single rank-1 cell for `minv`.
A rank-*n* array could be broken down into a frame of cells in *n+1* different ways,
and it's the function being applied that determines which decomposition to use.

Unfortunately, the implicit control structure
that's so convenient for the programmer
is a problem for a compiler.
Historically, implementations of such languages have had to
do without static information about the iteration space.
Interpreters (and line-at-a-time compilers, to a lesser extent)
get to inspect the concrete data they're dealing with,
but static compilers have had to make do with emitting a generic loop structure.
A "loop" over a scalar might sound like trivial overhead,
but not when it appears within some other hot loop.
Being unable to see when loop boundaries match up is also a barrier to loop fusion.
The lack of thorough static shape information was a long-standing problem
my advisor pointed out to me when I was a new student looking at possible research projects,
and he was interested in applying some form of static analysis to gather that information.

The first step in addressing it was to come up with
a formal semantics for rank polymorphism.
Although [APL has existed since the 1960s](http://www.jsoftware.com/papers/APL.htm),
it had mostly lived in a separate world from mainstream programming language research.
The formal techniques developed in PL had seen little to no
application to APL and its "successor" language J.

There's a lot to dislike about APL and J—special
case behavior in many of the primitive operators,
limited function arity,
syntactic separation of first-order and second-order functions,
the impossibility of parsing an entire program at once
(fun fact: static analysis
[has been tried](http://dl.acm.org/citation.cfm?id=805380) there)—and
of course the idiosyncratic identifiers used for primops
have prompted plenty of internet arguments.
None of those things are essential to the programming model,
so I'm [building a new language called Remora](http://www.ccs.neu.edu/home/jrslepak/proposal.pdf)
to isolate the aspects I want to study.

People don't always think of a type system as a form of static analysis,
but it turned out to be an effective way of gathering shape information.
Remora's [type system](http://www.ccs.neu.edu/home/jrslepak/esop14-full.pdf)
uses a restricted form of dependent types,
in the style of [Dependent ML](https://www.cs.cmu.edu/~rwh/theses/xi.pdf).
An array type is indexed by the shape,
the numeric sizes of the array's individual dimensions.
Index polymorphism (*i.e.*, Π types) allows functions to work on varying cell shapes
and even varying cell ranks
(which is essential for primitives like `append` and `reduce`,
which operate along the major axis of arrays, no matter their rank).
Frame-rank polymorphism, which gives rise to the control structure,
remains completely implicit,
leaving it to be identified by the type rule for function application.
As a nice bonus, type soundness rules out
run-time errors arising from incompatible argument shapes.


- - -

_If you liked this post, you may also be interested in:_

- [History of Actors](http://prl.ccs.neu.edu/blog/2016/10/19/history-of-actors/)
- [Datalog for Static Analysis](http://prl.ccs.neu.edu/blog/2017/02/21/datalog-for-static-analysis/)
