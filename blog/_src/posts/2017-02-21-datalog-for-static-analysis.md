    Title: Datalog for Static Analysis
    Date: 2017-02-21T12:58:27
    Tags: HOPL
    Authors: Ben Greenman

<!-- more -->

Datalog is an old DSL that frequently appears in work on static analysis.
This edition of [HOPL 2017](/blog/2017/02/15/introducing-hopl-2017/) explores the origins of Datalog in general, its
early use in program analysis, and why Datalog remains a useful tool.

Full notes:

- [Local Copy](/blog/static/datalog-for-static-analysis.pdf)
- [Source of Truth](https://github.com/nuprl/hopl-s2017/tree/master/datalog-for-static-analysis)

- - -

Datalog as a language was introduced by 1978 (its semantic foundations date
back to 1976). It is _predicate logic_ as a database query language. The
traditional view of a Datalog program is a _time invariant_ transformation
over the _time varying_ data stored in an external database.

In the early 1990's, Uwe Aβmann designed a graph rewriting systems (EARS) that
could:

1. Uniformly express various problems in static analysis
2. Systematically derive efficient solutions to such problems.

(Prior work had derived the same solutions with ad-hoc methods.) Aβmann's system
is equivalent to Datalog.

In 1993, Reps used the <tt>CORAL</tt> deductive database (an implementation of
Datalog) to derive an on-demand (read: lazy) implementation of program slicing
from a _specification_ of the slicing problem.

Both Aβmann's and Reps work appeared in 1994. This was the first time Datalog
had been used to implement a static analysis.

Researchers continue to use Datalog because:

- predicate logic (specifically: Horn clauses without function symbols or negation)
  is useful for expressing recursive relations ... and static analyses are all about recursive relations
- the language separates _specifications_ from their _implementation_
- there are many techniques for efficiently serving a Datalog query
- these techniques have been implemented in [at least one](https://developer.logicblox.com/wp-content/uploads/2016/01/logicblox-sigmod15.pdf)
  commercial Datalog engine

For an excellent description of how Datalog can benefit static analysis, see
the introduction to [Rep's paper](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.648.1834&rep=rep1&type=pdf).

