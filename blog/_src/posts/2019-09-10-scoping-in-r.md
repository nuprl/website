    Title: Scoping in R
    Date: 2019-09-10T10:00:00
    Tags: scope, r, by Ming-Ho Yee

In the [previous post](/blog/2019/09/05/lexical-and-dynamic-scope/) of this
three-part blog series, we discussed lexical and dynamic scope. Now, in this
second part, we can return to the original question: _is R lexically or
dynamically scoped?_

<!-- more -->

Recall the example program from before:

```r
x <- 1
f <- function() x
g <- function() {
  x <- 2
  f()
}
g() # what does this return?
```

Let's examine what happens when we run this example. First, we create a mapping
for `x` in the top-level environment. On line 2, we define a function `f`, which
returns the value of some `x`. On line 3, we define a function `g`, which
creates a new mapping for `x`, and then calls `f`. Note that the assignment on
line 4 does _not_ update the definition on line 1.

When `f` is called, it needs to look up the value of `x`. In other words, does
the reference of `x` on line 2 refer to the assignment on line 1 or the
assignment on line 4? If `f` returns `1`, then the behaviour matches lexical
scoping. If it returns `2`, then the behaviour matches dynamic scoping.

When we run this example, the result is `1`. This implies that R is lexically
scoped.

But there's more to this story. In the rest of this blog post, I'll examine
some interesting scoping examples in R, and discuss how the scoping definitions
relate to R.

The [next and final part](/blog/2019/09/10/four-kinds-of-scoping-in-r/) of this
blog series, published simultaneously with this one, is an appendix where I
implement four different scoping disciplines in R.


## R is lexically scoped, but...

In _Evaluating the Design of the R Language_,[^1] Morandat, Hill, Osvald, and
Vitek write:

> As is often the case, R is lexically scoped up to the point it is not. R
> is above all a dynamic language with full reflective access to the running
> program’s data and representation.

[^1]: F. Morandat, B. Hill, L. Osvald, J. Vitek. "Evaluating the Design of the
R Language," in _Proceedings of the European Conference on Object-Oriented
Programming (ECOOP)_, 2012.
\[[DOI](https://doi.org/10.1007/978-3-642-31057-7_6)\]\[[Available
online](http://janvitek.org/pubs/ecoop12.pdf)\]

In other words, R provides many different "escape hatches"---ways to bypass
lexical scoping. Additionally, even without escape hatches, some of R's
functionality can be surprising.


### Functions, environments, and variables in R

Before we look at some examples, I think it's useful to briefly discuss some of
the core concepts in R that relate to scoping.

  - **Functions.** R has first-class functions, and functions evaluate to
    closures. In other words, a function value includes both the body of the
    function as well as the environment that the function was evaluated in. In
    R, the programmer can modify the environment of a closure. Note that R is
    function scoped; there is no block scoping.

  - **Environments.** An environment in R is a mapping from variables to
    values. Each function has its own local environment. Furthermore, each
    environment has a reference to the "enclosing" environment that it was
    evaluated in. R environments are first-class, meaning the programmer can
    add, modify, or removing variable mappings, and also change the reference to
    the enclosing environment.

  - **Variable lookup.** When R looks up a variable, it will search in the
    current environment for a mapping. If no mapping is found, then it will
    search in the enclosing environment. This process continues until a mapping
    is found, or the topmost, empty environment is reached, in which case an
    error is raised.

  - **Variable assignment.** `<-` is the variable assignment operator in R. The
    expression `x <- 1` assigns the value `1` to the variable `x` in the
    current environment. If a mapping for `x` already exists in the
    environment, then the assignment will update and overwrite the existing
    value. Otherwise, a new mapping is created in the environment. Note that
    variable assignment can only update the current environment, and never
    creates a scope.

From this description, we can see that R implements lexical scoping (or at
least, something that behaves a lot like lexical scoping): each function value
is associated with the environment it was evaluated in, and variable lookup
proceeds along the chain of enclosing environments. In fact, the creators of R
have confirmed that lexical scoping was their intent.[^2]

On the other hand, variable lookup depends on the run-time state of the
program---names cannot be resolved statically. Furthermore, since R provides
operations for environment manipulation, a programmer can easily circumvent
lexical scoping.

The following examples will make this clear.

[^2]: R. Gentleman and R. Ihaka. "Lexical Scope and Statistical Computing",
_Journal of Computational and Graphical Statistics_, vol. 9, no. 3, 2000.
\[[DOI](https://doi.org/10.1080/10618600.2000.10474895)\]\[[Available
online](https://www.stat.auckland.ac.nz/~ihaka/downloads/lexical.pdf)\]


### Examples

#### Adding variable mappings at run time

```r
x <- 1
f <- function() {
  g <- function() x
  x <- 2
  g()
}
f() # 2
```

When `f` is called, it creates a function `g` that returns `x`, assigns `2` to
`x`, and then calls `g`. When `g` is called, it looks up `x`. Since no mapping
is found in `g`'s environment, it searches in the enclosing environment
(`f`'s), and finds that `x` has value `2`. Therefore, `g` returns `2`.

Note that the `x` on line 3 is resolved only when function `g` is called, not
when it is defined. However, when `g` is defined, its environment has a
reference to `f`'s environment. Therefore, as long as `x` is defined _before_
`g` is called, the lookup will always succeed.

Here's a second example:

```r
x <- 1
f <- function(b) {
  if (b)
    x <- 2
  x
}
f(TRUE)  # 2
f(FALSE) # 1
```

`f` is a function that branches on its argument, `b`. If `b` evaluates to true,
then the expression `x <- 2` is evaluated, and a mapping for `x` is created in
`f`'s environment. Otherwise, no mapping is created.

When we look up the value of `x` on line 5, R will first search the function's
environment. If `b` evaluated to true, then R will find a value for `x`, which
is `2`. Otherwise, R will search in the enclosing environment of `f`, and find
that `x` is `1`.

Both of these examples vaguely resemble dynamic scoping, in that `x` takes the
value of the most recent assignment. However, this is not how R is implemented,
and it is not consistent with how R behaves in other examples.

#### Function lookup

```r
f <- function(x) x
g <- function(f) {
  f(0) # not an error
}
g(42) # 0
```

R has slightly different lookup rules, if the variable is in function call
position. Specifically, R will search the environment chain and skip
non-function values.

In this example, we call `g` with the argument `42`, which is not a function.
Then, in the body of `g`, we call `f(0)` on line 3, which requires looking up
`f`. Although there is an `f` in the environment of `g`, its value is `42`,
which is not a function. R will then search the enclosing environment, where it
finds the function defined on line 1. Therefore, the lookup on line 3 resolves
to the function on line 1, so `f(0)` returns `0`.

This behaviour exists because `c` is the built-in function that constructs
vectors (in other words, one of the most commonly used functions in R), but it
is also a commonly used argument name.

#### Super assignment

```r
x <- 0
f <- function() {
  x <- 1
  x <<- 2
  x
}
f() # 1
x   # 2
```

`<<-` is the "super assignment" operator. It skips the current environment and
then searches the chain of enclosing environments until it finds a variable to
update. If no variable is found, then a new mapping is created at the top
environment.

In the above program, we define `x` to be `0` at the top level, and then define
the function `f`. When we call `f` on line 7, it assigns `1` to `x` on line 3,
which creates a mapping in the local environment. On line 4, the super
assignment skips the mapping in the local environment and instead updates the
mapping created on line 1. Next, `f` returns `x`, which is looked up from the
local environment and has value `1`. Finally, line 8 looks up `x` from the top
level environment, which has value `2`.

#### Evaluating arbitrary code

```r
x <- 1
f <- function(t) {
  eval(parse(text = t))
  x
}
g <- function() {
  x <- 2
  f("x <- 0")
}
g() # 0
```

R has a mechanism for converting an arbitrary string to code and then executing
it. On line 3, we parse and evaluate the argument `t`, which happens to be the
string `"x <- 0"`. Then, when line 4 executes, the lookup of `x` returns `0`.

#### Simulating dynamic scope

```r
x <- 1
f <- function() {
  get("x", envir = parent.frame())
}
g <- function() {
  x <- 2
  f()
}
g() # 2
```

On line 3, we perform an explicit variable lookup for `x`, but we do so in the
environment `parent.frame()`, which refers to the calling function's
environment, in this case, `g`'s environment.. Therefore, the lookup returns
`2`.

Note that R has a similarly named function, `parent.env(e)` which returns the
_enclosing environment_ of the given environment `e`.

#### Constructing an arbitrary environment

```r
x <- 1
f <- function() {
  e <- new.env()
  e$x <- 3
  get("x", envir = e)
}
g <- function() {
  x <- 2
  f()
}
g() # 3
```

When `f` is called, it constructs a new environment, `e`, which is initially
empty. (By default, its enclosing environment is the current environment, which
is `f`'s.) Next, on line 4, it directly adds a mapping to that environment,
assigning `3` to `x`. Then, on line 5, the lookup is explicitly done in
environment `e`, so `f` returns `3`.

#### Deleting mappings

```r
x <- 1
f <- function() {
  rm("x", envir = parent.env(environment()))
  x
}
g <- function() {
  x <- 2
  f()
}
g() # Error in f() : object 'x' not found
```

Not only is it possible to dynamically add and modify mappings in R, but it is
also possible to _delete_ mappings. This is what line 3 does: it explicitly
removes the mapping for `x` from the enclosing environment of the current
environment. In other words, the definition on line 1 is deleted. Therefore,
when `f` is called, the lookup of `x` fails and an error is raised.

#### Infinite loop during variable lookup

```r
enva <- new.env()
envb <- new.env()
parent.env(enva) <- envb
parent.env(envb) <- enva
f <- function() x
environment(f) <- enva
f()
```

In this final example, manipulation of environments allows us to create a
function where variable lookup results in an infinite loop.

On lines 1 and 2, we create new, empty environments. Both have the same
enclosing environment, which is the top-level environment. However, on lines 3
and 4, we modify their enclosing environments to create a cycle: `enva`'s
enclosing environment is `envb`, and `envb`'s enclosing environment is `enva`.

On line 5, we define a function with a free variable, `x`, but on line 6, we set
`f`'s environment to be `enva`. Finally, we call `f`.

When the body of `f` is evaluated, it needs to look up `x`. Lookup starts in
`f`'s environment, which we set to be `enva`. Since no mapping for `x` is
found, lookup continues in `enva`'s enclosing environment, which is `envb`.
However, `envb` is also empty, so lookup continues in its enclosing
environment, which is `enva`, and now lookup results in an infinite loop.


### An intuition for scoping in R

Some of the above examples appear to demonstrate dynamic scoping. Recall two of
our examples:

```r
# example 1
x <- 1
f <- function() {
  g <- function() x
  x <- 2
  g()
}
f() # 2

# example 2
x <- 1
f <- function(b) {
  if (b)
    x <- 2
  x
}
f(TRUE)  # 2
f(FALSE) # 1
```

It seems that `x` takes on the value of the last assignment, but we know this
is not the case, from the first example. This is also not how R is implemented.
What's missing from our intuition?

The key insight is that R is _function scoped_. In R, each function has an
associated environment, and that environment implements a scope. In general,
only a function definition can create a scope. Therefore, the assignment
operator `<-` _does not create a new scope_, and it is more useful to think of
it as a mutation _on the current environment_. (In contrast, in most languages,
a variable binding or definition creates a new scope, and an assignment mutates
that variable.)

In a sense, it might be more accurate to say that R _environments_ are
lexically scoped, variables are scoped to functions (but a reference can occur
syntactically before a definition), and variable assignment is an update to the
environment.


## Discussion

All of this might make you a little uncomfortable, and uncertain about R's
scoping rules.

On one hand, R passes the first example program as a lexically scoped language,
the implementation of closures and variable lookup imply "lexical-like"
behaviour, and the creators have confirmed that lexical scoping was the intent.

On the other hand, variable lookup depends on the run-time state of the
program, and variable bindings cannot be resolved statically. Some of the
examples even resemble dynamic scoping, where a free variable takes the value
of the most recent assignment---but this is not consistent with R's behaviour
in other examples. Furthermore, the dynamic nature of R and its reflection and
metaprogramming capabilities allow programmers to completely circumvent lexical
scoping.

This ambiguity shows up in a paper,[^3] where the authors write:

> Furthermore, because variable scoping in R is dynamic and can be modified at
> the language level [...] it cannot be trivially guaranteed that `x` is going
> to point to the same data structure throughout the entire execution of the
> loop.

[^3]: L. Stadler, A. Welc, C. Humer, and M. Jordan. "Optimizing R Language
Execution via Aggressive Speculation," in _Proceedings of the Symposium on
Dynamic Languages (DLS)_, 2016.
\[[DOI](https://doi.org/10.1145/2989225.2989236)\]

It is true that a variable `x` may not point to the same data structure during
the execution of a loop. It is true that scoping in R can be modified at the
language level.

It is true that variable _lookup_ is dynamic, as it is performed at run time
and depends on the run-time program state. If that is your definition of
_dynamic scope_, then it would be fair to say that R is dynamically scoped.

But if your definition of _dynamic scope_ is "a variable is bound to the most
recent assignment during the program's execution," then it is not correct to
say R is dynamically scoped.

I think we have this ambiguity because _scope_ (the places in a program where a
variable can be referenced) and _variable lookup_ or _name resolution_
(determining which binding or definition a name refers to) are often considered
together. For most lexically scoped languages, name resolution can be done at
compile time. For most dynamically scoped languages, name resolution must be
done at run time. R is lexically scoped, but must perform name resolution at
run time.

Personally, I prefer the definition of _scope_ that treats name resolution as
an orthogonal issue. I think it is more useful to keep the two issues separate.
In addition, I think it is confusing and unhelpful to say that R is _both_
lexically and dynamically scoped, or that R is _neither_ lexically and
dynamically scoped.

I think it is more helpful to treat R as a lexically scoped language (with
certain exceptions and surprises) than as a dynamically scoped language---when
I read and write R code, I find it more convenient to think about nested
function definitions and free variables in terms of lexical scoping rules. And
I think that it is more accurate, based on the design and implementation, to
classify R as a lexically scoped language.

Regardless, it is very easy to miscommunicate, so I think it's important to be
very clear and make sure you and your audience know what definitions of scoping
you're using!


## Conclusion

This entire adventure started when we were working on a paper,[^4] and asked
each other, is R lexically or dynamically scoped? Eventually, it became
apparent that we had different definitions of lexical and dynamic scope, so of
course we were unable to agree on an answer!

This got me interested in exploring definitions of scope, the history of lexical
scope, and how R fits with traditional definitions of lexical scope. The result
was this mini blog series.

To summarize, I would say that _scope_ refers to the places in a program where
a variable is visible and can be referenced. Under _lexical scoping_, the scope
of a variable is determined by the lexical (_i.e._, textual) structure of a
program. Under _dynamic scoping_, a variable is bound to the most recent value
assigned to that variable, _i.e._, the most recent assignment during the
program's execution.

I would say that R _aims_ to be lexically scoped---it was part of the design and
implementation, but certain features make the situation more complicated. In
particular, variables are function scoped, definitions do not introduce new
scopes, and variable lookup is performed at run time. Furthermore, the dynamic
nature of R and its metaprogramming capabilities allow programmers to completely
circumvent lexical scoping.

Finally, there are some definitions of lexical and dynamic scope that also
consider variable lookup. Under these definitions, R might be considered a
dynamically scoped language, since variable lookup happens at run time.
Therefore, it is important to be precise about your definitions!

If you want more content about R and scoping, the [third and final
part](/blog/2019/09/10/four-kinds-of-scoping-in-r/) of this blog series is
already published. In it, I walk through four different examples of using
metaprogramming to simulate different scoping disciplines in R.

**Edited 2020/02/21:** For another discussion on R environments and lookups,
(and also packages and namespaces, which I did not cover in my post), [this blog
post](http://blog.obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/) has some
nice examples and diagrams.


[^4]: O. Flückiger, G. Chari, J. Ječmen, M.-H. Yee, J. Hain, and J. Vitek. "R
Melts Brains: An IR for First-Class Environments and Lazy Effectful Arguments,"
in _Proceedings of the Symposium on Dynamic Languages (DLS)_, 2019. To appear.
\[[Available online](http://janvitek.org/pubs/dls19.pdf)\]

_I would like to thank Sam Caldwell, Guido Chari, Oli Flückiger, Aviral Goel,
Ben Greenman, Jakob Hain, Jan Ječmen, Hugo Musso Gualandi, Artem Pelenitsyn,
and Jan Vitek for their comments, feedback, and discussions that have greatly
improved and shaped this blog post._

_If you liked this post, you may also be interested in the following
Twitter threads about R: [one][], [two][] and [three][]._

[one]: https://twitter.com/mhyee/status/1063983175163158531
[two]: https://twitter.com/mhyee/status/1067818720532316166
[three]: https://twitter.com/mhyee/status/1074744049951739905

---

## References
