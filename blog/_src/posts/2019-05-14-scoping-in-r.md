    Title: Scoping in R
    Date: 2019-05-14T10:28:00
    Tags: scoping, r, by Ming-Ho Yee

This all started with a simple question about the R programming language:
_is R lexically or dynamically scoped?_

To answer that question, we need to understand what _scoping_ means, along with
_lexical scope_ and _dynamic scope_.

<!-- more -->

## What is scoping?

_Scoping_ refers to the places in a program where a variable is visible and can
be used. In particular, the interesting situation is when a function has free
variables. Consider the example below:

```r
x <- 1
f <- function(a) x + a
g <- function() {
  x <- 2
  f(0)
}
g() # what does this return?
```

In this snippet, we define a variable `x` at the top level and two functions `f`
and `g`. The body of `g` uses the parameter `a`, but also the free variable `x`.
On line 7, we call `g`, which in turn calls `f` and returns that result.
(In this language, the last expression of a function is implicitly returned.)

What value does `g` return when it is called?

To answer that question, we need to know what value `x` has on line 2. Is it
bound to the `x` defined on line 1, in the same lexical environment that `f` was
defined? Or is it bound to the `x` defined on line 4, in the same dynamic
environment that `f` was called?


### Lexical scoping

Under _lexical scoping_ (also known as _static scoping_), a free variable is
evaluated in its _lexical environment_, i.e., where it is defined. In the example
above, the free variable `x` is used in the function `f`, which is defined at
the top-level environment. At that point in the program, there is a definition
of `x` visible, with value `1`.

Therefore, under lexical scoping, the example program returns `1`.

Most programming languages we use today are lexically scoped. In principle, a
compiler could statically determine what values free variables are bound to.


### Dynamic scoping

Under _dynamic scoping_, a free variable is evaluated in its _dynamic
environment_, i.e. where it is called. In other words, the variable is bound to
the value last assigned to it (at run time). In our example, just before `f` is
called, `x` is assigned the value `2`.

Therefore, under dynamic scoping, the example program returns `2`.

Dynamically scoped programming languages include bash and LaTeX. Emacs Lisp is
dynamically scoped, but allows the programmer to opt into lexical scoping.
Conversely, Perl and Common Lisp are lexically scoped by default but allow the
programmer to choose dynamic scoping if desired.


## What about R?

The example is written in valid R. If you run it, you will see that the result
is `1`. Therefore, according to our litmus test, **R is lexically scoped**. But
is this the whole story?

In _[Evaluating the Design of the R Language][mhov2012]_ (ECOOP 2012)
([doi][mhov2012-doi]), Morandat, Hill, Osvald, and Vitek write:

> As is often the case, R is lexically scoped up to the point it is not. R
> is above all a dynamic language with full reflective access to the running
> program’s data and representation.

[mhov2012]: http://janvitek.org/pubs/ecoop12.pdf
[mhov2012-doi]: https://doi.org/10.1007/978-3-642-31057-7_6


### Circumventing scoping in R

There are many ways to "defeat" scoping in R---made easier because it supports
explicit manipulation of environments.

Here are some different tricks we can try.

#### Evaluating arbitrary code

```r
x <- 1
f <- function(a) {
  eval(parse(text = "x <- 0"))
  x + a
}
g <- function() {
  x <- 2
  f(0)
}
g() # 0
```

On line 3, we parse the string `"x <- 0"` into an unevaluated R expression and
then evaluate it. This assigns `0` to `x`, which is no longer a free variable
when it is used on line 4.

#### Simulating dynamic scope

```r
x <- 1
f <- function(a) {
  get("x", envir = parent.frame()) + a
}
g <- function() {
  x <- 2
  f(0)
}
g() # 2
```

On line 3, we perform an explicit variable lookup for `x`, but we do so in the
environment `parent.frame()`, which refers to the calling function's
environment.

#### Constructing an arbitrary environment

```r
x <- 1
f <- function(a) {
  e <- new.env()
  e$x <- 3
  get("x", envir = e) + a
}
g <- function() {
  x <- 2
  f(0)
}
g() # 3
```

On line 3, we construct an empty environment, `e`. Next, on line 4, we add a
new binding to that environment, where `x` has the value `4`. Finally, on line
5, we perform a variable lookup in environment `e`.

#### Deleting bindings

```r
x <- 1
f <- function(a) {
  rm("x", envir = parent.env(environment()))
  x + a
}
g <- function() {
  x <- 2
  f(0)
}
g() # Error in f(0) : object 'x' not found
```

On line 3, we delete the binding `x` from the given environment,
`parent.env(environment())`. This refers to the parent (or enclosing)
environment of the current environment of `f`, i.e. the environment where `f`
was defined, which is the top-level environment.


### What does "scoping in R is dynamic" mean?

In _Optimizing R Language Execution via Aggressive Speculation_ (DLS 2016)
([doi][swhj2016-doi]), Stadler, Welc, Humer, and Jordan write:

> [...] variable scoping in R is dynamic and can be modified at the language
> level [...]

[swhj2016-doi]: https://doi.org/10.1145/2989225.2989236

Our previous examples should demonstrate that scoping in R "can be modified at
the language level."

It is also true that "variable scoping in R is dynamic," in the sense that
scoping is determined at run time. However, this is more general than the
definition of _dynamic scoping_ we used in this blog post.

Similarly, I've seen the term _lexical scoping_ used with a less specific but
stronger definition---that all variable bindings can be determined at compile
time. Under these more general definitions, R would be dynamically scoped
rather than lexically scoped.

I prefer the more specific definitions of lexical and dynamic scoping; I think
they're more useful. Either way, it's important to be clear and make sure
your audience knows what definitions you're using!

---

## Extra: another taxonomy for scoping rules

In _[Lexical Scope and Statistical Computation][gi2000]_ (2000)
([doi][gi2000-doi]), Gentleman and Ihaka, the creators of R, define four kinds
of scoping rules for the purpose of their discussion:

[gi2000]: https://www.stat.auckland.ac.nz/~ihaka/downloads/lexical.pdf
[gi2000-doi]: https://doi.org/10.1080/10618600.2000.10474895

  - *trivial*: no free variables allowed
  - *dynamic*: as we defined it; free variables are evaluated in their dynamic
     context
  - *static*: values of free variables are defined by global variables, e.g.
    scoping in C
  - *lexical*: as we defined it; free variables are evaluated in their lexical
     context

The paper is written for a statistics audience, and the authors explain how
lexical scoping in R is useful. They provide motivating examples from
statistical computing, illustrating how lexical scoping in R is an advantage
over the scoping rules of S, the language R was based on.

My colleague, Jan Ječmen, came up with examples of the four scoping rules.
(I edited them slightly for this blog post.)


### Trivial scoping

```r
x <- 0
makeTrivial <- function() {
  x <- 1
  res <- function(a) a + x
  trivEnv <- new.env(parent = emptyenv())
  trivEnv$"+"   <- `+`
  environment(res) <- trivEnv
  res
}
f <- makeTrivial()
g <- function() {
  x <- 2
  f(0)
}
g() # Error in f(0) : object 'x' not found
```

On line 5, we construct a new environment, setting its outer environment to be
the empty environment. On line 6, we provide a binding for the `+` function.
Then on line 7, we set this environment to be the environment of `res`. Notably,
`x` is not given a binding.


### Static scoping

```r
x <- 0
makeStatic <- function() {
  x <- 1
  res <- function(a) a + x
  environment(res) <- globalenv()
  res
}
f <- makeStatic()
g <- function() {
  x <- 2
  f(0)
}
g() # 0, refers to `x` on line 1
```

On line 5, we set the top-level environment to be the environment of `res`.
Therefore, the free variable `x` refers to the `x` defined at the top level.


### Lexical scoping

```r
x <- 0
makeLexical <- function() {
  x <- 1
  res <- function(a) a + x
  res
}
f <- makeLexical()
g <- function() {
  x <- 2
  f(0)
}
g() # 1, refers to `x` on line 3
```

Since this is the default scoping for R, no changes are required. The free
variable `x` on line 4 refers to the definition on line 3.


### Dynamic scoping

```r
x <- 0
makeDynamic <- function() {
  x <- 1
  res <- function(a) {
    `parent.env<-`(environment(), parent.frame())
    a + x
  }
  res
}
f <- makeDynamic()
g <- function() {
  x <- 2
  f(0)
}
g() # 2, refers to `x` on line 12
```

In R, every environment has a reference to its parent environment, which is the
outer lexical environment. On line 5, we call the `parent.env<-` function to
overwrite the parent environment of `environment()`, the current environment.
Specifically, we set the parent environment to `parent.frame()`, which refers to
the environment of the function that calls `res`.

---

_If you liked this post, you may also be interested in the following Twitter
threads about R: [one][], [two][] and [three][]._

[one]: https://twitter.com/mhyee/status/1063983175163158531
[two]: https://twitter.com/mhyee/status/1067818720532316166
[three]: https://twitter.com/mhyee/status/1074744049951739905
