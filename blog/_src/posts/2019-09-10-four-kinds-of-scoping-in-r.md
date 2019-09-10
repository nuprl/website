    Title: Four Kinds of Scoping in R
    Date: 2019-09-10T11:00:00
    Tags: scope, r, by Ming-Ho Yee

In the [first](/blog/2019/09/05/lexical-and-dynamic-scope/) and
[second](/blog/2019/09/10/scoping-in-r/) parts of this blog series, I defined
lexical and dynamic scope, and demonstrated interesting cases of scoping in R.

In this third and final part of my blog series, I'd like to discuss a paper by
the creators of R, where they motivate the need for lexical scoping in a
statistical programming language.

This is a "bonus" blog post, because I'm going to dive into some of the hairier
R features to show how four different kinds of scoping can be simulated in R.

<!-- more -->

## Lexical scope and statistical computation

In _Lexical Scope and Statistical Computation_,[^1] Robert Gentleman and Ross
Ihaka, the creators of R, discuss why they designed R with lexical scoping. The
paper is written for a statistics audience, and they provide motivating
examples for having lexical scoping in R.

[^1]: R. Gentleman and R. Ihaka. "Lexical Scope and Statistical Computing,
_Journal of Computational and Graphical Statistics_, vol. 9, no. 3, 2000.
\[[DOI](https://doi.org/10.1080/10618600.2000.10474895)\]\[[Available
online](https://www.stat.auckland.ac.nz/~ihaka/downloads/lexical.pdf)\]

For the purpose of their discussion, they define four (slightly different)
kinds of scoping rules:

  - *trivial*: no free variables allowed
  - *static*: a free variable takes its value from a set of global variables
  - *lexical*: a free variable takes the value of the binding that was in
    effect when the function was defined
  - *dynamic*: a free variable takes the value of the most recent assignment to
    that variable

Note that under this set of definitions, _static scoping_ is a separate scoping
rule and not another name for _lexical scoping_.

It is possible to simulate each of strategies in R. For fun, we can even
construct "factories" that take a function, and then modify it to use the
desired scoping rule! (Jan Ječmen originally provided these examples to me, and
I adapted them for this blog post after some feedback from Artem Pelenitsyn.)


### Template

Our examples will follow the template given below:

```r
factory <- function(fun) {
  <???>
}

x <- 0
h <- function() {
  x <- 1
  factory(function(a) x+a)
}
g <- h()
f <- function() {
  x <- 2
  g(0)
}
f() # error, 0, 1, or 2
```

We want to define a `factory` that takes a function literal and returns a
closure that implements the desired scoping rule.

Our example consists of three definitions of `x`. On line 5, we assign `0` to
`x` at the top level. On line 7, we assign `1` to `x` inside function `h`, where
we also create the closure. On line 12, we assign `2` to `x` inside the
function `f` and right before we call `g`, which is the closure.

Finally, we call `f` and observe the result:

  - Under trivial scoping, no free variables are allowed, so `f()` should result
    in an error.
  - Under static scoping, free variables may only refer to global variables, so
    `f()` should return `0`.
  - Under lexical scoping, free variables refer to the variables in scope when
    the function was defined, so `f()` should return `1`.
  - Under dynamic scoping, free variables take the value from the most recent
    assignment, so `f()` should return `2`.

We will implement the body of `factory` in only 3-5 lines of code. The rest of
the code snippet, from lines 7 to the end, will remain the same, other than the
call to `factory` on line 10.


### Trivial scoping

```r
makeTrivial <- function(fun) {
  res <- eval(substitute(fun))
  environment(res) <- baseenv()
  res
}

x <- 0
h <- function() {
  x <- 1
  makeTrivial(function(a) x+a)
}
g <- h()
f <- function() {
  x <- 2
  g(0)
}
f() # Error in f(0) : object 'x' not found
```

`substitute` returns the unevaluated parse tree for `fun`. In other words, it
obtains the literal argument that was passed for `fun`. This works because of
call-by-need semantics in R: function arguments are packaged up into
_promises_. As a result, the syntax tree of arguments is available for
metaprogramming. A recent paper by Goel and Vitek[^2] discusses laziness in R
in more detail.

In this example, on line
8, we call `factory` with `function(a) x+a` as the argument for the formal
parameter `fun`. Then, we evaluate that parse tree with `eval`.

At this point, `res` is the closure with expression `function(a) x+a` and a
reference to the environment of `makeTrivial`. On line 3, we change that
reference to `baseenv()`, which is the environment containing library
definitions. Since this environment is above the (user) top-level environment,
global variables are not available.

Therefore, variable lookup in the function literal will only search the base
environment, so `f()` results in an error.

[^2]: A. Goel and J. Vitek. "On the Design, Implementation and Use of Laziness
in R," in _Proceedings of the ACM in Programming Languages (PACMPL)_, vol. 3,
no. OOPSLA, 2019. To appear. \[[Available
online](http://janvitek.org/pubs/oopsla19a.pdf)]

### Static scoping

```r
makeStatic <- function(fun) {
  res <- eval(substitute(fun))
  environment(res) <- globalenv()
  res
}

x <- 0
h <- function() {
  x <- 1
  makeStatic(function(a) x+a)
}
g <- h()
f <- function() {
  x <- 2
  g(0)
}
f() # 0
```

For this example, on line 3, we update the environment of `res` to refer to
`globalenv()`, which is the top-level environment where globals are defined.

Therefore, variable lookup searches the top-level environment, so `f()`
returns `0`.

### Lexical scoping

```r
makeLexical <- function(fun) {
  res <- eval(substitute(fun))
  environment(res) <- parent.frame()
  res
}

x <- 0
h <- function() {
  x <- 1
  makeLexical(function(a) x+a)
}
g <- h()
f <- function() {
  x <- 2
  g(0)
}
f() # 1
```

Although lexical scoping is the default for R, our factory template requires
some metaprogramming to work properly. We need to set the environment of `res`
to `parent.frame()`, which is the environment of the function (`h`) that called
the current function (`makeLexical`). This allows us to simulate lexical
scoping, as if the function literal was evaluated inside `h`, rather than
`makeLexical`.

Therefore, variable lookup searches the environment of `h`, so `f()` returns
`1`.

### Dynamic scoping

```r
makeDynamic <- function(fun) {
  function(...) {
    res <- eval(substitute(fun))
    environment(res) <- parent.frame()
    res(...)
  }
}

x <- 0
h <- function() {
  x <- 1
  makeDynamic(function(a) x+a)
}
g <- h()
f <- function() {
  x <- 2
  g(0)
}
f() # 2
```

For this example, we need another level of indirection. `makeDynamic` returns
an anonymous function literal. The anonymous function takes `...`, which
represents an arbitrary list of arguments, and then on line 5 we call `res`
with those exact arguments. Note that we set the environment of `res` to be the
environment of the _caller_ of the anonymous function. Because of the multiple
levels of indirection, the caller is `f`, on line 17.

On line 12, `makeDynamic` returns a closure for the anonymous function. `h`
returns that closure when it is called, and assigns it to `g`. When `g` is
called on line 17, the function literal `function(a) x+a` is finally evaluated,
and its environment is set to the environment of `f`, the caller of `g`.

Therefore, variable lookup searches the environment of `f`, so `f()` returns
`2`.


## Conclusion

Hopefully this blog post has shown another way of looking at scoping
definitions. As discussed in the [previous
post](/blog/2019/09/10/scoping-in-r/), it's very easy to get confused because
different definitions are used by different people. Here, Gentleman and Ihaka
very clearly state what definitions they are using.

And finally, while I am far from an expert on metaprogramming in R, I hope this
post has given a taste of what is possible.

_I would like to thank Jan Ječmen for coming up with and showing me the
original versions of these code examples, and Artem Pelenitsyn for his feedback
to improve and not discard these examples from an earlier blog draft._

---

## References
