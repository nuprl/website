    Title: Transient for Optional and Keyword Functions
    Date: 2020-11-12T10:15:16
    Tags: typed racket, transient, by Ben Greenman

A short adventure into the depths of optional and/or keyword
 functions in Racket.

<!-- more -->

- - -

Transient, or rather _the Transient semantics for a mixed-typed language_,
 is one way to let statically-typed code safely interact with untyped code.
You can read all about it in
 [Michael Vitousek's 2019 dissertation](http://hdl.handle.net/2022/23172)
 or [my 2020 dissertation](https://ccs.neu.edu/home/types/publications/publications.html#g-dissertation-2020),
 and you can see how it compares to other mixed-typed semantics
 [here](http://prl.ccs.neu.edu/blog/2018/10/06/a-spectrum-of-type-soundness-and-performance/).
The idea is to give up on [behavioral type guarantees](http://prl.ccs.neu.edu/blog/2019/10/31/complete-monitors-for-gradual-types/)
 and focus on a kind of type soundness.
To enforce soundness, Transient rewrites every expression in typed code
 with assertions called _shape checks_; for example:

- if a typed module imports an untyped library, then every value that crosses
  the module boundary gets a shape check;
- if typed code reads from an array, then every element that comes out must
  satisfy a shape check; and
- if a typed function escapes to untyped code, then the function must use
  a shape check to validate every input that it receives.

Our goal today is to understand the shape checks for functions.
Suppose we know how to turn a type **T** into a shape check, and we have a
 function with type **T** that needs to check its inputs.
The question is how to actually do the check in Racket.

In your standard theory, rewriting is no problem.
A (simplified, model) function takes exactly one argument and needs exactly one
 shape check in the body; if **T = (-> Symbol Symbol)** then we need
 to check the shape (**symbol?**) of the domain type (**Symbol**):

```
;; source code
(: f (-> Symbol Symbol))
(define (f sym)
  sym)

;; ===>

;; imaginary (but realistic) rewritten code
(define (f sym)
  (assert symbol? sym)
  sym)
```

A Typed Racket function can accept optional arguments, keyword arguments,
 and optional keyword arguments.
These are still fairly easy to handle in theory.
Below, the function type **T** accepts 1 to 3 inputs:

```
;; source code
(: g (->* [#:a Boolean] [Symbol #:c Void] Symbol))
(define (g #:a a [b 'b] #:c [c #f])
  (if a b (if c 'left 'right)))

;; ===>

;; imaginary, unrealistic rewritten code
(define (g #:a a [b 'b] #:c [c #f])
  (assert boolean? a)
  (assert symbol?  b)
  (assert void?    c)
  (if a b (if c 'left 'right)))
```

Good --- we basically know what we want.
If the Racket core language had optional and keyword functions, then we'd be
 done.

But no, Racket expands these optional/keyword
 functions into primitive [`lam`](https://docs.racket-lang.org/raco/decompile.html#(def._((lib._compiler%2Fzo-structs..rkt)._lam)))
 and [`case-lam`](https://docs.racket-lang.org/raco/decompile.html#(def._((lib._compiler%2Fzo-structs..rkt)._case-lam))).
Typed Racket type-checks this expanded code, thus Shallow Typed Racket
 (the Transient version) must rewrite the expanded code.

Let's keep digging.

From now on, ``Shallow'' or ``Shallow TR'' refers to my implementation
 of Transient for Typed Racket (TR).
We'll talk about Shallow instead of ``Transient'' in case someone later finds
 a better way to implement the Transient idea.


## False Start: Follow the Type

Beware, Shallow TR cannot rely on type annotations to decide which shape
 checks to insert.
The example function `g` above demonstrates that annotations are not
 good enough.
With our imagined rewrite, calls that leave out the optional
 `#:c` keyword lead to a shape-check failure because the variable `c` gets
 the default value `#f` instead of a void value.
Concretely, the third assert from above fails:

```
(define (g #:a a [b 'b] #:c [c #f])
  ....
  (assert void? c) ;; fails if c is #f
  ....)
```

The problem arises from subtyping.
According to the annotations, the function `g` has an external type that is
 less precise than the internal type that validates the function body::

```
;; external type T
(: g (->* [#:a Boolean] [Symbol #:c Void] Symbol))

;; internal type T2, subtype of external (T2 <: T), validates body
(: g (->* [#:a Boolean] [Symbol #:c (U #f Void)] Symbol))
```

Thanks to this external / internal distinction, the following easy
 rewrite solution fails.
Despite the failure, this first solution is a useful starting point.


### Solution 0, Step 1: Mimic the Typechecker

Shallow TR rewrites well-typed code.

```
      [(~and (let-values ([(f) fun]) . body) kw:kw-lambda^)
      ....]
      [(~and (let-values ([(f) fun]) . body) opt:opt-lambda^)
      ....]
```


### Solution 0, Step 2: Parse the Domain Type


### Solution 0, Step 3: Insert a Shape Check


 the straightforward TR pattern match solution fails ....

TODO show the TR matching code, explain what could work but fails



## On the Trail: optkey Expansion

Nevermind the TR syntax class, go deeper what is actually there
A few example expansions.
Discuss methods ... or just acknowledge


## The Shallow TR Rewrite Strategy

ok expansion above leads us to plan

interesting note: for subtyping example, arg starts with bottom type,
 occurrence typing deals with defaults


## Trouble with Methods, a Bugfix


## Discussion


<!-- 1. basics, transient idea / goal -->
<!-- 2. first attempt looking at type as guide checking in body -->
<!-- 3. oh no failed, why? subtyping -->
<!-- 4. closer look what happened ... real expansion ... new strategy -->
<!-- 5. still fail, method, now what?! -->
<!-- 6. bugfix again -->
<!-- 7. what did we learn?  -->
<!--    - xxx -->
<!--    - ps Don't let this post scare you about keyword functions. Use them without fear. -->



### subtyping

> Protecting functions turned out to be the most difficult part of the
>  rewriting---worse than rewriting classes and other macro-expanded code.
> The tricky detail is that the external type of a function can differ
>  from the types available inside the function body.
> @Figure-ref{fig:transient:opt} presents an example from the @bm{synth}
>  benchmark.
> This @codett{array-append} function accepts an optional argument @codett{k},
>  but the public type requires callers to send only one input.
> Consequently, Typed Racket internally gives @codett{k} the bottom type
>  at the top of the expanded function.
> The bottom type is widened via occurrence typing before any user code
>  appears.
> @|sShallow| Racket cooperates with this widening protocol to insert
>  correct checks.
> In an early version, though, every call to @codett{array-append}
>  raised an error.


### optkw

> The fix to Racket is especially interesting (@github-pull["racket" "racket" "3182"]).
> It came about because some @|sshallow|-typed programs failed with a strange
> error message:
>
> @nested[#:style 'inset @codett{Expected a real number, got #<unsafe-undefined>}]
>
> @|noindent|These programs were fully-typed, but somehow a run-time value
>  contradicted the type checker without causing trouble in the @|sDeep| semantics.
> Worse, this sentinel undefined value did not appear in the source code.
> The problem was due to a disagreement between core Racket and Typed Racket
>  about how to encode a method with optional arguments as a function with
>  a fixed-length argument list.
> Racket used an extra run-time check; Typed Racket thought the check was redundant.
> The fix was indeed to change Racket, which means that pre-fix versions of Typed Racket
>  are a hair's breadth from a dangerous unsoundness.
> Their saving grace is that the type optimizer does not transform methods;
>  if it did, then user code would receive unsafe-undefined values because
>  of the incorrect type assumption.

