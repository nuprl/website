    Title: Transient for Optional and Keyword Functions
    Date: 2020-11-12T10:15:16
    Tags: typed racket, transient, by Ben Greenman

Join me for a short adventure into the depths of optional and/or keyword
 functions.

<!-- more -->

- - -

Transient, or rather _the Transient semantics for a mixed-typed language_,
 is one way to let statically-typed code safely interact with untyped code.
You can read all about it in
 [Michael Vitousek's 2019 dissertation](http://hdl.handle.net/2022/23172)
 and see how it compares to other mixed-typed semantics
 [here](http://prl.ccs.neu.edu/blog/2018/10/06/a-spectrum-of-type-soundness-and-performance/).
The idea is to rewrite typed code with assertions called _shape checks_;
 for example:

- if a typed module imports an untyped library, then every value that crosses
  the module boundary gets a shape check;
- if typed code reads from an array, then every element that comes out must
  satisfy a shape check; and
- if a typed function escapes to untyped code, then the function must use
  a shape check to validate every input that it receives.

Our focus today is on rewriting typed functions.
Suppose we know how to turn a type `T` into a shape check, and we know every
 function that needs to check its inputs.
The question is how to actually do the check in Racket.

In theory, rewriting is no problem.
A function takes exactly one argument and needs exactly one shape check in
 the body:

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
These are still fairly easy to handle in theory:

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
The question is how to get there because Racket compiles optional and keyword
 functions into primitive [`lam`](https://docs.racket-lang.org/raco/decompile.html#(def._((lib._compiler%2Fzo-structs..rkt)._lam)))
 and [`case-lam`](https://docs.racket-lang.org/raco/decompile.html#(def._((lib._compiler%2Fzo-structs..rkt)._case-lam)))
 forms.


## False Start: Follow the Type

Beware, a Transient cannot rely on type annotations to decide which shape
 checks to insert.
The example function `g` from before demonstrates that annotations are not
 good enough --- with our imagined rewrite, calls that leave out the optional
 `#:c` keyword lead to a shape-check failure because the variable `c` gets
 the default value `#f` instead of a void value.

The problem arises from subtyping.
According to the annotations, the function `g` has a type that is less precise
 than its true ``internal'' type:

```
;; external type
(: g (->* [#:a Boolean] [Symbol #:c Void] Symbol))

;; internal type, subtype of external, used for checking body
(: g (->* [#:a Boolean] [Symbol #:c (U #f Void)] Symbol))
```

Thanks to this external / internal distinction,
 the straightforward TR pattern match solution fails ....


A first complication shows up in the theory.
If the type system allows subtyping, then 



But the question is how to implement what we want

In Typed Racket, though, a function may accept optional arguments,
 keyword arguments, and optional keyword arguments.
These 




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

