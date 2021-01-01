    Title: The Typed Racket Optimizer vs. Transient
    Date: 2020-01-15T12:16:35
    Tags: typed racket, transient, offsite, by Ben Greenman

What type-directed optimizations does Typed Racket perform
 and do any require full types?

<!-- more -->

> This post is based on a short talk. Slides from the talk are here:
> <http://ccs.neu.edu/home/types/resources/talks/prl-offsite-2019.pdf>

Standard Typed Racket guarantees full type soundness and uses higher-order
 contracts to make sure that interactions between Typed Racket and untyped
 Racket obey the types.
These contracts can be very expensive [[JFP 2019](https://doi.org/10.1017/S0956796818000217)].
And so, the standard types are very strong but (possibly) slow.

Lately, I've been working on a [transient](https://dl.acm.org/citation.cfm?id=3009849)
 back-end for Typed Racket.
Transient Typed Racket provides a weaker guarantee --- only that typed code
 cannot get "stuck" --- via simpler run-time checks.
Early data shows that these simple checks are often faster 
 than the standard boundary checks [[ICFP 2018](https://doi.org/10.1145/3236766)],
 hence we want both options for Typed Racket programmers: slow/correct
 and fast/wrong.

The implementation of Transient needs to re-use some parts of Standard Typed
 Racket and modify others.
Typed Racket comes with three major components:

1. a static type checker,
2. a compiler from types to contracts, and
3. a type-driven optimizer [[PADL 2012](https://www2.ccs.neu.edu/racket/pubs/padl12-stff.pdf), [OOPSLA 2012](https://doi.org/10.1145/2384616.2384629)].

Transient Typed Racket can re-use all of the type checker
 and parts of the type-to-contract compiler.
The question for this post is: can Transient re-use the optimizer?


## Q. Can Transient re-use the Typed Racket optimizer?

The answer requires some thought because Standard Typed Racket and Transient
 Typed Racket preserve different amounts of type information.

- In Standard Typed Racket, if an expression **e** has type **T** and reduces
  to a value **v** (for short, **e : T -->* v**), then the result **v** definitely
  matches the full type **T**.
- In Transient Typed Racket, if **e : T -->* v** then the result **v** matches
  the toplevel "shape" of **T** but (maybe) nothing more.

The idea of a "shape" is that it corresponds to the outermost constructor of
 a type.
A shape check must be decidable, but otherwise finding the best shape for a type
 is an engineering challenge.
On one hand, deeper checks give stronger guarantees.
On the other hand, shallower checks are quicker to validate.

Here are a few shapes according to the current Transient prototype:

```
  Shape(Natural)                = Natural
  Shape(Listof String)          = Listof Any
  Shape(Symbol -> Boolean)      = Any -> Any
  Shape(Vector Void Void)       = Vector Any Any
  Shape(U Void (Listof Symbol)) = U Void (Listof Any)
```

For the current shapes, can we re-use the Typed Racket optimizer?


## Optimization Topics

Typed Racket implements 15 kinds of type-directed transformation.
Below, each gets: a short description, an example, and a verdict of "safe"
 or "unsafe" for Transient.

To be clear: some optimization topics perform many kinds of transformations,
but this post picks only one example transformation for each.

- - -
### Topic 1: apply

[apply.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/apply.rkt)
 "inlines" expressions of the form `(apply f (map g xs))` to map and fold
 in one pass over the list (`xs`).
Currently, the pass only triggers when `f` is `+` or `*`.

#### Example

```
  ;; Type Assumptions
  (: xs (Listof Integer))

  ;; --------------------------------------------------
  ;; Before Optimization
  (apply + (map abs xs))

  ;; --------------------------------------------------
  ;; After Optimization
  (let loop ((v 0)
             (lst xs))
    (if (null? lst)
      v
      (loop (+ v (abs (unsafe-car lst)))
            (unsafe-cdr lst))))
```

**Verdict**: safe, but risky.

Technically, this transformation is unsound for Transient because of how it uses `unsafe-car`.
The expansion of `(apply * (map g xs))` applies `(g (unsafe-car xs))` without
 confirming that the first element of `xs` matches its expected type.
This unsoundness is no problem, though, as long as _every_ Transient-typed function
 checks the shape of its input.
(Typed functions that flow to untyped code already need to check inputs.)


- - -
### Topic 2: box

[box.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/box.rkt)
 safely applies unsafe box operations to expressions with `Box` type.

#### Example

```
  ;; Type Assumptions
  (: b (Boxof Symbol))

  ;; --------------------------------------------------
  ;; Before Optimization
  (unbox b)

  ;; --------------------------------------------------
  ;; After Optimization
  (unsafe-unbox b)
```

**Verdict**: safe


- - -
### Topic 3: dead-code

[dead-code.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/dead-code.rkt)
 uses type information to identify code that cannot run.
Once identified, the TR optimizer makes the dead code obvious for the Racket
 bytecode compiler.
The pass deals with `if` expressions, `lambda` expressions, and `case-lambda`;
 the latter is the most interesting for Transient.

#### Example

```
  ;; Type Assumptions
  (: f (-> Symbol Symbol)

  ;; --------------------------------------------------
  ;; Before Optimization
  (define f
    (case-lambda
      ((s) s)
      ((s i)
       (for/list ((_i (in-range i))) s))))

  ;; --------------------------------------------------
  ;; After Optimization
  (define f
    (case-lambda
      ((s) s)
      ((s i)
       ; dead code, replace with no-op
       (void))))
```

**Verdict**: unsafe, can change behavior

The pass infers that some branches of a `case-lambda` can never run because
 the type says they do not exist.
In Standard Typed Racket, this inference is correct because a run-time contract
 seals off the "untyped" branches.
In Transient, though, there is no need to add a contract and therefore no
 guarantee these branches are inaccessible.
An application in untyped code can enter the dead branch;
 if it does, then adding Transient types to part of a program can change
 its result to `(void)` and thereby violate the graduality design goal [[SNAPL 2015](http://drops.dagstuhl.de/opus/volltexte/2015/5031/), [ICFP 2018](https://doi.org/10.1145/3236768)]
 --- that is, that adding types should only change behavior by introducing runtime
 type mismatches.


- - -
### Topic 4: extflonum

[extflonum.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/extflonum.rkt)
 safely applies unsafe extflonum operations to expressions with `Extflonum` type.

#### Example

```
  ;; Type Assumptions
  (: e Extflonum)

  ;; --------------------------------------------------
  ;; Before Optimization
  (extflabs e)

  ;; --------------------------------------------------
  ;; After Optimization
  (unsafe-extflabs e)
```

**Verdict**: safe


- - -
### Topic 5: fixnum

[fixnum.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/fixnum.rkt)
 safely applies unsafe fixnum operations to expressions with `Fixnum` type.

#### Example

```
  ;; Type Assumptions
  (: f Fixnum)

  ;; --------------------------------------------------
  ;; Before Optimization
  (exact->inexact f)

  ;; --------------------------------------------------
  ;; After Optimization
  (unsafe-fx->fl f)
```

**Verdict**: safe


- - -
### Topic 6: float-complex

[float-complex.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/float-complex.rkt)
 unboxes complex numbers (into one real-part variable and one imaginary-part variable)
 and rewrites operations to handle the unboxed numbers.

#### Example

```
  ;; Type Assumptions
  (: f (-> Float-Complex Float-Complex Float-Complex))

  ;; --------------------------------------------------
  ;; Before Optimization
  (define (f n0 n1)
    (+ n0 n1))

  ;; --------------------------------------------------
  ;; After Optimization
  (define (f n0 n1)
    (let* ((unboxed-real-0 (unsafe-flreal-part n0))
           (unboxed-imag-0 (unsafe-flimag-part n0))
           (unboxed-real-1 (unsafe-flreal-part n1))
           (unboxed-imag-1 (unsafe-flimag-part n1))
           (unboxed-real-2 (unsafe-fl+ (real->double-flonum unboxed-real-0)
                                       unboxed-real-1))
           (unboxed-imag-2 (unsafe-fl+ (real->double-flonum unboxed-imag-0)
                                       unboxed-imag-1)))
      (unsafe-make-flrectangular unboxed-real-2 unboxed-imag-2)))
```

**Verdict**: safe, with caution

The body of a Transient-typed function (that can flow to untyped code)
 must first check that its inputs have the correct shape.
Currently, the **float-complex** pass creates functions that apply `unsafe-flreal-part` before
 anything else; to be safe, the pass needs to make sure that Transient checks
 come first.


- - -
### Topic 7: float

[float.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/float.rkt)
 safely applies unsafe flonum operations to expressions with `Flonum` type
 and also transforms some `random` calls to use `unsafe-flrandom`.


#### Example

```
  ;; --------------------------------------------------
  ;; Before Optimization
  (random)

  ;; --------------------------------------------------
  ;; After Optimization
  (unsafe-flrandom (current-pseudo-random-generator))
```

**Verdict**: safe, but a close call

Accessing a parameter, as in `(current-pseudo-random-generator)`, is an
 elimination form that may require a shape check.
This particular parameter, however, is protected by a contract that enforces
 the precondition of `unsafe-flrandom`.


- - -
### Topic 8: list

[list.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/list.rkt)
 safely applies unsafe list operations to list expressions.

#### Example

```
  ;; Type Assumptions
  (: lst (List Symbol Symbol))

  ;; --------------------------------------------------
  ;; Before Optimization
  (list-ref lst 0)

  ;; --------------------------------------------------
  ;; After Optimization
  (unsafe-list-ref lst 0)
```

**Verdict**: safe, with strong-enough shape checks

The shape check for a `(Listof T)` must check for proper lists (via `list?`);
 note that the cost of this check depends on the size of incoming values.
The shape check for a `(List T ...)` type must validate the length of incoming
 values.


- - -
### Topic 9: number

[number.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/number.rkt)
 performs simple transformations on `Real`-valued expressions.

#### Example

```
  ;; Type Assumptions
  (: r Real)

  ;; --------------------------------------------------
  ;; Before Optimization
  (+ r)

  ;; --------------------------------------------------
  ;; After Optimization
  r
```

**Verdict**: safe


- - -
### Topic 10: pair

[pair.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/pair.rkt)
 safely applies pair-access operations to (possibly-nested) pairs.

#### Example

```
  ;; Type Assumptions
  (: p (Pairof (Pairof Symbol Void) String))

  ;; --------------------------------------------------
  ;; Before Optimization
  (cdar p)

  ;; --------------------------------------------------
  ;; After Optimization
  (unsafe-cdr (unsafe-car p))
```

**Verdict**: unsafe

Transient guarantees the first level of a type, but nothing more.
Concretely, `Shape(Pairof (Pairof Symbol Void) String) = Pairof Any Any`
 and so the `unsafe-cdr` above is not safe.


- - -
### Topic 11: sequence

[sequence.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/sequence.rkt)
 safely applies unsafe sequence operations to expressions with `(Sequenceof T)` type.

#### Example

```
  ;; Type Assumptions
  (: s String)

  ;; --------------------------------------------------
  ;; Before Optimization
  (for ((c s))
    (void))

  ;; --------------------------------------------------
  ;; After Optimization (simplified)
  (for ((c (in-string s)))
    (void))
```

**Verdict**: safe, with strong enough shape checks (see **list** and **vector**)


- - -
### Topic 12: string

[string.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/string.rkt)
 safely applies unsafe string operations to expressions with `String` type.
(Note that `unsafe-string-ref` is only safe when the result is sure to be
 a Latin-1 character.)

#### Example

```
  ;; Type Assumptions
  (: b Bytes)

  ;; --------------------------------------------------
  ;; Before Optimization
  (bytes-length b)

  ;; --------------------------------------------------
  ;; After Optimization
  (unsafe-bytes-length b)
```

**Verdict**: safe


- - -
### Topic 13: struct

[struct.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/struct.rkt)
 safely applies unsafe struct operations to struct expressions, using
 Typed Racket's [internal registry of struct info](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/types/struct-table.rkt).

#### Example

```
  ;; Type Assumptions
  (struct open-interval ([lo : Real] [hi : Real]))
  (: ivl open-interval)

  ;; --------------------------------------------------
  ;; Before Optimization
  (open-interval-lo ivl)

  ;; --------------------------------------------------
  ;; After Optimization
  (unsafe-struct-ref ivl 0)
```

**Verdict**: safe


- - -
### Topic 14: unboxed-let

[unboxed-let.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/unboxed-let.rkt)
 cooperates with the `float-complex` pass by transforming the binding-site
 of some complex numbers.
This pass may change a `let`-expression into a `let-values` that expects
 a real-part and imag-part, and may change a function to expect twice as many
 arguments --- provided the optimizer can find _all_ calls to the function.

#### Example

```
  ;; Type Assumptions
  (: k Float-Complex)

  ;; --------------------------------------------------
  ;; Before Optimization
  (let ((f (lambda ((n : Float-Complex)) (+ n n))))
    (f k))

  ;; --------------------------------------------------
  ;; After Optimization
  (let ((f (lambda (real-part-n imag-part-n) ....)))
    (f (unsafe-flreal-part k) (unsafe-flimag-part k)))
```

**Verdict**: safe, thanks to the (conservative) escape analysis


- - -
### Topic 15: vector

[vector.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/vector.rkt)
 safely applies vector operations to vector expressions.

#### Example

```
  ;; Type Assumptions
  (: v (Vector (Listof Symbol) String))
  (: lst (Listof Symbol))

  ;; --------------------------------------------------
  ;; Before Optimization
  (vector-set! v lst 0)

  ;; --------------------------------------------------
  ;; After Optimization
  (unsafe-vector-set! v lst 0)
```

**Verdict**: safe, with strong-enough shape checks

The check for `(Vector T ...)` must check the length of incoming values.

- - -

## Summary

The Typed Racket optimizer implements 15 kinds of transformations.
Two are definitely unsafe for Transient as-is (**dead-code**, **pair**).
One must take care when rewriting a Transient function (**float-complex**).
One may limit our ability to reduce the number of run-time checks in a program (**apply**).
Two others require transient checks whose cost depends on the size of the input values (**list**, **sequence**).

There may be other issues that I missed while reading the optimizer code.
If so, I'll try to remember to update this post.

