    Title: The Typed Racket Optimizer vs. Transient
    Date: 2019-11-17T21:32:56
    Tags: typed racket, offsite, by Ben Greenman

What type-directed optimizations does Typed Racket perform
 and do any require full types?

<!-- more -->

> This post is based on a short talk. Slides from the talk are here:
> <http://ccs.neu.edu/home/types/resources/talks/prl-offsite-2019.pdf>

<!-- TODO edit -->
Lately, I've been working on a [transient](https://dl.acm.org/citation.cfm?id=3009849)
 back-end for Typed Racket.
The goal of this project is to trade guarantees for performance.
Standard Typed Racket guarantees full type soundness, but uses higher-order
 contracts to make sure that interactions between Typed Racket and untyped
 Racket obey the types.
These contracts can be very expensive [[JFP 2019](https://doi.org/10.1017/S0956796818000217)].
Transient Typed Racket provides a weaker guarantee --- only that typed code
 cannot get "stuck" --- via simpler run-time checks.
Early data shows that these simple checks are sometimes better and sometimes
 worse than the standard boundary checks [[ICFP 2018](https://doi.org/10.1145/3236766)],
 hence we want both options for Typed Racket programmers.

Typed Racket comes with three major components:

1. a static type checker,
2. a compiler from types to contracts, and
3. a type-driven optimizer [[PADL 2012](https://www2.ccs.neu.edu/racket/pubs/padl12-stff.pdf), [OOPSLA 2012](https://doi.org/10.1145/2384616.2384629)].

Transient Typed Racket can re-use all of the type checker
 and parts of the type-to-contract compiler.
The question for this post is: can Transient re-use the optimizer?

The answer requires some thought because Standard Typed Racket and Transient
 Typed Racket preserve different amounts of type information.

- In Standard Typed Racket, if an expression `e` has type `T` and reduces
  to a value `v` (for short, `e : T -->* v`), then the result `v` definitely
  matches the full type `T`.
- In Transient Typed Racket, if `e : T -->* v` then the result `v` matches
  the toplevel "shape" of `T` but (maybe) nothing more.

A "shape" roughly corresponds to the outermost constructor of a type.
The only requirement is that every shape check is decidable.
Finding the best shapes is an engineering challenge.
On one hand, deeper checks give stronger guarantees.
On the other hand, shallower checks give better performance.

Here are a few shapes according to the current Transient prototype.
These shapes try to balance between guarantees and performance;
 for example, the shape of a function describes its arity.

```
  Shape(Natural)                = Natural
  Shape(Listof String)          = Listof Any
  Shape(Symbol -> Boolean)      = Any -> Any
  Shape(Vector Void Void)       = Vector Any Any
  Shape(U Void (Listof Symbol)) = U Void (Listof Any)
```

For the current shapes, can we re-use the Typed Racket optimizer?


<!-- TODO do we know enough about Transient to explain these? -->
<!-- x functions defend selves -->
<!-- x list? walks list -->

## Optimizations

Typed Racket implements 15 kinds of type-directed transformation.
Below, each gets: a short description, an example, and a verdict of "safe"
 or "unsafe" for Transient.

- - -
### apply

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
### box

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
### dead-code

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
 --- that is, that adding types can only change behavior by introducing runtime
 type mismatches.


- - -
### extflonum

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
### fixnum

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
### float-complex

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

The body of a Transient-typed function must first check that its inputs have
 the correct shape.
Currently, this pass creates functions that apply `unsafe-flreal-part` before
 anything else; to be safe, the pass needs to make sure that Transient checks
 come first.


- - -
### float

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
This particular parameter, however, is protected by a contract --- so there
 is no need for Transient-typed code to check.


- - -
### list

[list.rkt](https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/list.rkt)
 safely applies unsafe list operations to `(List T ...)` and `(Listof T)` expressions.

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

**Verdict**: safe


- - -
### number

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
### pair

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
 and so the `unsafe-cdr` above is not safe to use.


- - -
### sequence

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

**Verdict**: safe


- - -
### string

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
### struct

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
### unboxed-let

[unboxed-let.rkt](Keep it brie://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/optimizer/unboxed-let.rkt)
 works together with the `float-complex` pass by unboxing the binding-site
 of some complex numbers.
This pass may change a `let`-expression into a `let-values` that expects
 a real-part and imag-part, and may change a function to expect twice as many
 arguments --- provided the optimizer can find _all_ calls to the function.

#### Example

```
  ;; --------------------------------------------------
  ;; Before Optimization
  (let ((f (lambda ((n : Float-Complex)) (+ n n))))
    (void))

  ;; --------------------------------------------------
  ;; After Optimization
  (let ((f (lambda (real-part-n imag-part-n) ....)))
    (void))
```

**Verdict**: safe, thanks to the escape analysis


- - -
### vector

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

**Verdict**: safe

- - -

## Summary

The Typed Racket optimizer implements 15 kinds of transformations.
Two are definitely unsafe for Transient as-is.
One may limit our ability to reduce the number of run-time checks in a program.
Two others require transient checks whose cost depends on the size of the input values.

There may be other issues that I missed while reading the optimizer code.
If so, I'll try to remember to update this post.

