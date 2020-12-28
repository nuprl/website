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
 and focus on a (weak) form of type soundness.
To enforce soundness, Transient rewrites every expression in typed code
 with assertions called _shape checks_; for example:

- if a typed module imports an untyped library, then every value that crosses
  the module boundary gets a shape check;
- if typed code reads from an array, then every element that comes out of the
  array must satisfy a shape check; and
- if a typed function escapes to untyped code, then the function must use
  a shape check to validate every input that it receives.

Our goal today is to understand the shape checks for functions.
Suppose we know how to turn a type **T** into a shape check, and we have a
 function with type **T** that needs to check its inputs.
The question is how to actually do the check in Racket v7.9.

In your standard theory, rewriting is no problem.
A (simplified, model) function takes exactly one argument and needs exactly one
 shape check in the body; if **T = (-> Symbol Boolean)** then we need
 to check the shape **symbol?** of the domain type **Symbol**:

```
;; source code
(: f (-> Symbol Boolean))
(define (f sym)
  (eq? sym 'hola))

;; ===>

;; imaginary (but realistic) rewritten code
(define (f sym)
  (assert sym symbol?)
  (eq? sym 'hola))
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
  (assert a boolean?)
  (assert b symbol?)
  (assert c void?)
  (if a b (if c 'left 'right)))
```

Good --- we basically know what we want.
If the Racket core language had optional and keyword functions, then we'd be
 done.

But no, Racket expands these optional/keyword
 functions into primitive [**lambda**](https://docs.racket-lang.org/raco/decompile.html#(def._((lib._compiler%2Fzo-structs..rkt)._lam)))
 and [**case-lambda**](https://docs.racket-lang.org/raco/decompile.html#(def._((lib._compiler%2Fzo-structs..rkt)._case-lam)))
 forms.
Typed Racket type-checks this expanded code, thus Shallow Typed Racket
 (the Transient version) must rewrite the expanded code.

Let's keep digging.

From now on, "Shallow" or "Shallow TR" refers to my implementation
 of Transient for Typed Racket (TR).
We'll talk about Shallow instead of "Transient" in case future work reveals a
 better way to implement the Transient idea.


## False Start: Follow the Type

Beware --- Shallow TR cannot rely on type annotations to decide which shape
 checks to insert.
The example function **g** above demonstrates that annotations are not
 good enough.
With our imagined rewrite, calls that leave out the optional
 **#:c** keyword lead to a shape-check failure because the variable **c** gets
 the default value **#f** instead of a void value.
Concretely, the third assert from above fails:

```
(define (g #:a a [b 'b] #:c [c #f])
  ....
  (assert c void?) ;; fails if c is the #f default value
  ....)
```

The problem arises from subtyping.
According to the annotations, the function **g** has an external type that is
 less precise than the internal type that validates the function body:

```
;; external type T
(: g (->* [#:a Boolean] [Symbol #:c Void] Symbol))

;; internal type T2, subtype of external (T2 <: T), validates body
(: g (->* [#:a Boolean] [Symbol #:c (U #f Void)] Symbol))
```

Thanks to this external / internal distinction, the following easy
 rewrite idea, _Solution 0_, fails.
Despite the failure, this first solution is a useful starting point for
 a success.


#### Solution 0, Step 1: Mimic the Typechecker

Shallow TR uses the same type checker as classic _Deep_ TR.
If type checking succeeds, then Shallow must insert shape checks.
Otherwise, compilation stops with a type error.

Thanks to its wholesale reuse of the type checker, Shallow TR can use
 syntax patterns from the type checker to navigate expanded Racket code.
For optional and keyword functions in particular, Shallow can get started
 by looking at how the type checker recognizes these forms in expanded code.

Here are two syntax patterns for keyword functions and optional functions
 in the Deep TR type checker ([typecheck/tc-expr-unit.rkt](https://github.com/racket/typed-racket/blob/325f621716966b95a68af700624bafa21ac66e14/typed-racket-lib/typed-racket/typecheck/tc-expr-unit.rkt#L274-L295)).
The omitted code (**....**) does actual type checking:

```
(define (tc-expr/check/internal form expected-type)
  ....
  (syntax-parse form
    #:literal-sets (kernel-literals tc-expr-literals)
    ....
    [(~and (let-values ([(f) fun]) . body) kw:kw-lambda^)
    ....]
    [(~and (let-values ([(f) fun]) . body) opt:opt-lambda^)
    ....]
```

Ok!
Those two patterns say a lot about the expansion of optional and keyword
 functions:

1. Both forms expand to a **let-values** that binds one function **fun**.
2. TR uses the syntax classes **kw-lambda^** and **opt-lambda^** to
   tell these particular **let-values** apart from others.

Shallow TR can use exactly these patterns to 
 recognize optional/keyword functions.


#### Solution 0, Step 2: Parse the Domain Type

Once the Shallow TR rewriter has found an optional/keyword function,
 the next step is to find the function's type and figure out the right
 shape check.
For an optional function, the rewriter has an expression that
 matches the following pattern:

```
    [(~and (let-values ([(f) fun]) . body) opt:opt-lambda^)
    ....]
```

First, we need a type.
The type checker decorates (almost) every expression with a type as a syntax
 property.
(Unreachable code may not have a type.)
The [**type-of**](https://github.com/racket/typed-racket/blob/325f621716966b95a68af700624bafa21ac66e14/typed-racket-lib/typed-racket/types/type-table.rkt#L80)
 function gets the type decoration from an expression.
A little experimentation shows that the function
 part of our expression, **fun**, has a type.
Great.

Second, we need to parse the domain from the function type.
This is easier said than done.
Fortunately, our final solution does not need the parsing step so I will
 list the challenges and move on:

- The type of **fun** could be a straightforward [**Fun type**](https://github.com/racket/typed-racket/blob/325f621716966b95a68af700624bafa21ac66e14/typed-racket-lib/typed-racket/rep/type-rep.rkt#L693),
  but it could also be a: [**DepFun type**](https://github.com/racket/typed-racket/blob/325f621716966b95a68af700624bafa21ac66e14/typed-racket-lib/typed-racket/rep/type-rep.rkt#L701),
  or [**Poly type**](https://github.com/racket/typed-racket/blob/325f621716966b95a68af700624bafa21ac66e14/typed-racket-lib/typed-racket/rep/type-rep.rkt#L521),
  or [**PolyDots type**](https://github.com/racket/typed-racket/blob/325f621716966b95a68af700624bafa21ac66e14/typed-racket-lib/typed-racket/rep/type-rep.rkt#L531),
  or even a [**Union type**](https://github.com/racket/typed-racket/blob/325f621716966b95a68af700624bafa21ac66e14/typed-racket-lib/typed-racket/rep/type-rep.rkt#L900).
- Each part of the domain type corresponds to one parameter of the **fun** expression.
  Matching the parameter names to types is not straightforward; for example,
   do the mandatory parameters come first in **fun**, or the mandatory keywords?


#### Solution 0, Step 3: Insert a Shape Check

Once we have the target **fun** expression and a map from parameter names
 to types, the final step of our tentative solution is easy.
First, convert the types to shape predicates.
Second, parse **fun** to separate the parameters from the body.
Third, insert a block of shape checks to the top of the body.
All together, rewriting **fun** goes something like this:

```
(syntax-parse fun
  [(#%plain-lambda formals . body)
   #:with (shape-check ...)
          (make-shape-checks #'formals (type-of fun))
   #'(#%plain-lambda formals (#%plain-app void shape-check ...) . body)])
```

The rewritten function executes shape checks immediately, and then proceeds
 with the **body** after validating each actual parameter.


## On the Trail: optkey Expansion

Our _Solution 0_ fails because the type of the **fun** expression
 that it gets from the type-checked code is an external type.
In terms of the **g** function from above, _Solution 0_ uses the type
 **Void** instead of the internal type **(U Void #f)** to check the **c** parameter.
To get internal types, we need to look closer at **fun** and the rest of
 the optional/keyword expansion.

Let's study three example functions and their expanded forms.
The expansions reveal a common pattern that motivates a new Shallow TR strategy.

If you want to expand these examples yourself, hide them from the Racket
 toplevel as follows.
For each example function **X** create a module **test.rkt** like this:

```
#lang racket/base

(define _ignore
  (let ()
    X
    (void)))
```

Invoke the expander with `raco expand test.rkt > test.rkt.txt` and explore
 the generated **.txt** file.


### Example 1: mandatory keyword

The source is a function with one mandatory positional argument and one optional positional argument.

```
(lambda (x [y 0])
  (+ x y))
```

Expansion generates a **case-lambda** that accepts one or two arguments.
The one-argument case supplies a default value for the missing parameter.
Both cases call a generated function **F** that expects two arguments,
 resolves defaults in a different way,
 and executes the function body.

```
(let-values (((F)
              (lambda (x2 y1)
                (let-values (((x) x2))
                  (let-values (((y) (if '#f '0 y1)))
                    (let-values () (#%app + x y)))))))
  (case-lambda
   ((x) (#%app F x '0))
   ((x y1) (#%app F x y1))))
```

Note: the expression **(if '#f '0 y1)** in the generated **F** function
 is equal to **y1** alone.
In general, the **if** is for default expressions.
([Unlike Python](https://pythonconquerstheuniverse.wordpress.com/2012/02/15/mutable-default-arguments/),
 Racket evaluates a mutable default once for each function call.)
When the default is an immediate value, as this example illustrates,
 the expander generates a **#f** test.
A general-purpose optimizer can remove this test before the code runs.


### Example 2:

The source is a function with one mandatory positional argument and one mandatory keyword argument:

```
(lambda (x #:y y)
  (+ x y))
```

Expansion generates several functions:

- **F0** expects a plain list of arguments and executes the source function's body
- **F1** expects a list of keywords, a list of arguments, and a final argument.
  The purpose of **F1** is to organize a call to **F0**.
- **lifted/2** is the constructor for a generated struct type.
  Other functions help the struct call **F1**.
  Nevermind the details; I don't fully understand them either.

The important piece for Shallow TR is the **F0** function
 because the goal of rewriting is to protect the original
 function body against untyped inputs.

```
(let-values (((F0)
              (lambda (y1 x3)
                (let-values (((x) x3))
                  (let-values (((y) y1))
                    (let-values () (#%app + x y)))))))
  (let-values (((F1)
                (lambda (given-kws given-args x3)
                  (let-values (((y1) (#%app car given-args)))
                    (#%app F0 y1 x3)))))
    (#%app
     lifted/2
     (lambda (given-kws given-argc)
       (if (#%app = given-argc '3)
         (let-values (((l2571) given-kws))
           (if (#%app pair? l2571)
             (if (#%app eq? (#%app car l2571) '#:y)
               (#%app null? (#%app cdr l2571))
               '#f)
             '#f))
         '#f))
     (case-lambda
      ((given-kws given-args x)
       (#%app F1 given-kws given-args x)))
     '(#:y)
     '(#:y))))
```


### Example 3:

The source is a function with one mandatory positional argument and one optional keyword argument:

```
(lambda (x #:y [y 0])
  (+ x y))
```

Expansion again generates several functions:

- **F0** expects a plain list of arguments, resolves the optional default,
  and executes the source function's body
- **F1** calls **F0**
- At the bottom, there are two **case-lambda** functions that call
  **F1**

Again, the **F0** function is the focal point for Shallow TR rewriting.

```
(let-values (((F0)
              (lambda (y1 x3)
                (let-values (((x) x3))
                  (let-values (((y) (if '#f '0 y1)))
                    (let-values () (#%app + x y)))))))
  (let-values (((F1)
                (lambda (given-kws given-args x3)
                  (let-values (((y2) (#%app pair? given-kws)))
                    (let-values (((y1)
                                  (if y2 (#%app car given-args) '0)))
                      (#%app F0 y1 x3))))))
    (#%app
     make-optional-keyword-procedure
     (lambda (given-kws given-argc)
       (if (#%app = given-argc '3)
         (let-values (((l1571) given-kws))
           (let-values (((l1571)
                         (if (#%app null? l1571)
                           l1571
                           (if (#%app eq? (#%app car l1571) '#:y)
                             (#%app cdr l1571)
                             l1571))))
             (#%app null? l1571)))
         '#f))
     (case-lambda
      ((given-kws given-args x)
       (#%app F1 given-kws given-args x)))
     null
     '(#:y)
     (case-lambda
      ((x) (#%app F1 null null x))))))
```


## Solution: The Shallow TR Rewrite Strategy

All three examples show a common pattern among the expansions of optional
 and keyword functions.
Each function expands to a **let-values** form:

```
(let-values (((f) fun)) . body)
```

Furthermore, the generated **fun** is a lambda that first resolves optional
 arguments and then executes the body of the original function.
Here is the **fun** from _Example 3_ again;
 it has formal parameters for the keyword arg. and the mandatory arg.,
 and one **let-values** to resolve each parameter:

```
  (lambda (y1 x3)
    (let-values (((x) x3))
      (let-values (((y) (if '#f '0 y1)))
        (let-values () (#%app + x y)))))
```

Another experiment with **type-of** shows that the right-hand side of
 each **let-values** has an internal type annotation.
Excellent!
Both **(type-of x3)**
 and **(type-of (if '#f '0 y1))** are the right types for shape checks.
Shallow TR can:

- inspect the **let-values** one-by-one;
- convert the type of each right-hand expression to a shape predicate; and
- rewrite each right-hand **expr** into **(assert expr shape?)**.

This should work!
In fact, we can do slightly better:

- when the right-hand expression is a conditional **(if test default-expr supplied-arg)**
- then Shallow only needs to check the supplied arg: **(if test default-expr (assert supplied-arg shape?))**

Note: Shallow needs to rewrite the default expression, but it can trust its
 final shape because of (Transient) type soundness.


## A Problem with Methods and a Bugfix

Currently, Shallow TR rewrites optional and keyword functions using
 the **let-values** plan described above.
Each formal parameter has one **let-values** binding,
 and the type on each bound expression defines the shape check.

Last May, though, this rewriting caused new failures in methods
 with optional arguments.
The failure was due to a mismatch between Typed Racket
 and the Racket class expander.
Since then, we [fixed the class expander](https://github.com/racket/racket/pull/3182).

First, here is a class with one method that runs correctly.
The method **f** accepts an optional positional argument **x**; the default
 value of **x** is the current value of the field **my-num** (fields are mutable):

```
(define c0%
  (class object%
    (super-new)
    (field (my-num 2))
    (define/public (f [x my-num])
      (+ x x))))
```

Second, here is a similar method that fails.
This time, the default is an immediate value **2**:

```
(define c1%
  (class object%
    (super-new)
    (define/public (f [x 2])
      (+ x x))))
```

Running a call **(send o1 f)** used to raise a shape-check failure about
 a strange value:

> shape error: Expected a real number, got `#<unsafe-undefined>`

What is going on?

It turns out, the undefined value comes from the expander.
Here is an optional function with a default expression:

```
(lambda (x [y z])
  (+ x y))
```

Expansion generates a function **F0** that checks for the undefined value,
 and an outer **case-lambda** that supplies undefined when the default
 is needed:

```
(let-values (((F0)
              (lambda (x2 y1)
                (let-values (((x) x2))
                  (let-values (((y)
                                (if (#%app eq? y1 unsafe-undefined)
                                  z
                                  y1)))
                    (let-values () (#%app + x y)))))))
  (case-lambda
   ((x) (#%app F0 x unsafe-undefined))
   ((x y1) (#%app F0 x y1))))
```

That's the normal way that **unsafe-undefined** shows up:
 the [expander for optional/keyword functions](https://github.com/racket/racket/blob/c0ff11e27bd28e070c20b7a9b0f7365f8f2b665a/racket/collects/racket/private/kw.rkt)
 looks for default expressions
 vs. default values and uses the undefined value for expressions.

Three other facts conspired to make the problem with optional methods:

1. Typed Racket also looks for default expressions vs. default values
   (search for **immediate-default** [here](https://github.com/racket/typed-racket/blob/325f621716966b95a68af700624bafa21ac66e14/typed-racket-lib/typed-racket/base-env/annotate-classes.rkt)).
   When an optional parameter has a default expression, Typed Racket
    widens its internal type to accept the **unsafe-undefined** value
    (search for **-Unsafe-Undefined**
     [here (kw)](https://github.com/racket/typed-racket/blob/325f621716966b95a68af700624bafa21ac66e14/typed-racket-lib/typed-racket/types/kw-types.rkt)
     and
     [here (opt)](https://github.com/racket/typed-racket/blob/325f621716966b95a68af700624bafa21ac66e14/typed-racket-lib/typed-racket/typecheck/tc-lambda-unit.rkt)).
2. The class expander does some pre-processing on optional methods and
   inadvertantly turned every default value into a default expression.
3. Shallow TR pushes default expression checks **(if test default-expr supplied-arg)**
   to the **supplied-arg** instead of wrapping the whole **if** form.

In the end, Typed Racket saw a default value and inferred an overly-precise
 type.
The type would be correct but for the class expander.
As-is, the type was unsound---but harmless because the false assumption
 was guarded by an **if** test for **unsafe-undefined**.
Running Shallow TR revealed the unsoundness with its eager shape check.

Again, the resolution was to fix the class expander ([racket/racket #3182](https://github.com/racket/racket/pull/3182)).
Both Typed Racket and Shallow TR stayed the same.
The change removes an unnecessary run-time check from expanded optional methods.


## Lessons

1. Optional and keyword functions are not core forms in Racket.
   They expand to a combination of simple functions.
2. Digging into the expansion is sometimes necessary.
   There are at least three places that do so---the class expander, TR, and Shallow TR---and unfortunately they all need to cooperate.
3. The development of Shallow TR helped find several latent bugs in TR, Racket, and other libraries.
   Figure 57 of [my dissertation](https://ccs.neu.edu/home/types/publications/publications.html#g-dissertation-2020)
    lists them all.


