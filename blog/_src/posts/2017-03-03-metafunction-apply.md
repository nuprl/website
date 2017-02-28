    Title: PLT Redex: mf-apply
    Date: 2017-03-03T08:54:20
    Tags: PLT Redex, package, lang-extension, by Ben Greenman

The `mf-apply` keyword is for checked metafunction application in PLT Redex.
In other words, `(mf-apply f x)` is just like `(f x)`, but errors if `f` is
 not a previously-defined metafunction.

Also, consider applying to attend _The Racket School of Semantics and Languages_
in Salt Lake City this summer:
[http://summer-school.racket-lang.org/2017/](http://summer-school.racket-lang.org/2017/)



<!-- more -->

## Metafunctions vs. List Patterns

Have you used PLT Redex? Good!
Maybe this has happened to you:

```racket
#lang racket
(require redex)

;; -----------------------------------------------------------------------------
;; 1. You define a language
(define-language STLC
  [V ::= integer boolean C]
  [C ::= (closure Λ ρ)]
  [Λ ::= (λ (x : τ) M)]
  [M ::= (M M) V Λ x]
  [τ ::= Int Bool (τ → τ)]
  [ρ ::= ((x V) ...)]
  [Γ ::= ((x τ) ...)]
  [x ::= variable-not-otherwise-mentioned]
  #:binding-forms (λ (x : τ) M #:refers-to x))


;; -----------------------------------------------------------------------------
;; 2. You define a few metafunctions
(define-metafunction STLC
  closure->lam : C -> Λ
  [(closure->lam (closure Λ ρ))
   Λ])

(define-metafunction STLC
  closure->env : C -> ρ
  [(closure->env (closure Λ ρ))
   ρ])


;; -----------------------------------------------------------------------------
;; 3. You try defining a judgment form . . .
(define-judgment-form STLC
  #:mode (free-variables I O)
  #:contract (free-variables M (x ...))
  [
   --- FVS-Var
   (free-variables x (x))]
  [
   (free-variables M_0 (x_0 ...))
   (free-variables M_1 (x_1 ...))
   --- FVS-App
   (free-variables (M_0 M_1) (x_0 ... x_1 ...))]
  [
   (where (λ (x_0 τ) M) Λ)
   (free-variables M (x_1 ...))
   (where (x_2 ...) ,(set-remove (term (x_1 ...)) (term x_0)))
   --- FVS-Λ
   (free-variables Λ (x_2 ...))]
  [
   --- FVS-Integer
   (free-variables integer_0 ())]
  [
   --- FVS-Boolean
   (free-variables boolean_0 ())]
  [
   (where Λ (closure->lam C))
   (free-variables Λ (x_0 ...))
   (where ((x_1 τ_1) ...) (closure-env C))
   (where (x_2 ...) ,(set-subtract (term (x_0 ...)) (term (x_1 ...))))
   --- FVS-Closure
   (free-variables C (x_2 ...))])


;; -----------------------------------------------------------------------------
;; 4. You test the judgment, and it mysteriously fails
(judgment-holds
  (free-variables (closure (λ (x : Int) x) ())
                  ()))
;; ==> #f
```

**WHAT HAPPENED??!**

The problem is this line in the `FVS-Closure` rule:

```racket
   ....
   (where ((x_1 τ_1) ...) (closure-env C))
   ....
```

which checks that the list `(closure-env C)` (whose first element is the
 symbol `closure-env` and second element is the symbol `C`) matches the pattern
 `((x_1 τ_1) ...)`.

Right.

Of course you meant to apply the metafunction `closure->env` but made a typo.
And since the syntax for metafunction application is the same as the syntax
 for building a list, Redex doesn't report an error.

We can fix this code with the new
[`mf-apply`](https://www.cs.utah.edu/plt/snapshots/current/doc/redex/The_Redex_Reference.html#%28form._%28%28lib._redex%2Freduction-semantics..rkt%29._mf-apply%29%29)
keyword (available on [GitHub](https://github.com/racket/racket) or in a
[snapshot build](https://www.cs.utah.edu/plt/snapshots/)):

```racket
   ....
   (where ((x_1 τ_1) ...) (mf-apply closure-env C))
   ....
```

Running `raco make` now gives a compile-time error.

```
  term: expected a previously defined metafunction
    at: closure-env
    in: (mf-apply closure-env C)
```


### But I still need to type `mf-apply` correctly!

Leif Andersen says:

> I should point out that this has the issue of you still need to type
> `mf-apply` correctly. ;)

That is, if you accidentally write:

```racket
   ....
   (where ((x_1 τ_1) ...) (mf-applu closure-env C))
   ....
```

Then the code compiles, thinking you intend to match a list of three elements
 against the pattern.

Never fear, there are at least two solutions.


#### Solution 1: rename `mf-apply`

A simple fix is to rename the `mf-apply` keyword to something shorter (and
harder to mis-type):

```racket
#lang racket
(require redex
         (rename-in redex
           [mf-apply MF]))
```


#### Solution 2: the `mf-apply` lang extension

A fancier solution is to install the `mf-apply` meta-language.

```
  $ raco pkg install mf-apply
```

This language updates the [_readtable_](http://docs.racket-lang.org/reference/readtables.html#%28tech._readtable%29)
 to interpret S-expressions that begin with `#{`:

```racket
#lang mf-apply racket
(require redex)

(term #{f x ...})
```

as a metafunction application:

```racket
#lang mf-apply racket
(require redex)

(term (mf-apply f x ...))
```

You the programmer only needs to write the `#{....}` syntax.

Source code is on GitHub:

- [https://github.com/bennn/mf-apply](https://github.com/bennn/mf-apply)

(It's the simplest lang-extension I know of)


## What is PLT Redex?

PLT Redex is a library for semantics engineering.
One of my favorite Redex features is it implements capture-avoiding substitution
 and α-equivalence for any language with a `#:binding-forms` specification
 (such as STLC, above).

You can read more:

- in the "Amb" tutorial: [http://docs.racket-lang.org/redex/tutorial.html](http://docs.racket-lang.org/redex/tutorial.html)
- in the "Long Tutorial": [http://docs.racket-lang.org/redex/redex2015.html](http://docs.racket-lang.org/redex/redex2015.html)
- in the Redex reference manual: [http://docs.racket-lang.org/redex/The_Redex_Reference.html](http://docs.racket-lang.org/redex/The_Redex_Reference.html)
- on the PLT Redex website: [https://redex.racket-lang.org/](https://redex.racket-lang.org/)
- on GitHub: [https://github.com/racket/redex](https://github.com/racket/redex)

And if you act now, you can become a _Redexan_ between July 10 and July 14
 at the summer school in Salt Lake City, Utah:

- [http://summer-school.racket-lang.org/2017/](http://summer-school.racket-lang.org/2017/)


