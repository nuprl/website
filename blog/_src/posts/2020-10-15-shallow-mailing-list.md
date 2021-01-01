    Title: Transient Answers Old Questions
    Date: 2020-10-15T13:32:12
    Tags: typed racket, transient, by Ben Greenman

Several old questions from the Typed Racket mailing list have new and simple
answers under a "transient" Typed Racket.

<!-- more -->

- - -

For the past few months, I've been adding a transient semantics to Typed Racket.
The project is called Shallow Typed Racket.
Details are in the [RFC](https://github.com/racket/typed-racket/pull/952)
 and [pull request](https://github.com/racket/typed-racket/pull/948).

The short story is that the new Shallow Racket does less to enforce types
 when typed code interacts with untyped code.
Typed code is still type-sound, but that's about it.
By contrast, types are much stronger in classic Typed Racket.

Shallow Racket's weaker types allow more programs to run.
While testing whether the new freedom is useful, I reviewed a few years of
 Typed Racket questions on the [Racket mailing list](https://groups.google.com/g/racket-users).
There were a surprising number of questions that went like this:

> **Q.** Hey, I ran a program expecting _X_ to happen, but _Y_ happened instead.
> Is this a bug?
>
> **A.** No, Typed Racket has to do _Y_ because of its strong types.

... but changing to shallow types gives the _X_ behavior!
Here are their stories.

Going forward, **Deep** refers to normal Typed Racket and **Shallow** refers to Shallow Typed Racket.

- - -


## Higher-Order Value as Any

Original message : [groups.google.com/g/racket-users/c/cCQ6dRNybDg/m/CKXgX1PyBgAJ](https://groups.google.com/g/racket-users/c/cCQ6dRNybDg/m/CKXgX1PyBgAJ)

#### On 2018-04-16, _mailoo_ wrote:

>  I play a little with the "Any" type (due to 'dynamic-require' which 
>  return Any), and I'm not able to cast them back in a function. 
>
>  I (over) simplify my question with this little program : 

```
(: p Any) 
(define (p i) (displayln i)) 

; Here I want to get back my function 
(define proc (cast p (-> Integer Void))) 
(proc 2) 
```

>  but I get this error when I try to execute the function : 

```
; contract violation 
; Attempted to use a higher-order value passed as `Any` in untyped code: #<procedure:p> 
```

### What's going on?

**Deep** raises an error because it must enforce the `Any` type with a contract that
 rejects all interactions.
Things would go badly if an Any-typed function expected a String but got an
 Integer.


### How's transient?

**Shallow** prints 2 and returns void. No error. Same goes for dynamic-require.

- - -


## Parametric Contract Affects Untyped Code

Original message : [groups.google.com/g/racket-users/c/ZbYRQCy93dY/m/kF_Ek0VvAQAJ](https://groups.google.com/g/racket-users/c/ZbYRQCy93dY/m/kF_Ek0VvAQAJ)

#### On 2019-12-15, John Clements wrote:

>  It looks like my quick attempt at importing index-of into TR is running into a problem. Here’s the program I ran: 

```
  #lang typed/racket 

  (require/typed racket/list 
  [index-of (All (T) ((Listof T) T -> (U False Natural)))]) 

  (index-of '(n s e w) 'n) ;; returns... #f? 
```

>  In typed/racket/no-check this returns 0, and also in racket (mutatis mutandis).
>
>  I thought this might be some kind of parametricity issue, but even when I instantiate index-of at Symbol which should pretty much clear the way for arbitrary equality checking, I still get False.


### What's going on?

**Deep** enforces parametricity for `All` types, and this throws off the equality
 function that index-of uses internally.


### How's transient?

**Shallow** returns 0.

ps John, thanks very much for working on [Advent of Code](https://adventofcode.com) and mailing the list!

- - -


## Unable to Protect Opaque Value as Any

Original message : [groups.google.com/g/racket-users/c/jtmVDFCGL28/m/jwl4hsjtBQAJ](https://groups.google.com/g/racket-users/c/jtmVDFCGL28/m/jwl4hsjtBQAJ)

#### On 2019-12-11, Marc Kaufmann wrote:

> I have one file called `type-test.rkt` with the following

```
#lang typed/racket

(require (only-in typed/web-server/http response/xexpr response))

(provide f2)

(: f2 (-> (U response Any)))
(define (f2)
  (define x '(body (h1 "Try it")))
  (: resp response)
  (define resp (response/xexpr x))
  resp)
```

> Then I have another *untyped* file for a servlet:

```
#lang racket

(require "type-test.rkt"
         web-server/servlet
         web-server/servlet-env)

(define (start req)
  (f2))

(serve/servlet start
               #:servlet-regexp #rx""
               #:launch-browser? #false
               #:port 8080)
```

> Notice that I am telling [f2] that `resp` is of type `response`. Yet, when I run the server with `start` [....] I get the following result:
>
> (f2): Error, see below.
>
> The error is:

```
f2: broke its own contract
  any-wrap/c: Unable to protect opaque value passed as `Any`
  value: #<response>
  in: the range of
      (-> Any)
```

### What's going on?

**Deep** tries to enforce the `Any` type with a contract that rejects all
 interactions, but needs to know what interactions are possible in order
 to make a reject-all contract.
For many values, Deep can ask questions like procedure? and struct-info
 to learn enough.
But this program sends an opaque response struct across a boundary and
 Deep does not have the right inspector to learn about the struct fields.


### How's transient?

**Shallow** does nothing to enforce the Any type.
This program runs, and in general Shallow never complains about opaque values.

- - -


## Type Inference Installs a Precise Type

Original message : [groups.google.com/g/racket-users/c/2X5olKMV3C4/m/mJhsp9ZWBgAJ](https://groups.google.com/g/racket-users/c/2X5olKMV3C4/m/mJhsp9ZWBgAJ)

#### On 2020-02-14, John Clements wrote:

> I think I may understand what’s going on here, but a student and I worked on this for quite a while today before I found the problem. 
>
> Here’s a program: 

```
#lang typed/racket 

(define-type Store (Mutable-HashTable Integer Value)) 
(define-type Value (U Real Boolean String)) 

(define top-store
  (cast
    (make-hash (list (cons -1 14) (cons 1 #t) (cons 2 #f)))
    Store))

(hash-set! top-store 5 1234)
```

> It fails with this error:

```
contract violation
expected: (or/c (and/c byte? positive?) #t #f)
given: 1234
in: the values of
the 3rd conjunct of
(and/c hash?
       hash-mutable?
       (hash/c exact-integer?
               (or/c (and/c byte? positive?) #t #f)
               #:immutable #f))
```

### What's going on?

First off, **Deep** runs fine after swapping `cast` for `ann`.

Second, Typed Racket does try to generalize inferred types for mutable data.
If the only value in the hash is the byte 14 then Deep also runs.

The problem is that Typed Racket does not generalize the inferred value type
 (U Byte Boolean) and that cast is a run-time tool for enforcing types.
Casts create contracts to protect mutable data.
In this program, there are two contracts:
 one based on the Store type to protect code that uses the hash,
 and one based on the inferred type to protect the hash against bad writes.
That second contract raises the error message.


### How's transient?

**Shallow** runs successfully.
The cast looks for a hash, does not make a contract, and ignores the inferred
 type going forward.

- - -


## Same-Arity Functions in a Case Lambda

Original message : [groups.google.com/g/racket-users/c/BDrrgW0axGQ/m/P31NxeGHAAAJ](https://groups.google.com/g/racket-users/c/BDrrgW0axGQ/m/P31NxeGHAAAJ)


#### On 2019-07-05, Ryan Kramer wrote:

> In the code below, can `maybe-car` have the given type [....]?

```
#lang typed/racket

(module untyped racket
  (provide maybe-car)
  (define (maybe-car x)
    (cond
      [(pair? x) (car x)]
      [else x])))

(require/typed
 'untyped
 [maybe-car (All (a b) (case->
                        (-> (Pairof a b) a)
                        (-> a a)))])
```

> [Current error:]

```
Type Checker:
 Type (All (a b) (case-> (-> (Pairof a b) a) (-> a a)))
  could not be converted to a contract:
   function type has two cases of arity 1
```

### What's going on?

**Deep** tries to enforce the type with a Racket `or/c` contract, but cannot.
The problem is that or/c only has partial support for unions.
If or/c ends up with two possible higher-order options at runtime, it halts.
In this case, we end up with two function contracts that have the same arity
 and don't know which to apply to an incoming function.

Note, the "Type Checker" error message is much better than what or/c would
 give on its own.


### How's transient?

**Shallow** simply checks that maybe-car accepts both arities inside the
 case-> type.
The code runs fine.
Later, when the function gets applied in typed code, Shallow spot-checks the
 results.

- - -


### Immutable Type Affects Untyped Code

Original message : [groups.google.com/g/racket-users/c/UD20HadJ9Ec/m/Lmuw0U8mBwAJ](https://groups.google.com/g/racket-users/c/UD20HadJ9Ec/m/Lmuw0U8mBwAJ)

#### On 2020-02-17, Bertrand Augereau wrote:

> Hello everybody,
> I'm trying to gradually type my script to make it a proper app (yes
> I'm a static-ish guy) and I have an issue (Racket 7.6 CS).

```
; racket_mod.rkt:
#lang racket

(provide (struct-out s))
(provide list-of-s)
(provide set-list-of-s!)

(struct s (a))
(define list-of-s '())
(define (set-list-of-s! los)
  (set! list-of-s los))
```

```
; racket_mod_typed.rkt:
#lang typed/racket

(provide (struct-out s2))
(provide list-of-s2)
(provide set-list-of-s2!)

(struct s2 ([a : Natural]))
(define list-of-s2 '())
(define (set-list-of-s2! [los : (Listof s2)])
  (set! list-of-s2 los))
```

```
; racket_main.rkt:
#lang racket

(require "racket_mod.rkt")
(require "racket_mod_typed.rkt")

(define los (list (s 1) (s 2)))
(set-list-of-s! los)
(displayln list-of-s)

(define los2 (list (s2 1) (s2 2)))
(set-list-of-s2! los2)
(displayln list-of-s2)
```

> list-of-s2 is empty and list-of-s is not, the only difference seems to
> be the type annotations.
> Can someone help me ? :)


### What's going on?

**Deep** enforces the type of `list-of-s2` with a listof contract, which
 ends up making a copy of the original (empty) list as it traverses and
 validates it.
The original value does change in typed code, but the main module only has
 access to the empty copy.

Here's a step-by-step breakdown:

1. the typed module creates an empty list-of-s2
2. the main module imports the list and receives a new copy
3. the main module calls set-list-of-s2! and the typed module updates the original list-of-s2 variable
4. the main module reads from its copy --- and it's still empty



### How's transient?

**Shallow** lets the original list travel to untyped code.
There are no contracts in the way.


## Discussion

Wow!
It's great to see that Shallow Racket works "as expected" on these examples.
I hope the Shallow option makes types more accessible to more Racket programmers
 in the future.

If you have a similar experience with a deep-types error, let me know.

Keep in mind, though, the freedoms of shallow types allow silent failures.
A value can pass by a mis-matched type annotation without Shallow raising an
 error --- and if that happens, the end result may be really, really confusing.
Of course you can always switch back to Deep Typed Racket for debugging.

Shallow Typed Racket is coming soon.
Follow the [pull request](https://github.com/racket/typed-racket/pull/948)
 or watch the Racket release notes for news.


### Links

- [Larger example](http://prl.ccs.neu.edu/blog/2019/10/31/complete-monitors-for-gradual-types/)
  where Shallow misses an error that Deep catches
- Michael M. Vitousek [invented](http://hdl.handle.net/2022/23172)
  the Transient semantics and implemented it in
  [Reticulated Python](https://github.com/mvitousek/reticulated).
- My [upcoming dissertation](https://ccs.neu.edu/home/types/publications/publications.html#g-thesis-2020)
  has lots more to say about Shallow Typed Racket.

_Thanks to Artem Pelenitsyn for reading and criticizing an early version of this post._

