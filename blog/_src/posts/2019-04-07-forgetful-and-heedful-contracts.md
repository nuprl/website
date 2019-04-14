    Title: Forgetful and Heedful contracts
    Date: 2019-04-07T23:15:11
    Tags: migratory typing, higher-order contracts, by Ben Greenman

_Forgetful_ and _heedful_ are two methods for space-efficient contracts
 developed by Michael Greenberg circa 2014.
These methods were born in the shadow of a third method with stronger
 theoretic properties.
Since then, however, the forgetful method has been re-invented at least twice.
Both deserve a second look.
<!-- TODO cite? -->

<!-- more -->
- - -

Contracts are a tool for specifying and dynamically-enforcing the behavior
 of a program.
In a language with contracts, a programmer can annotate an API with
 code that documents the intended use for other readers.
When client code interacts with the API, the annotations ensure that the
 actual behavior matches the expected.
If there is a mismatch, the contract annotations can report an issue
 in terms of: the API code, the client code, and the contract between them.

For example, a Racket module that exports a sorting function can use a contract
 to describe the kind of input it expects.
If a client module sends invalid input, the contract **blames** the client
 module for the error **assuming** that the contract is bug-free:

```
  #lang racket/base

  (module sort racket
    (provide
      (contract-out
        [quicksort
          (-> (vectorof point/c) void?)]))

    (define point/c (vectorof integer?))

    (define (quicksort points)
      ....))

  (module client racket
    (require (submod ".." sort))
    (quicksort '()))

  (require 'client)
```

```
quicksort: contract violation;
 expected a vector
  given: '()
  in: the 1st argument of
      (-> (vectorof (vectorof integer?)) void?)
  contract from: 
      (file.rkt sort)
  blaming: (file.rkt client)
   (assuming the contract is correct)
```

That covers the basics.
For an extended introduction to contracts, visit the Racket guide.
<!-- TODO cite -->

> The quicksort example and the related figures are from the paper
> [_Collapsible Contracts: Fixing a Pathology of Gradual Typing_](TODO)


### Contracts and "Space Efficiency"

The `(vectorof point/c)` contract used above describes a possibly-mutable
 array that contains elements that match the `point/c` contract.
Since the array can be mutated, this contract has implications for two parties:

1. the client module must supply a good array, and
2. the sorting module must not insert a bad element.

To enforce the second condition, the `vectorof` contract wraps incoming
 vectors in a proxy that checks future writes.
Suppose the client sends a vector with four points:

```
(vector (vector 4 4)
        (vector 2 2)
        (vector 1 1)
        (vector 3 3))
```

After applying the contract, the vector is wrapped in a proxy that checks
 incoming writes and outgoing reads.
The following picture illustrates the wrapper with a **solid** blue bar
 for the **write** checks against the sort module and a _striped_ blue bar
 for the _read_ checks against the client.

![Figure 1: True running time vs. predicted running time for 16 configurations](img/posts/2019-04-07-forgetful-and-heedful-contracts/vector-chaperone-0.png)





Perhaps you've heard about space-efficient contracts.
The problem is this, the goal for a solution is that.

In a POPL 2015 paper, Michael Greenberg presented one strategy.
The _eidetic_ contracts in the paper work like this, and preserve behavior and
blame.

Michael invented two other space-efficient calculi: forgetful and heedful.
You can read more in the extended version of his POPL 2015 paper.

### A Problem with Normal (Space-Inefficient) Contracts

### Forgetful Space-Efficiency

Mission accomplished, space is constant.
But forgetting the contracts also forgets the blame.

### Heedful Space-Efficiency

Okay we have a weaker bound but a better story for blame.

The eidetic design goes further --- with a weaker bound and a more complicated
 data structure --- and gets blame exactly right.

Read more in Michael's paper, or follow Dan's example.


### Imitation and Flattery

Vitousek, transient, forgetful space efficiency ... something like heedful blame
but undefined behavior for relatively simple example.
Link to my webpage?

Castagna and Lanvin, forgetful contracts, only cared about soundness anyway.

> _Forgetful_ is an interesting middle ground: if contracts exist to make
> partial operations safe (and not abstraction or information hiding),
> forgetfulness may be a good strategy.
<!-- Section 10, bottom of page 23 -->

I don't know any heedful imitators besides myself.


### Well why not eidetic?

Simpler
easier to implement
faster

talk about oopsla contracts, overhead ... huge amount of work, but huge payoff merged in Racket xxx
but many contracts remain


### Strawmen

> Since _eidetic_ and [classic contracts] behave the same, why bother with
> _forgetful_ and _heedful_? First and foremost, the calculi offer insights
> into the semantics of contracts: the soundness of _forgetful_ depends on a
> certain philosophy of contracts; _heedful_ relates to threesomes without blame
> [Siek and Wadler 2010]. Second, we offer them as alternative points in the
> design space. Finally and perhaps cynically, they are strawmen---warp up
> exercises for _eidetic_.
<!-- Section 1, bottom of page 2 -->

Why didn't forgetful and heedful appear in the POPL paper?
In hindsight they shed light on their successors.

The quote above about strawmen might have encouraged the review committee to
ask for their removal.
Who knows.
But surely nobody wants to waste their time with a ``strawman'' design
or acknowledge that their current work is built on a foundation of straw.

So maybe next time, acknowledge the limitations of your design without
a cute allusion?
