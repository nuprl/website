    Title: Forgetful and Heedful contracts
    Date: 2019-04-07T23:15:11
    Tags: migratory typing, higher-order contracts, by Ben Greenman

_Forgetful_ and _heedful_ are two methods for space-efficient contracts
 developed by [Michael Greenberg](http://www.cs.pomona.edu/~michael/) in [2014][g].
These methods were born in the shadow of a third method, _eidetic_,
 with stronger theoretic properties.
Since then, however, the forgetful method has been re-invented at least twice.
Both deserve a second look.

<!-- more -->
- - -

Contracts are a tool for specifying and dynamically-enforcing the behavior
 of a program.
In a language with contracts, a programmer can annotate an API with
 code that documents the intended use for other readers.
When client code interacts with such an API, the annotations ensure that the
 actual behavior matches the expected.
If there is a mismatch, the contract annotations can report an issue
 in terms of [three parties](https://www2.ccs.neu.edu/racket/pubs/popl11-dfff.pdf):
 the API code, the client code, and the contract between them.

For example, a Racket module that exports a sorting function can use a contract
 to describe the kind of input it expects.
If a client module sends invalid input, the contract blames the client
 module for the error, assuming that the contract is bug-free:

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
For an extended introduction to contracts, visit
 [The Racket Guide](https://docs.racket-lang.org/guide/contracts.html).

The quicksort example and the related figures are from the paper
 [_Collapsible Contracts: Fixing a Pathology of Gradual Typing_][fgsfs]


### Classic contracts and "Space Efficiency"

The `(vectorof point/c)` contract used above describes a possibly-mutable
 array whose elements match the `point/c` contract.
Since the array can be mutated, this contract has implications for two parties:

1. the client module must supply a good array, and
2. the sorting module must not insert a bad element.

To enforce the second condition, the `vectorof` contract wraps incoming
 vectors in a proxy that checks future writes.
Suppose the client sends a vector with four points:

```
(quicksort (vector (vector 4 4)
                   (vector 2 2)
                   (vector 1 1)
                   (vector 3 3)))
```

After applying the contract, the vector is wrapped in a proxy that checks
 incoming writes and outgoing reads.
The following picture illustrates the wrapper with a **solid** blue bar
 for the **write** checks against the sort module and a _striped_ blue bar
 for the _read_ checks against the client.

<img src="/img/vector-chaperone-0.png" alt="A wrapped vector"/>

In a straightforward implementation, these wrappers can stack up if multiple
 contracts are applied to the same value.
For our quicksort in particular, the elements of the vector are mutable
 vectors and may accumulate wrappers as the vector is sorted ---
 because every **write** and _read_ applies a contract to the element.

<img src="/img/vector-chaperone-1.png" alt="Layers of element wrappers"/>

On the bright side, these wrappers enforce the contracts and help the
 programmer understand the source of the error if any contract is violated.

Unfortunately, the wrappers also affect the performance of the program.
There are prices to pay for:
 (1) checking values against the contracts,
 (2) allocating new wrappers,
 (3) and "indirecting" future writes/reads through wrappers.
These space and time costs can add up.

> "on a randomly ordered vector of 1,000 points, a call to quicksort can
> wrap the inner vectors an average of 21 times" -- [_Collapsible Contracts_][fgsfs]

To fix the problem, researchers have been exploring _space-efficient_
 implementations of contracts that attach a bounded number of wrappers to any
 value.
Michael Greenberg is one of these researchers, and _eidetic_, _forgetful_,
 and _heedful_ are his names for three implementations.

(Although the goal of this post is to promote _forgetful_ and _heedful_,
 we will review all three.)


### Eidetic space-efficiency

The eidetic method introduces a data structure to represent higher-order
 contracts.
The structure supports a _merge_ operation;
 when two contracts meet, they are merged in a way that avoids duplication.
Eidetic contracts have the same behavior as normal "wrapping" contracts
 and their size is bounded by the number (and height) of source-code
 contracts in the program.

An eidetic contract is an `N`-ary tree (for `N > 0`):

- each node represents a higher-order contract combinator, such as `vectorof`
- the `N` children of a node represent the different interactions that the
  value supports
- each leaf is a list of non-higher-order, or _flat_, contracts

For example, the `(vectorof point/c)` source-code contract describes an
 eidetic tree with 3 nodes and 4 singleton-list leaves.
Section 3.1 of the [Collapsible Contracts][fgsfs] paper has an illustration.
Each tree node represents a `vectorof` contract;
 these nodes have `N=2` children because vectors support reads and writes.

A successful merge combines two trees of the same shape
 by re-using half the nodes
 and appending the leaf lists.
Re-using nodes saves some space, and helps reduce the overhead of trees
 relative to simple wrapping contracts.
The main savings comes from filtering the leaf lists --- if an
 implementation comes with a `contract-stronger?` predicate that tests
 whether one flat contract accepts fewer values than a second, then it
 can remove leaf-list contracts that are preceded by stronger ones.
Trees make this filtering possible.

Suffice to say, eidetic is an ideal solution in theory but comes with
 practical challenges.
Are trees more expensive than wrappers in the common case?
Can the leaf-lists in a tree share elements?
Should `contract-stronger?` try to solve problems that lack polynomial-time
 solutions?

Thankfully, there are at least two "compromise" alternatives.


### Forgetful space-efficiency

<!-- "no operation relies on e being a T2, skipping the check doesn't risk soundness" p.12 -->
<!-- "In forgetful \lambda_H, we offer a simple solution to space inefficient casts: just forget about them" p.11 -->
<!-- "Just the same, when accumulating casts on the stack, we throw away all but the last cast" p.11 -->
<!-- "forgetful ... skip[s] checks and change[s] blame labels" p.3 -->

> "Forgetful is an interesting middle ground: if contracts exist to make
> partial operations safe (and not abstraction or information hiding),
> forgetfulness may be a good strategy." -- [_Space-Efficient Manifest Contracts_][g]
<!-- Section 10, bottom of page 23 -->

The forgetful method is exceptionally simple.
When applying a new contract to a value, first check whether it is
 wrapped in a similar contract.
If so, then replace the existing wrapper with one that combines:

1. the client obligations from the old contract, and
2. the server obligations from the new contract

If not, proceed as usual --- by wrapping (an unwrapped value)
 or raising an error.
Every value receives at most **one** wrapper;
 this wrapper changes as the value flows to different clients.

Forgetful is safe in the sense that every piece of code can trust the
 top-level shape of the values it receives.
Suppose module `A` exports a function `f` with contract `(-> T1 T2)` to
 module `B`, and suppose module `B` shares this function with a few other
 client modules using different contracts.
As `f` flows to a new client, it keeps the `T1` domain check and gets a
 replacement for the `T2` codomain check.

- Keeping `T1` ensures that the code inside the function
  (defined by module `A`) receives input that matches its expectation.
- Replacing `T2` ensures that each new client receives output that it expects.

Unfortunately, replacing `T2` also means that clients of module `B` cannot
 trust the `T2` contract.
This contract is not checked, and so forgetful contracts **miss** some
 errors that would be caught by standard contracts.
For the same reason, a bug in module `B` may go undetected by its clients
 --- even if a later contract reports an issue, the contract system has
 no memory that `B` was partly-responsible.

Despite these changes in behavior, forgetful is a straightforward
 method for saving space and time relative to classic contracts.


### Heedful space-efficiency

A heedful contract is a set of classic higher-order contracts.
When applying a new contract to a value, check whether the new contract
 is in the set.
If so, ignore the new contract.
If not, add the new contract to the set --- or raise an error.
Every value gets at most one set-wrapper, and each member of a set-wrapper
 represents a new constraint.

To check a value against a set, for example when reading from a vector, check
 each of the elements in any order.
If an element raises an error, report it.*
Alternatively, an implementation can check all the elements and report
 all that disagree with the value.

The heedful method is a compromise between forgetful and eidetic.

- Unlike forgetful, heedful uses a new data structure to represent contacts
   and requires some kind of `contract-stronger?` predicate.
  Heedful also remembers (some of) the history of a value and catches the
   same errors as classic and eidetic contracts.

- Unlike eidetic, heedful uses a simpler data structure with
   no need to keep duplicate flat contracts
   depending on the order they are encountered.
  Heedful cannot, however, uniquely identify the two parties involved in a
   contract error.
  In general, there are multiple contracts that a programmer
   must inspect to find the source of a mismatch.

For details, see [the extended version][g]
 of Michael's POPL 2015 paper.
Don't bother searching [the conference version](http://www.cs.pomona.edu/~michael/papers/popl2015_space.pdf)
 --- aside from one remark
 in Appendix B, heedful and forgetful are nowhere to be found.

`*` If an implementation promises to report one mismatch, instead of all
 mismatches, then it does not need to keep the full set of contracts.
Thanks to [Michael Ballantyne](http://mballantyne.net/) for explaining
 this to me.

### Priorities and Appearances

The extended version of _Space-Efficient Manifest Contracts_ introduces
 the forgetful and heedful methods with extreme modesty.
It's tempting to skip past them and focus on the eidetic method.

> "Since eidetic and classic contracts behave the same, why bother with
> forgetful and heedful? First and foremost, the calculi offer insights
> into the semantics of contracts: the soundness of forgetful depends on a
> certain philosophy of contracts; heedful relates to threesomes without blame
> [[Siek and Wadler 2010](https://dl.acm.org/citation.cfm?doid=1706299.1706342)].
> Second, we offer them as alternative points in the design space.
> Finally and perhaps cynically, they are strawmen---warm up
> exercises for eidetic." -- [_Space-Efficient Manifest Contracts_][g]
<!-- Section 1, bottom of page 2 -->

And yet, at least two other research papers rely on these "strawmen" --- or
 rather, the ideas behind the names.

[_Gradual Typing with Union and Intersection Types_][cl],
 at ICFP 2017,
 demonstrates one technique for adding two varieties of types to a gradual
 language.
The semantics in the paper is forgetful;
 if a higher-order value crosses multiple type boundaries,
 the intermediate server obligations disappear.

> "if a lambda abstraction is preceded by multiple casts, then the rule
> erases all of them, except for the last one" -- [_Gradual Typing with Union and Intersection Types_][cl]
<!-- page 21 -->

This forgetfulness was a deliberate choice.
A classic semantics would satisfy the same type soundness theorem,
 but the authors picked forgetful for its simplicity and performance
 implications.

> "removing these casts preserves the soundness of the evaluation while
> reducing the number of them"
>
> "while this choice makes the calculus simpler without hindering soundness,
> it yields a formalism unfit to finger culprits" -- [_Gradual Typing with Union and Intersection Types_][cl]
<!-- p.27 -->
<!-- page 21 -->

<!-- The followup at POPL 2019 is not forgetful. -->
<!-- It's similar to eager coercions ... keep all types around and error -->
<!--  if there's a new type that doesn't match the old ones. -->
<!-- Also, that paper chooses not to let functions have intersection types, -->
<!--  which kind-of-avoids the questions ... but really the eagerness is key. -->

[_Big Types in Little Runtime_][vss], at POPL 2017,
 presents a gradual typing system that avoids the use of wrappers.
Instead, their _transient_ semantics rewrites typed code ahead of time
 to mimic the checks that forgetful contracts would perform.
These checks suffice for a shallow type soundness theorem.

That paper also introduces a heedful-like strategy for improving the error
 messages produced by a forgetful check.
The strategy adds a global map to the semantics;
 keys in the map are unique identifiers for values (heap addresses),
 and values are sets of types.
When a value meets a compatible type, the type is added to the value's set.
When a mismatch occurs, the semantics [tries to report](https://www.ccs.neu.edu/home/types/resources/notes/transient-undefined-blame-extract.pdf)
 every type in the set that relates to the mismatch.

And so, forgetful and heedful were edged out of POPL 2015 but managed to sneak in
 to POPL 2017.
Since then, forgetful appeared in ICFP 2017 and, briefly, in
 [ICFP 2018](https://www2.ccs.neu.edu/racket/pubs/icfp18-gf.pdf).
Where will we see them next?

[cl]: https://dl.acm.org/citation.cfm?id=3110285
[vss]: https://dl.acm.org/citation.cfm?id=3009849
[g]: https://arxiv.org/abs/1410.2813
[fgsfs]: http://users.cs.northwestern.edu/~robby/pubs/papers/oopsla2018-fgsfs.pdf
