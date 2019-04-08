    Title: PSA: Heedful and Forgetful space-efficient contracts
    Date: 2019-04-07T23:15:11
    Tags: migratory typing, by Ben Greenman

Quick reminder about two lesser-known methods for space-efficient contracts.
They have been re-invented at least once.

<!-- more -->

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
