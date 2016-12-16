# Gradual Type Precision as Retraction
2016-12-15
Max New

Notes by Ben Greenman
https://prl.ccs.neu.edu/seminar-notes/README.md

# Intro

In the 60s and 70s, Dana Scott modeled the typed lambda calculus with
projections from the untyped lambda calculus [1][2]. In the 90s, Robby Findler
and Matthias Blume explored the idea of contracts as "pairs of projections" [3].

  [1] "Relating Theories of the Lambda Calculus", 1980.  
      http://andrewkish-name.s3.amazonaws.com/scott80.pdf  
  [2] "Data Types as Lattices", 1975.  
      http://agp.hx0.ru/arts/Scott.Datatypesaslattices.1975.pdf  
  [3] "Contracts as Pairs of Projections", FLOPS 2006.  
      https://www.eecs.northwestern.edu/~robby/pubs/papers/fb-tr2006-01.pdf  

M: note that Robby stopped working on contracts as pairs of projections
   because he wasn't able to use it to describe dependent contracts  
Max: well I'm not working on contracts in general,

In the 00s, people started working on gradual typing. This talk will outline
how Dana Scott's ideas can provide a semantic foundation for gradual typing,

Let's take this connection seriously and see if it helps us design contract
systems / gradually typed languages better.


# TLDR; Scott

A _retraction_ between domains `A` and `B` is a pair of functions:
- `s : A >--> B`
- `r : B -->> A`
- such that `s` is injective and `r` is surjective
- and composing `r ∘ s` gives the identity function

The function `s` is called a _section_ and the function `r` is called a
_retraction_. Together, they are a _section/retraction pair_.

Here is a picture:

```
            s    *-------*
    *---*  >-->  |       |
    | A |        |   B   |
    *---*  <<--  |       |
            r    *-------*
```

> Will:
>   okay, a retraction implies an isomorphism between A and a subset of B

Composing `s ∘ r` may not be the identity on `B`, but it is an _idempotent_ on
`B`.  An _idempotent_ on a domain `X` is a function `e : X -> X` such that
`e ∘ e = e`.

Here is a proof that `s ∘ r` is an idempotent on `B`:

```
    (s ∘ r) ∘ (s ∘ r) =
    s ∘ (r ∘ s) ∘ r =
    s ∘ id_A ∘ r =
    s ∘ r
```

Scott proved that idempotents in a typed λ calculus are in close correspondence
with untyped λ calculi. More precisely, Scott proved [1]:

> Every untyped theory is the theory of a reflexive domain in a typed theory.

Connection to gradual typing: every type is the retract of a dynamic type.
(Every untyped term can be typed at a dynamic type and every typed term
can be type-erased.)


# TLDR; Siek/Taha

A gradually typed languages allows typed and untyped terms in the same program.
The type system accepts any terms that *might possibly* be well-typed.
The runtime system inserts casts / coercions that check whether the runtime
value of untyped terms matches the types that typed terms expect.


# The Idea

Contracts / casts / coercions in a gradually typed language have a natural
interpretation in terms of sections and retractions.

> M:
>   a minute ago, you threw in a sub-clause "the contracts we can check at
>   runtime", so you're assuming a topological space where everything is
>   computable. When you pick such spaces, you usually get some junk that doesn't
>   have the right properties.  
> Max:
>   I don't care about denotational semantics, I care about programming
>   languages, so I'm not worried about the junk. It doesn't come up.  
> M:
>   at some point you're going to show us a concrete category, rigth?  
> Max:
>   yes, coming soon


# Blame

There's a problem with this picture, blame doesn't have an obvious Dana Scott
interpretation. How do we relate blame?

Does blame have to be a structure I define on the language,
or can we have semantic properties that blame needs to satisfy?
I'm looking for emantic concepts that inform how we implement blame in a
programming language.

> M:
>   read Robby's tech. report [3], there are arrows that represent blame  
> Christos:
>   CPCF [4] has a nice model for blame

  [4] "Correct Blame for Contracts", POPL 2011.  
      http://www.ccs.neu.edu/racket/pubs/popl11-dfff.pdf

Wadler and Findler[5] defined gradual type precision in terms of blame.
I'm going to derive type precision, and we'll see that it's nearly the same
definition but disagrees at one important point.

> M: good!

  [5] "Well Typed Programs Can't be Blamed", ESOP 2009.  
      http://homepages.inf.ed.ac.uk/wadler/papers/blame/blame-tr.pdf


# META LESSONS
- always start with the middle whiteboard
- you can always move whiteboards as you write!


# Lets get more concrete

Types in our gradually typed language include ground types, function types,
and the dynamic type.

```
  τ := Bool | Num | τ → τ | Dyn
```

The simply-typed rule for checking an application mostly works, but you have
to do a little extra:

```
    Γ ⊢ t : A → B
    Γ ⊢ u : C
    ???
    ----
    Γ ⊢ t u : B
```

The idea is,
- if `A` and `C` are static types, they must be equal.
  (be strict on static types!)
- if `A` or `C` is Dyn, this should typecheck

> M:
>   what if `Γ ⊢ t : Dyn` ?  
> Max:
>   it typechecks, and a runtime cast coerces `t` to a function type

The source language has **implicit casts** and typechecking makes these
 casts **explicit**.

And so, the full application rule uses a *type compatibility relation*, τ ~ τ

```
    Γ ⊢ t : A → B
    Γ ⊢ u : C
    A ~ C
    ----
    Γ ⊢ t u : B
```

Here are two rules for `τ ~ τ`:

```
   ---
   A ~ Dyn


   A ~ A'
   B ~ B'
   ---
   A → B ~ A' → B'
```

Wadler/Findler also define a *type precision relation* `A ⊑ B`
to say when `A` is "more precise" (i.e. has fewer `Dyn`) than `B`.

"A is a subset of B" is a fair intuition.

Here's one surprising rule from the type precision relation you see in the
literature:

```
   A ⊑ A'
   B ⊑ B'
   ---
   A → B ⊑ A' → B'
```

Scary right? Wadler/Findler call this "naive subtyping".
But it makes sense of you think of `⊑` as "has fewer Dyn".

(Max comments:
> Subset is possibly a misleading intuition for `A ⊑ B`. However, it agrees
> with a slightly different notion which Mellies-Zeilberger [6] call
> refinement in which you think of `A -> B ⊑ A' -> B'` as meaning that we
> are seeing `A -> B` as the subset of `A' -> B'` functions that happen to,
> when given things in the subset `A ⊑ A'`, give you values that land in the
> subset `B ⊑ B'`.

  [6] http://noamz.org/papers/funts.pdf
)

With type intersection `A ⊓ B`, such that:

```
   A ⊓ B ⊑ A
   A ⊓ B ⊑ B
```

Type compatibility falls out:

```
   A ~ B := ( A ⊓ B /= ∅ )
```

> Will:
>   `τ ~ τ` is not transitive?  
> Max:
>   right, for the same reason that non-empty intersection isn't transitive

Here's the typing rule for an explicit cast from type `A` to type `B`:

```
    Γ ⊢ t : A
    A ~ B
    ---
    Γ ⊢ <B ⇐ A>p t : B
```

To implement a gradually typed language, you need to introduce tags that stick
around at runtime. Use these to implement `τ ~ τ`.

```
    G := Bool | Num | Dyn→Dyn
```

We'll use `(Dyn_G v)` to represent a value `v` that's tagged with the ground
type `G`.

Here's the dynamic semantics of casts from Wadler / Findler.
These `p` are "blame labels" that are used to report errors to the programmer.

```
    <B ⇐ B>p v ↦ v

    <Dyn ⇐ G>p v ↦ (Dyn_G v)

    <Dyn ⇐ A→B>p v ↦ Dyn_(Dyn→Dyn) (<Dyn→Dyn ⇐ A→B>p v)

    <A'→B' ⇐ A→B> v ↦ (λ (x : A') <B' ⇐ B> (v (<A ⇐ A'>p' x)))
```

(BG: there are 2 more rules incoming)

And `p'` is `p` with blame reversed.

> M:
>   the `p'` operations assumes you have a closed system, in the real world,
>   programs are open and you can link new types in. How are you going to compute
>   `p'`?  
> Max:
>   at link-time, just wait until you have the whole program  
> M:
>   what's cheating about p' is that
>   in principle you should write these rules with an evaluation context
>   and the `'` operation would take a second argument,
>   like `p' = reverse_blame(p, e)`,
>   where `e` is the original context where `p` was generated.
>   (when the program is loaded)  
> Christos:
>   right, the continuation that really matters is the context where you attach
>   the contract  
> Max:
>   exactly, you can't forget it and attach it later  
> Will:
>   we get the point, using `e` and a context would clutter the presentation  
> M:
>   theoreticians say "let there be a fresh variable", we all know things go
>   horribly wrong if you're not careful about that

The last rule is where checking happens:

```
    <A ⇐ Dyn>p (Dyn_G v) ↦ <A ⇐ G>p v   -- if `G ~ A`
    <A ⇐ Dyn>p (Dyn_G v) ↦ blame p      -- otherwise
```

If G is compatible with A, just keep checking the underlying value
if not compatible, type error.
Who's to blame? The dynamic value, for not satisfying the type.

I like my syntax to mean something, I want a semantic definition in terms
of these things here. And I'll give one in terms of section/retraction pairs.

> M:
>   in between today and Wadler/Findler, Christos gave a semantics in terms of
>   blame assignment  
> Christos:
>   I disagree with that statement  
> M:
>   you could easily add this chapter to your dissertation


# Type Precision as Section/Retraction

Rule #1 in a gradually typed language is that you can cast from any type
to `Dyn` and back again. In other words:

```
    A >--> Dyn -->> A
```

is the identity function. This makes a section/retraction pair for any
non-function type `A`.

Might work for `A→B` with an eta law. Not sure.

Now type precision is a statement about how these casts to/from Dyn commute:

```
    A ⊑ B

    :=

    A >--> Dyn

    v
    |       =
    v

    B >--> Dyn
```

(BG: the vertical arrow means `A >--> B`)

and

```
    A <<-- Dyn

    A
    A       =
    |

    B <<-- Dyn
```

(BG: the vertical arrow is supposed to represent `A <<-- B`)

`A` is more precise than `B` if casting from `Dyn` to `B` and then casting to
`A` is the same as casting directly from `Dyn` to `A`. You can say, the cast
to `A` checks more things, so it's more likely to fail.

All of these casts that happen at runtime in a gradually typed language
factor into uses of the above two diagrams.

> Christos:
>   this is exactly whaat you're doing with the third rule, right?
>   by first casting to Dyn→Dyn and then casting to Dyn  
> Max:
>   right, and the fact that these arrows commute is what justifies removing
>   extra casts in your implementation


Another notion from Wadler / Findler is positive and negative subtyping.
They define type precision in terms of <:+ and <:-

```
    A ⊑ B := A <:+ B and B <:- A

    A <:+ B := <B ⇐ A>p never blames the "positive" party in `p`

    B <:- A := <A ⇐ B>p never blames the "negative" party in `p`
```

Unlike naive subtyping, the definitions of `<:+` and `<:-` are contravariant
for functions. Contravariance regained! For example:

```
    A' <:+ A
    B <:- B'
    ---
    (A → B) <:- (A' → B')
```

In Wadler/Findler, `G <:- Dyn`. This means casting from the ground type
never blames the negative party.

> M:
>   so that's asymmetric  
> Max:
>   well it's not the case that a cast from Dyn to anything never blames

OK, so Wadler / Findler got type precision from these 2 judgments.
If we follow the section/retraction intuition we get 2 judgments too,
one for sections and one for retractions. In general:

```
  A <:+ B iff A >--> B
```

positive subtyping implies we can extract a section,

```
  B <:- A iff B -->> A
```

negative subtyping implies we can extract a retraction.

Since every (ground) type comes with a section/retraction pair into Dyn,

```
    ---
    A <:+ Dyn

    ---
    Dyn <:- A
```

we can derive <:+ and <:-
Here's one example:

```
    A' <:- A
    B <:+ B'
    ---
    A→B <:+ A'→B
```

The term that computes this is:

```
    λ(f : A→B) s_B ∘ f ∘ r_A
```

> Christos:
>   isn't this just what Robby and Matthias Blume did in their implementation?  
> Max:
>   yes  
> M:
>   Christos, you did this too.  
> M:
>   in our world (the tech. report version) we use a pair of projections.
>   Does one of them correspond to your sections?  
> Max:
>   no it's different  
> M:
>   if we can figure that out, we can use topology instead of ...  
> Christos:
>   what's the difference?  
> Max:
>   This one's typed, you can only get one projection (one idempotent) from this.
>   But there, you have 2 projections. So strictly more information.
>   Maybe this one is insufficent to justify blame.  
> M:
>   exactly, you need the 2 for the blame part  
> Max:
>   Well you still have 2 functions, so still possible to distinguish
>   But there you really need 2 contracts  
> M:
>   maybe we can build more  
> Max:
>   maybe you have more junk  
> M:
>   In Scott's world, any retract is a type. Just because the type terrorists
>   haven't written them down doesn't mean they won't be useful  
> Max:
>   agreed  

We get **mostly** the same rules for positive and negative subtyping, but:

```
    G <:- Dyn
```

Is NOT true.
If it was true, that would mean that casting

```
    <Boolean ⇐ Dyn> (<Dyn ⇐ Boolean> v)
```

would have to be the identity for any `v`. That's obviously false.

Let's look more at this disagreement. Maybe Wadler and Findler defined blame
wrong. Maybe this cast should maybe raise negative blame.

Look at the cast from ground types to `Dyn`. It turns out, `G <:- Dyn` was
implied by the dynamic semantics of casts:

```
    <Dyn ⇐ G>p v ↦ Dyn_G v
```

because this rule throws the blame label `p` away.
So it says no party is ever blamed.

We could change the dynamic semantics to keep `p`.

```
  <Dyn ⇐ G>p v ↦ Dyn_G(v, p)
```

And we also need to change the rule that performs a cast:

```
  <A ⇐ Dyn>p (Dyn_G (v, q)) ↦ <A ⇐ G>p v      --- if G ~ A
  <A ⇐ Dyn>p (Dyn_G (v, q)) ↦ blame(p, q')    --- otherwise
```

Before, we always blamed `p`. Reasoning was, `v` is a bad dynamic value.
Why not blame `q'`, the context, for performing the cast on `v` ?

> Christos:
>   Let me see if I understand correctly. If I have a function with a contract
>   that says it expects booleans, then what does this contract mean?
>   Is it:
>   - my callers have an obligation to produce booleans?
>   - I have an obligation to use my argument as a boolean?
>   Is this the distinction you're making?  
> Max:
>   yes, I'm saying it's reasonable to blame the function for not using its
>   argument as a boolean

Here's an example, imagine we have two typed printing functions.

```
    printN : Num → ⊥
    printB : Bool → ⊥
```

And a small program that does these casts:

```
    ⊤ -> B ⇒ Dyn ⇒ N -> ⊥
```

(BG: maybe,

```
   ((λ (x : Bool)
     (printN (<Nat ⇐ Dyn>(<Dyn ⇐ Bool> x))))
    true)
```
)

Racket would blame `true` when the cast fails.
It tells you where you can fix the program to prevent the error.
But how does the language know that `printN` is the correct function?
Maybe it's a mistake. Only the programmer knows.

> Will:
>   I doubt it

> M:
>   As you know, for 5 years I have said to the amusement of conference audiences
>   that "Phil Wadler is wrong". When two things crash, either one could be at
>   fault.  
> Max:
>   In my language, you still have the proeprty that well-typed programs
>   don't get blamed  
> M:
>   The BOUNDARY gets blamed. The untyped guy was innocent and suddently got
>   confronted with a brutal type  
> Max:
>   yes, mismatch between type and a dynamic term  
> BenS:
>   could you actually write this program?  
> Max:
>   yeah, type systems for gradually typed languages are very stupid

This is non-transitivity of compatibility coming through.
It also shows that type compatibility doesn't give you strong safety guarantees.

In a big program, the two parties could be very far apart.
It's not just applying `true` immediately to a function.
The Racket implementation is throwing away good debugging information!

> M:
>   This message has been lost at ICFP, they are praying to the god of
>   specifications. They never want to hear that specifications can be wrong.
>   Very few places in the world that admit specs can be wrong.  
> Max:
>   And you're all in one of them :)  
> Christos:
>   What's wrong is using types to generate specifications on untyped code.

My system doesn't blame more often than the original, but gives more information
when it does blame.

Is there a connection to complete monitoring?

> Christos:
>   This isn't about complete monitoring, but it may violate "correct blame"
>   as defined in the papers. The very shallow structure for blame (because of
>   the "blame-swapping" `p'`) make it incomparable to CPCF [4].  
> M:
>   The direction of flow is very important, if I have no control over someone
>   stuffing a value down my throat, I can't be blamed  
> Max:
>   The old system just doesn't blame enough parties.
>   I guess it's a "completeness of blame" problem.  
> Christos:
>   Completeness of monitoring means you covered all the channels.
>   So this system doesn't affect complete monitoring.
>   Correctness of blame means that wherever I point you to, you can fix something
>    there and move the blame somewhere else.
>   Seems like you want to blame the place with the "least distance" to a
>    "correct" program.  
> Max:
>   no no, it's about blaming **all** the places that you could make a change
>   to fix the issue

> Ryan:
>   What is `p`? What does `p` mean? What is a party?
>   Is `p` a party? We haven't seen a definition yet.
>   Seems more that p should be a boundary with 2 sides  
> M:
>   `p` is the boundary and is preferential of one side as "the party"  
> Ryan:
>   Do the authors of the original system think of `p` as a pair of parties?

> Christos:
>   I've been looking at contracts for information flow, your idea is related.
>   You want a taint-tracking system for base values.
> 
>   The meta-theory definition of correct blame tracks provenance. The Racket
>   contract system doesn't. I've talked to Matthew about implementing this,
>   and he says it **will not** happen, for a very practical reason.
>   The issue is that you need proxies for every base value and every primitive
>   operation needs to dispatch on these proxies.  
> M:
>   In smalltalk, where 5 is an object, you can maybe do this.
>   In our world it is way too expensive.  
> Max:
>   You don't need to proxy, just add the blame information to the dynamic tag
>   you're already keeping on values, to implement `integer?` etc.  
> M:
>   Does the metadata have unbounded size?  
> Max:
>   No, each `p` is just a source location. That's a fixed size if you know the
>   number of locations  
> Will:
>   Fixed number of bits seems easy  
> M:
>   But that's a lot of bits  
> Will:
>   It's logarithmic.  
> Max:
>   For provenance, you just need the original location. That's one source
>   location for each value.  
> M:
>   There are 2 philosophies of gradual typing, in the Siek/Taha style every
>   function application is a program boundary.  
> Will:
>   Still a small number of bits  
> M:
>   Still a few thousand  
> Will:
>   That's nothing. The size you need is logarithmic in the number of boundaries.  
> Will:
>   Here's what I'd do. Instead of representing all values by 64 bits, I'd just
>   use 50 bits and keep the rest for tracking this stuff. Just keep enough extra
>   bits to have overflow space in case there's dynamic linking. Shouldn't be
>   hard to accomodate the space you need **in practice**.  
> M:
>   Okay cool, it's just a matter of implementation.
>   Your dissertation is spelled out.
