    Title: Closure Conversion as CoYoneda
    Date: 2017-08-28T10:30:00
    Tags: Yoneda, coYoneda, category theory, compilers, closure conversion, math, by Max New

The continuation-passing style transform (cps) and closure conversion
(cc) are two techniques widely employed by compilers for functional
languages, and have been studied extensively in the compiler correctness
literature.  Interestingly, *typed* versions of each can be proven to
be equivalence preserving using polymorphic types and parametric
reasoning, as shown by my advisor Amal Ahmed and Matthias Blume
([cps][polycps],[cc][polycc]).

In fact, there is something like a duality between the two proofs, cps
uses a universal type, closure-conversion uses an existential type and
the isomorphism proofs use analogous reasoning.
It turns out that both are instances of general theorems in category
theory: the polymorphic cps isomorphism can be proven using the Yoneda
lemma, and the polymorphic closure-conversion isomorphism can be
proven using a less well known theorem often called the [*co*Yoneda lemma][coYoneda].

The connection between cps and the Yoneda embedding/lemma is detailed
elsewhere in the [literature][isomorphically] and blogosphere
([ncafe][ncafe], [Bartosz][bartosz]), so I'll focus on closure
conversion here.
Also, I'll try to go into some detail in showing how the "usual"
version of Yoneda/coYoneda (using the category of sets) relates to the
appropriate version for compilers.

I'll assume some background knowledge on closure conversion and
parametricity below.
Fortunately, Matt Might has
a [nice blog post explaining untyped closure conversion][mmcc].

<!-- more -->
\\(
\newcommand{\Set}{\mathsf{Set}}
\newcommand{\Hom}{\mathsf{Hom}}
\\)

## Polymorphic Closure Conversion

Closure conversion is a way of compiling a language with closures
(i.e., basically any modern high-level language) to one that only has
function pointers/labels like C or machine code.
Closure conversion compiles high-level functions (aka closures) to a
pair of an environment that will contain the values of all the
functions' free variables and a code pointer to a block that takes as
inputs all the inputs to the function and values for all of the free
variables.

For instance

```
let x = 3 in λ y. x + y
```

would be converted to something like

``` 
let x = 3 in ([x: 3], λ env, y. let x = env.x in x + y)
```

Can we give a type to the resulting code?  The source program has type
`Number -> Number`, but the target has a type more like

```
{ x: Number} × ({x : Number} × Number -> Number).
```

In addition to being ugly, this type is leaking irrelevant details of
the function's implementation: all of its free variables are there in
its type, so two terms with the same function type but different free
variables would be translated to different types.
Also high-level program equivalences like \\(\beta\\)-reducing the term
to just `λ y. 3 + y` would not even preserve typing.
Not only that, but some bad code could now supply a *different*,
well-typed value for `x` than allowed which could break invariants the
programmer had about the function.

We could fix the type preservation issue by just using a dynamic type
for our environment, but this would still leak details in the values.
Fortunately, there is a nice solution to the other problems using
existential types.
The idea is that the type of the environment of free variables is
*irrelevant* to anyone that calls the function, only the function
itself should know what the environment looks like; the type of the
environment should be *abstract* to the caller and *concrete* to the
callee.
Existential types capture this.

We can translate functions in the source of type `A -> B` to
pairs of an environment and a code pointer, but now making the
environment type existentially quantified:

```
∃ Γ. Γ × (Γ × A -> B).
```
Then the syntax of existential types ensure that all any consumer can
do with the `env : Γ` in the pair is pass it to the code
pointer with an `A` argument.

How do we prove that this is correct? And what does correct even mean?
We'll focus on a property called *full abstraction* which says that if
two programs are equal in the source language, then their translations
are equal.
Here, equal in the source language will just mean \\(\beta,\eta \\)
equivalence, so things like as above:

```
let x = 3 in λ y. x + y
≡
λ y. 3 + y
```

To prove this we'll show that in a language with existential types the
types `∃ Γ. Γ × (Γ × A -> B)` and `A \to B` are isomorphic.
The usual proof is by parametricity, instead we'll use a closely
related category-theoretic argument: the coYoneda lemma.

## The CoYoneda Lemma

The coYoneda lemma is a generalization of the equivalence described
above.
I'll start with the ordinary version which uses *coends* and
*presheaves*.

The coYoneda lemma says that for any category \\( C \\), presheaf
\\( Q : C^{op} \to \Set \\), and object \\(A \in C \\),
\\(Q(A) \\) is isomorphic to the coend:
\\[ \exists B. (A \to B) \times Q(B) \\]
Let's break that down.

### Coends

A coend is a construction that is very similar to the parametric
existential quantifier.
If you're familiar with parametricity, a good intuition is that coends
have the same definition as existential types but where the only
relations are functional relations.

You can take the coend of a functor of type \\(M : C^{op} \times C \to
\Set \\).
We can get such an \\(M \\) from a type with a free type variable like
\\( X \times A \to X \\) by splitting the \\(X \\) into positive and
negative occurrences: \\(X^- \times A \to X^+ \\).
Then the coend \\(\exists X. M(X,X) \in \Set \\) is like the union of
all \\(M(X,X) \\), but where the \\(X \\) is ensured to be
"irrelevant".

So for any object \\(A \in C \\) there is a map \\(pack_A : M(A,A) \to
\exists X. M(X,X) \\), we can "hide the A".
To make sure the \\(X \\) is treated opaquely, we add an invariance
condition that says if you have an \\(mA : M(A,A) \\) and an \\(mB :
M(B,B) \\) such that the \\(A, B\\) positions are related by some
function \\(f : A \to B \\), then \\(pack_A(mA) = pack_B(mB)\\).
More formally, this means that if you have a \\(m' : M(B,A) \\), then

\\[ pack_B(M(B,f)(m')) = pack_A(M(f,A)(m'))\\]
or in a point-free style:
\\[ pack_B \circ M(B,f) = pack_A \circ M(f,A) : M(B,A) \to \exists X. M(X,X) \\]

A function parameterized by types like \\(pack \\) that has this
property is called a *co-wedge* from \\(M \\).

A coend is an object \\(\exists X. M(X,X) \\) and a co-wedge \\(\forall
A. pack_A : M(A,A) \to \exists X. M(X,X) \\) that are *universal*,
i.e. any other co-wedge \\(\forall A. f_A : M(A,A) \to C\\) factors through
\\(pack_A \\). This gives us the syntax for existential elimination.

If you are familiar with parametricity, it is a good exercise to see
why the usual condition for invariance wrt all *relations* implies
that a parametric \\(pack, \exists X. M(X,X) \\) will form a cowedge.
It seems that in general it would not be a universal co-wedge because
a parametric exists is invariant under all relations and there are
many relations that don't act like functions.

### Presheaves

Next, a presheaf is just a functor \\( Q : C^{op} \to
\Set \\).
Think of this as a set that is parameterised by a type of "inputs", so
if you have a map in \\(C, f : A \to B\\) you get a function \\(Q(f) :
Q(B) \to Q(A) \\) that "preprocesses" the inputs using \\(f
\\). Functoriality ensures that preprocessing with the identity is
just the identity and that composition of preprocessers is the
preprocessor from the composite function.

So the informal explanation of the coYoneda lemma is that for any
presheaf \\(Q \\), if we have an \\( \exists X. (A \to X) \times Q(X)
\\), then since we can't inspect the \\(X \\) in any way, all we can
really do is compose the \\(Q(X) \\) with the preprocesser from the
function \\(A \to X \\), giving us a \\(Q(A) \\).

### Enriched Categories and Enriched CoYoneda
But there's a gap from here to applying this to a programming
language, the coYoneda lemma as presented says that \\(Q(A) \\) and
\\(\exists B. (A \to B) \times Q(B) \\) are isomorphic as *sets*, but we
wanted an isomorphism of *types* in our programming language.  We can
reconcile this by considering *enriched* category theory and the
*enriched* coYoneda lemma.  Let \\(V \\) be a category, then if \\( V
\\) is sufficiently like the category of sets, then we can do a lot of
category theory by replacing the word "set" with "object of \\(V \\)".

Specifically, a \\(V \\)-enriched category (or just \\(V\\)-category)
has a set of objects \\(Ob \\), but for each pair of objects \\(A,B
\in Ob \\) we get a \\( V\\)-object \\(\Hom(A,B) \\) of morphisms from
\\(A \\) to \\( B \\).  If \\(V \\) is a closed category, we can see
\\(V \\) *itself* as a \\(V\\)-enriched category with the same objects
and just making \\(\Hom(A,B) = A \to B \\) i.e. the *internal* hom aka
exponential.

Then we can reinterpret the coYoneda lemma above by saying \\(C \\) is
a \\(V\\)-category and \\(Q \\) is a \\(V\\)-presheaf i.e., just a
contravariant functor from \\(V \\) to itself: \\(Q : V^{op} \to V\\)
where the preprocessing function is now a morphism in \\(C \\).
Haskelletons just call this
a
[contravariant functor](https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant.html).
Furthermore, since existential types provide at least as strong of a
reasoning principle as coends, the proof of the coYoneda lemma goes
through with existential types instead.
Finally, the point-free description above for coend can be interpreted
in any category.

Now that we're working all inside our language, let's look at what the
isomorphism looks like in Haskellish/Agdaish syntax.
We want mutually inverse functions

```
f : (Contravariant Q) => (∃ Γ. (Δ -> Γ) × (Q Γ)) -> Q Δ
g : (Contravariant Q) => Q Δ -> ∃ Γ. (Δ -> Γ) × (Q Γ)
```

If you try to implement them you won't be able to get it wrong, but here they are:

```
f (k, qΓ) = contramap k qΓ
g qΔ = (id, qΔ)
```

where we just instantiate \\(\Gamma = \Delta \\) in the second case.
You can prove \\( f \circ g = id \\) using just \\(\beta \\) and the
Contravariant laws, but to prove \\(g \circ f = id \\) you need to use
the coend reasoning.
For those of you that know about the Yoneda lemma, note the similarity
to that proof in using the identity function and instantiating a type
variable in a trivial way.

## Closure Version as CoYoneda

Now it's time to bring it all together.
Let \\(V \\) be our programming language viewed as a category in the
usual way.

We want to prove the closure conversion isomorphism:

\\[ A \to B \cong \exists \Gamma. \Gamma \times (\Gamma \times A \to B)
\\]

using the \\(V \\)-coYoneda lemma which says for any contravariant
functor \\(Q : V^{op} \to V \\), and object \\(\Delta \in V\\)

\\[ Q(\Delta) \cong \exists \Gamma. (\Delta \to \Gamma) \times Q(\Gamma)
\\]

Clearly based on the right hand side, \\(Q \\) should be \\( - \times
A \to B \\) which gives us for any \\(\Delta \in V\\):

\\[ \Delta \times A \to B \cong \exists \Gamma. (\Delta \to \Gamma) \times (\Gamma \times A \to B)
\\]

Next we pick \\(\Delta = 1\\), the unit type.
Then we use some basic facts about the unit type: \\(1 \times A \cong
A \\) and \\(1 \to \Gamma \cong \Gamma\\) (at least in a pure
language) to get the desired result by composition:

\\[ A \to B \cong 1 \times A \to B \cong \exists \Gamma. (1 \to
\Gamma) \times (\Gamma \times A \to B) \cong \exists \Gamma. \Gamma
\times (\Gamma \times A \to B)\\]

## Conclusion

Since closure conversion is an instance of the CoYoneda lemma, this
might be a nice example to give intuition for CoYoneda for
programmers.
While not as famous as its cousin Yoneda, CoYoneda is used
in [Haskell][coyoneda-haskell] and is also central to
the [Day Convolution][day], which can be used to give semantics
to [separation logic](atkey-thesis).

Also in researching for this post, I was surprised at how little I
could find on the relationship between ends/coends and relational
parametricity.
This seems very unfortunate as it looks like we're reproving some of
the same theorems (Yoneda, coYoneda) using very similar, but
incompatible formalisms.

## You might also like

- [Syntactic Parametricity Strikes Again](http://prl.ccs.neu.edu/blog/2017/06/05/syntactic-parametricity-strikes-again/)

- [Categorical Semantics for Dynamically Typed Programming
  Languages](http://prl.ccs.neu.edu/blog/2017/05/01/categorical-semantics-for-dynamically-typed-programming-languages/)

- [Understanding Constructive Galois
  Connections](http://prl.ccs.neu.edu/blog/2016/11/16/understanding-constructive-galois-connections/).

[polycps]: http://www.ccs.neu.edu/home/amal/papers/epc.pdf
[polycc]:  http://www.ccs.neu.edu/home/amal/papers/tccpoe.pdf
[coYoneda]: https://ncatlab.org/nlab/show/co-Yoneda+lemma

[ncafe]: https://golem.ph.utexas.edu/category/2008/01/the_continuation_passing_trans.html
[bartosz]: https://bartoszmilewski.com/2015/09/01/the-Yoneda-lemma/
[isomorphically]: http://www.cs.ox.ac.uk/people/daniel.james/iso/iso.pdf
[mmcc]: http://matt.might.net/articles/closure-conversion/
[coyoneda-haskell]: https://hackage.haskell.org/package/kan-extensions-5.0.2/docs/Data-Functor-Coyoneda.html
[day]: https://ncatlab.org/nlab/show/Day+convolution
[atkey-thesis]: https://bentnib.org/thesis.pdf
