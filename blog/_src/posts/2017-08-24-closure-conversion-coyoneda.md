    Title: Closure Conversion as CoYoneda
    Date: 2017-08-24T00:00:00
    Tags: yoneda, coyoneda, category theory, compilers, closure conversion, math, by Max New

The continuation-passing style transform and closure conversion are
two techniques widely employed by compilers for functional languages,
and have been studied extensively in compiler correctness literature.
Interestingly, *typed* versions of each can be proven to be
equivalence preserving using polymorphic types and parametric
reasoning, as shown by my advisor Amal Ahmed and Matthias Blume
([cps][polycps],[cc][polycc]).

In fact, there is something like a duality between the two proofs, cps
uses a universal type, closure-conversion uses an existential type and
the isomorphism proofs use analogous reasoning.
It turns out that both are instances of general theorems in category
theory: the polymorphic cps isomorphism can be proven using the Yoneda
lemma, and the polymorphic closure-conversion isomorphism can be
proven using a less well known theorem often called the [*co*Yoneda lemma][coyoneda].

The connection between cps and the Yoneda embedding/lemma is detailed
elsewhere in the [literature][isomorphically] and blogosphere
([ncafe][ncafe], [Bartosz][bartosz]), so I'll focus on closure
conversion here.
Also, I'll try to go into a little more detail than there in showing
how to get from the "usual" version of Yoneda/Coyoneda (using the
category of sets) to the appropriate version for compilers.

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
let x = 3 in ([x: 3], \lambda env, y. let x = env.x in x + y) 
```

(here we could obviously substitute `x` in but closures are
needed when `x` is a dynamic input).
Can we give a type to the resulting code?  The source program had type
`Number -> Number`, but the target had a type more like

```
{ x: Number} \times ({x : Number} \times Number -> Number).
```

In addition to being ugly, this type is leaking irrelevant details of
the function's implementation: all of its free variables are there in
its type, so two terms with the same function type but different free
variables would be translated to different types.
Also high-level program equivalences like \\(\beta\\)-reducing the term
to just `λ y. 3 + y` would not even preserve typing.
Not only that, but some bad code could now supply a *different* value
for `x` than allowed which could break invariants the programmer
had about the function.

We could fix the type preservation issue by just using a dynamic type
for our environment, but this would still leak details in the values.
Fortunately, there is a nice solution to the other problems using more
advanced types: existential types.
The idea is that the type of the environment of free variables is
*irrelevant* to anyone that calls the function, only the function
itself should know what the environment looks like: the type of the
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
let x = 3 in λ y. x + y ≡ λ y. 3 + y
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

The coYoneda lemma says that for any category \\( C \\), *presheaf*,
i.e. a functor \\( Q : C^{op} \to \Set \\), and object \\(A \in C \\),
\\(Q(A) \\) is isomorphic to:
\\[ \exists B. (A \to B) \times Q(B) \\]
usually coends use an integral sign but I think \\(\exists \\) is a
much more evocative notation that is used sometimes in logicy circles.

A coend is a construction that is very similar to the parametric
existential quantifier.
I don't think I've ever seen this written down explicitly, but it's
basically what you get when you take the parametricity condition for
the existential type but restrict your reasoning to only use
functional relations.

A presheaf is just a functor \\( Q : C^{op} \to \Set \\).  One nice
way to think about it is that \\( Q \\) is a sort of "generalized
object" of \\( C \\), in that it has a notion of map from object \\( B
\in C\\) to \\( Q \\): we think of \\(Q(B) \\) as "\\( B \to Q
\\)". The fact that \\( Q \\) is a functor means we can compose a
"map" "\\( q : B \to Q \\)" (really an element \\(q \in Q(B)\\)) with
real maps \\( f : A \to B \\) in \\( C\\) to get a map \\( q \circ f :
A \to Q \\) and this behaves as expected with respect to identity and
composition.  Then the Yoneda lemma says that any object is a
"generalized object" by taking the real maps into it and that this
interpretation doesn't add any new maps between the original objects.

So the informal explanation of the coyoneda lemma is that if we have a
\\( \exists B. (A \to B) \times Q(B) \\) then we can get a \\( f : A
\to B \\) and a "\\(q : B \to Q\\)" and we can't inspect \\(B \\) in
any way, then all we can really do is to compose the two maps.

But there's a gap from here to applying this to a programming
language, \\(Q(A) \\) and \\(\exists B. (A \to B) \times Q(B) \\) as
described so far are *sets*, but we wanted an isomorphism of *types*
in our programming language.

We can reconcile this by considering *enriched* category theory.
Let \\(V \\) be a category, then if \\( V \\) is sufficiently like the
category of sets, then we can do a lot of category theory by replacing
the word "set" with "object of \\(V \\)".
Specifically, we say a \\(V \\)-enriched category still has a set of
objects \\(Ob \\), but for each pair of objects \\(A,B \in Ob \\) we
get a \\( V\\)-object \\(\Hom(A,B) \\) of morphisms from \\(A \\) to
\\( B \\).
If \\(V \\) is a closed category, we can see \\(V \\) *itself* as a
\\(V\\)-enriched category with the same objects and just making
\\(\Hom(A,B) = A \to B \\).

Then we can reinterpret the coyoneda lemma above by saying \\(V\\)
category instead of category and \\(V\\)-valued presheaf instead of
presheaf. If we fix the \\(V\\)-category to be \\(V \\) itself, then a
\\(V\\)-presheaf is just a contravariant functor from \\(V \\) to
itself: \\(Q : V^{op} \to V\\).
Haskelletons just call this
a
[contravariant functor](https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant.html).
Furthermore, since existential types provide at least as strong of a
reasoning principle as coends, the proof of the coyoneda lemma goes
through with existential types instead.

Finally, to be explicit let's look at what the isomorphism looks like
in Haskellish/Agdaish syntax.
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
coend/parametric reasoning.
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
functor \\(Q : V^{op} \to V \\),

\\[ Q(\Delta) \cong \exists \Gamma. (\Delta \to \Gamma) \times Q(\Gamma)
\\]

Clearly based on the right hand side, \\(Q \\) should be \\( - \times
A \to B \\) which gives us:

\\[ \Delta \times A \to B \cong \exists \Gamma. (\Delta \to \Gamma) \times (\Gamma \times A \to B)
\\]

and if we pick \\(\Delta = 1\\), the unit type and the fact that \\(1
\times A \cong A \\) and \\(1 \to \Gamma \cong \Gamma\\) (at least in
a pure language) then we get the desired result by composition:

\\[ A \to B \cong 1 \times A \to B \cong \exists \Gamma. (1 \to
\Gamma) \times (\Gamma \times A \to B) \cong \exists \Gamma. \Gamma
\times (\Gamma \times A \to B)\\]

### You might also like

- [Syntactic Parametricity Strikes Again](http://prl.ccs.neu.edu/blog/2017/06/05/syntactic-parametricity-strikes-again/)

- [Categorical Semantics for Dynamically Typed Programming
  Languages](http://prl.ccs.neu.edu/blog/2017/05/01/categorical-semantics-for-dynamically-typed-programming-languages/)

- [Understanding Constructive Galois
  Connections](http://prl.ccs.neu.edu/blog/2016/11/16/understanding-constructive-galois-connections/).

[polycps]: http://www.ccs.neu.edu/home/amal/papers/epc.pdf
[polycc]:  http://www.ccs.neu.edu/home/amal/papers/tccpoe.pdf
[coyoneda]: https://ncatlab.org/nlab/show/co-Yoneda+lemma

[ncafe]: https://golem.ph.utexas.edu/category/2008/01/the_continuation_passing_trans.html
[bartosz]: https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/
[isomorphically]: http://www.cs.ox.ac.uk/people/daniel.james/iso/iso.pdf
[mmcc]: http://matt.might.net/articles/closure-conversion/
