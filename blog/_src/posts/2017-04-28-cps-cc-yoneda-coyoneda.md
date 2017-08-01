    Title: CPS is Yoneda, Closure-Conversion is Co-Yoneda
    Date: 2017-04-38T00:00:00
    Tags: yoneda, category theory, compilers, cps, duality, math, by Max New

The continuation-passing style transform and closure conversion are
two techniques widely employed by compilers for functional languages,
and have been studied extensively in compiler correctness literature.

Interestingly, *typed* versions of each can be proven to be
equivalence preserving using polymorphic types and parametric
reasoning, as shown by my advisor Amal Ahmed and Matthias Blume
([1][polycps],[2][polycc]).

In fact there is something like a duality between the two proofs, cps
uses a universal type, closure-conversion uses an existential type and
the isomorphism proofs use analogous reasoning.

In this post, I'll show that both are instances of general theorems in
category theory: the polymorphic cps isomorphism can be proven using
the Yoneda lemma, and the polymorphic closure-conversion isomorphism
can be proven using the less well known *co*Yoneda lemma.

<!-- more -->

I'll assume some background knowledge on cps and closure conversion
below. Fortunately, Matt Might has written nice blog posts explaining
both: [3][mmcps], [4][mmcc].

# Polymorphic CPS

The continuation-passing style transform makes all control-transfer
explicit and all calls into tail-calls by making the continuation of a
computation into a parameter that is explicitly invoked.

To do this, expressions are compiled to take an explicit continuation,
so for instance a value like \\( true\\) is compiled to a program \\(
k true \\) where \\( k \\) is the continuation coming from the context
of the value.

How could we give types to this translation? Well, let's ask a type
inferrer, GHCI:

	GHCI> :t \k -> k True
	\k -> k True :: (Bool -> a) -> a

that \\(a \\) is a polymorphic type variable, it says that this
translation of \\( True \\) can be typed for *any* return type for the
continuation.

It turns out that for a simple lambda calculus (no call/cc, etc) that
all of cps can by typed polymorphically in this way because all
continuations are used with a straightfoward protocol: every
continuation is threaded to exactly one spot in the term.



It also enables *new* control-flow capabilities in the form of control
operators like call/cc.

However allowing new control structures in our target language means
*breaking* source language reasoning.

It turns out that the "bad" programs are the ones that know too much
about the continuations, they duplicate or drop them.

One way to ensure only the well-bracketed functions are allowed is to
use the types:

The control-introducing cps transform can be typed by fixing a single
global "answer type", usually called _|_ to denote the connection to
double-negation, but it can be any fixed type:

So expressions of type A in the source are transformed into
expressions of type [(A_v -> _|_) -> _|_], where _v says how values
are transformed. Then, if I know how to produce a _|_ (like if I have
a different continuation in scope), I can use that instead of the
continuation you gave me, and abort control from the intended return
point.

We can use polymorphism to prevent this. Remember that our choice of
answer type was arbitrary? We can build this into the type translation
by quantifying over _|_, instead of [(A_v -> _|_) -> _|_], we get
[forall _|_. (A_v -> _|_) -> _|_].

How do we know this doesn't allow any new programs?  Since cps is a
subset of ordinary lambda calculus, we can show this by showing that
the type translation constructs an isomorphic type, that is we can
define an isomorphism between A and [forall _|_. (A -> _|_) -> _|_]
(the A_v is taken care of by induction).

The construction is easy, there's no other way to write it in System
F. To construct a cps term, we just apply the continuation to the A we
have:

	i = λ (x : A). Λ _|_. λ (k : A -> _|_) . k x

And for the reverse, we just pick _|_ to be A itself, and pass the
identity function as the continuation!

	j = λ (p : [forall _|_. (A -> _|_) -> _|_]). Λ _|_. p(A)(λ(x:A) )

Now we need to show that this is an isomorphism. One direction is easy
(j o i = id). If we know we just apply the continuation to a value and
we pick the identity continuation, we certainly get back where we
started.

On the other hand, to show (i(j(p)) = p) for an arbitrary p, we need
to use parametricity. Doing some calculations,

	i(j(p)) =~ Λ_|_. λ (k : A -> _|_). k(p(A)(id_A)).

Doing an eta expansion we can see that it's sufficient to show for any
type _|_, and continuation k : A -> _|_ that

	p(_|_) k == k(p(A)(id_A))

My advisor calls this "continuation shuffling" and it's probably her
favorite example of a theorem proven by parametricity.

The proof proceeds as follows. Proving x = k y is the same as showing
x and y are in the "k" relation, that is if we view a function as a
relation so we need to show

	(p(_|_) k, p(A)(id_A)) ∈ k
	
since p is a parametric function we can instantiate it with a choice
of relation between _|_ and A, again we will choose the function k,
and then it is sufficient to show that

	(k, λ (x : A). x) ∈ A -> k
	
that is, when applied to the same argument x,

	(k x, x) ∈ k
	
which remember is the same thing as showing

	k x = k x
	
which holds syntactically.

# Polymorphic Closure Conversion

Closure conversion allows us to implement proper higher-order
functions in a language without them. To do so, you compile a
high-level function \\(A \to B \\) to a pair that includes the values
of all the free variables at its definition site and a function that
accepts those values as extra arguments. Something like \\( \Gamma
\times (\Gamma \times A \to B) \\) where \\(\Gamma \\) is all the free
variables of this function. Unfortunately we've lost some modularity
by making \\(\Gamma \\) explicit! For instance two functions that came
from different parts of a program will have very different \\(\Gamma
\\)s.

Fortunately, this difference is immaterial because the only purpose of
\\(\Gamma \\) is to be passed as an argument to the function. We can
capture this "doesn't matter" aspect using *existential* types, so we
get \\[ \exists \Gamma. \Gamma \times (\Gamma \times A \to B)\\]

Then we can ask the same question, are there more "explicit closures"
than there were functions in our original language?  The answer again
is no, and we can get some argument as to why by constructing an
isomorphism between these two types in a language like the existential
types language in TAPL.

\\[ cc : (A -> B) -> \exists \Gamma. (\Gamma \times (\Gamma \times A \to B ))\\]
\\[ cc = \lambda f. pack<1, (*, \lambda (_, x) f x)>\\]
\\[ un-cc : (\exists \Gamma. (\Gamma \times (\Gamma \times A \to B ))) -> (A -> B) \\]
\\[ un-cc = \lambda p. unpack <\Gamma, (env, f')> = p in \lambda x. f'(env,x) \\]

In fact, to highlight the duality with cps, I'll use a slightly
different form of closure-conversion, that I'll call "open closure
conversion".  If we think of our function as being embedded in a term
whose free variables are given by \\(\Delta \\), then the type of our
function can be thought of as \\((\Delta \times A) \to B\\). 

Again, one direction is easy, \\(un-cc(cc(f)) \\) is beta-eta
equivalent to \\(f \\).

# Yoneda and CoYoneda

The connection between the Yoneda lemma and continuation-passing style
is somewhat well-known, and a favorite topic of category bloggers
(TODO: cite the blogosphere).

One formulation of the Yoneda lemma most relevant to us is that if we
have a category C and a "copresheaf", that is a functor [Q : C ->
Set], then for any object A ∈ C, 
	
	Q(A) = Natural(Y^o(A), Q)

which using the language of *ends* we can rewrite as

	Q(A) = forall B. Hom_C(A,B) -> Q(B)
	
here I've written the end literally as a forall type, which is I think
a much better intuition for the concept than the normal integral
notation.

How do we get from there to correctness of polymorphic CPS? First, we
would like Hom_C to just be the function type, so we need to really
talk about *enriched* category theory. The idea of enriched category
is to replace "the universe" Set with some other sufficiently
structured category. In this case, We will replace Set with our
language's category C. Fortunately all of this Yoneda stuff works
perfectly well in the enriched setting. So now [Q : C -> C] is just a
functor and Hom_C is just the normal arrow type.

	Q(A) = forall B. (A -> B) -> Q(B)

Then if we just pick Q to be the identity functor, so that Q(X) = X,
we get *exactly* the polymorphic cps identity:

	A = forall B. (A -> B) -> B
	
Furthermore, the categorical argument actually works too. This is
because ends are defined to be universal "dinatural" transformations,
which are basically a definition of parametric function that only uses
equalities and functions as the relations. Since this is a subset of
parametric reasoning, all equalities using the dinatural setting
extend to the parametric setting. Furthermore you can see why it works
since the only non-identity relaiton we used was a function relation
k.

Now what about the CoYoneda lemma? While not as famous as its sister,
the Yoneda lemma, it is still quite useful (kind of like closure
conversion and CPS). For instance, it's central to the definition of
the "Day Convolution" which is a key component to modelling separation
logic and kripke models generally.

The form we'll use is that for any category C and a presheaf, that is a
functor [P : C^op -> Set], and any object A ∈ C,

	P(A) = ∃ B. Hom(B,A) × P(B)

(Note that though we used a presheaf here and a coprosheaf for the
Yoneda lemma, that is not the duality between Yoneda and
CoYoneda. There are Yoneda and CoYoneda lemmas for presheaves and
copresheaves, it just so happens that CPS involves a copresheaf and
closure conversion involves a presheaf).

Applying our enriching strategy, we replace Set with C and Hom with ->
and get:

	P(A) = ∃ B. (A -> B) × P(B)

Then we can fit our "massaged closure conversion" type into this
framework by setting P(Γ) = Γ × A -> B, giving us:

	(Γ × A) -> B = ∃ Δ. (Γ -> Δ) × ((Δ × A) -> B)

This gives us the normal closure conversion by setting Γ = 1 as
before.


A --cps-> (B -> _|_) -> (A -> _|_)
A -> B --pcps-> forall C. (B -> C) -> (A -> C)

A -> B --cc-> G x ((G x A) -> B)
A -> B --pcc-> exists G. G x ((G x A) -> B)

G x A -> B --coyo-cc-> exists D. (G -> D) x ((D x A) -> B)

[polycps]: http://www.ccs.neu.edu/home/amal/papers/epc.pdf
[polycc]:  http://www.ccs.neu.edu/home/amal/papers/tccpoe.pdf
