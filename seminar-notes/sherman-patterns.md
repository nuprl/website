(The notes are not self-contained, the slides would help a lot to follow them.
It is reasonable to assume that the slides will eventually appear on Ben's
webpage at http://www.ben-sherman.net/talks.html ).

----

# Title: Overlapping pattern matching for programming with continuous functions
## Speaker: Ben Sherman

For real-numbers arithmetic we use floating point in our software. It
works very well in practice but it's very difficult to get formal
guarantees that the things we compute correspond to the mathematical
operations you may think about --- rounding for example makes it very
delicate.

There is a very interesting field of mathematics called formal
topology that takes about how to compute with numbers in a way that
corresponds to their abstract definition.

I think formal topology can be very interesting for computer science
applications. How do we compute with it and reason about those
programs?

In usual (discrete) programming there is a notion of pattern-matching
that is used to make case distinctions. We will show how to extend
this construction in our framework of topological computations.

Booleans are a discrete space: there are two clearly separated values,
`true` and `false`. If you look at a closed interval on the real line,
or at a circle, it is less clear what its points are, but we can
compute arbitrarily precise approximations of them.

Formal distribution is going to tell us the common structure about
discrete spaces, the real line or intervals, circles, distributions,
or compact spaces.

(Compact spaces can be enumerated/exhausted in finite time.)

The idea of how to compute in a topological space is as follows: I have
a notion of "cover" that correspond to questions I can ask to learn
information about my points. Given an "open cover", I can ask in which
of several possible regions a point lies in. But if two regions
overlap, and the point is in the intersection, I may get either
answer --- there is a form of non-determinism. I can get better
approximation by asking about more and more fine-grained open
covers. This is how we compute with spaces.

Functions reduce questions about the output into a question about the
input. We have a big function that computes a big number. I can ask
the question to the output: "what's a rational approximation of you
within error bound 0.1?". The function may be defined as a composition
of smaller computations, it will ask each sub-computation a question
in turn, eventually asking the inputs of the function, and then build
up an answer to my questions from these sub-answers.

Overlapping patterns:

"positive — within approximation ε"

```
pos?{ε} : R -> Bool  (it's a continuous function)

pos?{ε}(x) = case x of
| ι[̧· < ε](_) => {false}
| ι[· > -ε](_) => {true)
```

We are asking if the value is positive, but in fact if it is
between -ε and +ε, we may get either outputs: there is non-determinism
here.

We can use pattern-matching to do some useful tasks in an approximate
kind of way. For example we can compute the root of a (continuous)
function on the interval [0,1] within approximation ε.

Demos of game-like understanding of approximate computations
by question/answer series:

if I ask negate(sqrt(2)) a rational value within 0.1, it asks
sqrt(2) a rational value within 0.1, then negate the returned value.

if I ask 10*(sqrt(2)) a rational value within 0.1, it needs to ask
sqrt(2) a value within 0.01.

If we have two subcomputations, (pi + sqrt(2)) with (+ : R * R -> R),
a single question generates two sub-questions. We can have finitely
many sub-questions. (+ gets to decide how to split the precision
request: within 0.1, it can ask each argument within 0.05, or do
a different split; all choices will give a consistent answer in
a certain sense).

If I ask several questions to the same value, I want to be able to
combine the answers to get more information. If I ask at three
different precisions, I will in fact get three different intervals,
I want to know that the point lies in the intersection of the three
intervals.

Of course, this intersection has to be non-empty: we want to be
careful about how we define "spaces" to make sure that such an
inconsistency cannot occur.

How do we define spaces? We start by picking a fundamental open
property, the one thing that we can ask about the points in our
space. Typically questions are formal balls (for metric spaces,
open balls); for real numbers, those are intervals of positive
rational size.

B{ε}(q)

(with q ∈ Q, ε ∈ Q⁺)
(one interpretation of this formal ball is a set of reals
within strict ε-distance of the rational q)

We have reasoning rules on these formal balls:

B{ε}(q) : Open(R)

T ≤ ⋁{q:Q} B{ε}(q)

B{ε}(q) ≤ ⋁{B{δ}(q') | B{δ}(q') < B{ε}(q)}

the last rule (I get to choose δ whenever I want to apply the rule)
can be read geometrically and computationally:

geometry: if I take all the balls that are strictly included in my
formal ball, and union them, I get exactly the formal ball
I started with.

computationally: if I know a point lies within ε of q, I can ask for
a refined δ-approximation of a better q' in this part of the space.

To understand derivations in this system computationally, we can look
at the interplay between topological constructs (big unions) and
logical connectives (existential quantification, interpreted in
a constructive way). (Gabriel: there was a specific example of this on
a slide before 22, but it was a bit too much work to take note of it
in whole.)

If I get two inconsistent answers (1.0 within 0.1, and 1.20 within
0.05), I know that I lie in the intersection
B{0.1}(0.1) ∧ B{0.05}(1.20)
and I know that I ask for a cover of it
B{0.1}(0.1) ∧ B{0.05}(1.20) ≤ ⋁{}
contradiction!

This was the informal intuition for what a point is – along with
a formal definition of open covers. Let's now see the formal
definition of points.

```
(x ⊨ ·) : Open(A) → Prop
```

a point is question-asking device that takes opens and answer
a proposition (think of Coq's Prop universe).

```
x ⊨ P    P ≤ ⋁{i:I} Q{i}
————————————————————————
∃i:I, x ⊨ Q{i}

—————
x ⊨ ⊤

x ⊨ P   x ⊨ Q
—————————————
x ⊨ P ∧ Q
```

Note that we may forget about the middle rule, to get a notion of
partial space (we don't know of a starting question to ask
to points). If we lose the third rule, we get nondeterminism.


> Gabriel: are these rules "complete" in a certain sense? (Do you have
> a good reason to believe that these rules are "enough"?)

> Ben: they correspond exactly to the canonical definition of formal
> topology, so yes.

> Will: but note that the axioms classical topology are known to be
> incomplete, so in this sense of "enough" those would probably be
> incomplete as well.


Continuous maps, formally: if (f : A → B) is a (continuous) function,
we have (f* : Open(B) → Open(A)) which preserves arbitrary unions,
top, and finite intersections.

Using this, we get a programming language for continuous spaces. We
can see spaces as types, points as values, and continuous maps as
functions.

(You may ask: why wouldn't you allow non-continuous functions defined
by mapping from points to points? But in formal topology, you are not
losing anything by imposing the continuous restriction: we live in
a world were everything is continuous, and that gives us extra
computational powers of approximation!)

We can define sum spaces:

```
inl : A ↪ A + B
inr : B ↪ A + B
```

(note that (↪) functions are "embeddings" in a sense, which makes them
nice, just as injective constructors in everyday programming).

```
forget_sign(x) : ...

case (x) of
| ι[· > -ε] → x
| ι[· < ε] → -x
```

But there is an issue with overlapping: where an answer is
non-deterministic, we can use the conjunction rule P ∧ Q to learn both
answers at once, which would be inconsistent:

```
pos?{ε} : R -> Bool
pos?{ε}(x) = case x of
| ι[̧· < ε](_) => false
| ι[· > -ε](_) => true
```

This is wrong! If I lie in [-ε; ε] I get can get both answers, and
learn that my point lies *nowhere* (in ⊥).

To fix this, we have to change the return type to include
a non-determinism effect:

```
pos?{ε} : R -> P✧⁺(Bool)
pos?{ε}(x) = case x of
| ι[̧· < ε](_) => {false}
| ι[· > -ε](_) => {true}
```

With this type, if I learn both {false} and {true}, this is just fine,
I have a point in P✧⁺(Bool) that is consistent with these two answers,
it is {true, false}.

Overlapping pattern matching:

1. Coverings: all cases together must cover the entire space
2. Gluing: because there are overlaps, the result space must have unions

(Gluing doesn't necessarily mean that the output type must be a `P✧⁺(_)`
space. Other spaces have unions.)


> Max: so do you define your nondeterminism spaces as having the same
> points, but different opens?

> Ben: no in fact formally we do a different definition; don't take
> what's one the slides literally, it's there for intuition.


When we have a more complicated pattern, we have to prove that
covering holds somehow, and this is exactly what gives us the
computational content.

Lifted spaces A⊥: it has the same opens as A, plus an extra point ⊥.

```
up : A ↪ A⊥
bot : A⊥
```

> Max: so ⊥ is not an open, so you can observe termination, but not
> non-termination?

> Ben: exactly.

If you have f : A →ᶜ B, you can define (f⊥ : A⊥ →ᶜ B⊥)

```
f⊥(x) = case(x) of
| up(z) => up(f(z))
| _ => ⊥
```

Another example: partial booleans.

```
or : Bool⊥ * Bool⊥ →ᶜ Bool⊥

or(p) = case(p) of
| up(true), _ => up(true)
| _, up(true) => up(true)
| up(false), up(false) => up(false)
```

If the first one terminates and it's true, no need to look at the
second argument. Note that we miss some cases: implicitly in those
cases we just return ⊥.



> Mitch: what advantage does this give you over just representing the
> reals as, say, Cauchy sequences?

> Ben: the main advantage that I could say in general would be... If we
> look back to the very beginning with my example of approximate root
> finding. We can define the Real numbers as Cauchy sequences, you have
> to define a sort of continuity property that has to be computational,
> that depends on how the computation works. In constructive maths, Eric
> Bishop did essentially real numbers in term of Cauchy sequences. Then
> he had this issue with defining what continuity meant for those, if
> you want nice theorems to hold (Heine-Borel for example), you define
> a function to be continuous if it's pointwise-continuous, but then you
> loose the nice properties. So Bishop defines continuity as being
> uniformly continuous on every compact subspace. But that doesn't play
> well with taking subspaces, the composition of functions is no longer
> continuous. If you compose two functions that factor through the
> positive real numbers, it breaks down. On the contrary, formal
> topology gives you the correct notion of continuity.


> Gabriel: I am not convinced by the idea that the computational
> semantics of your language is *given* by the formal topology. I would
> rather have in mind a real-world implementation that does thing as we
> would expect, and then it is proved consistent with respect to the
> formal topology reading of things.

> Ben: certainly there are cases where it's hard to reason about those
> formal topology. Is there some other dynamic semantics that we can
> have that would implement this?

I can go in a little bit of an example of where it can be potentially
very inefficient, but there may be a way to fix it. If I have a stream
of numbers defined as, with (c < 1):

```
x{n} = c*x{n-1} + y{n}
```

if I want to approximate this to ε. If I have the naive implementation
of (+), it's going to approximate both to ε/2. Every time we get a new
input, we ask more and more precise questions to x₀. This is really
problematic. But in fact you could do some of continuous math where
you don't split approximation equally. If (c > 1), this is not stable,
you have to do this. But if (c < 1), you should be able to not refine
approximations of x₀.


> Gabriel: can you see what you do as a way to prove that programs,
> written in a normal programming language with extra annotations, are
> correct in how they handle approximations?

> Ben: for instance, with real numbers, we are never really sure how
> precise our answers are. Taking a function (A →ᶜ B). If I know that my
> inputs come from a compact space A, then I know that B is also
> compact. If I ask a question, there are only finitely many
> answers. Regardless of my inputs from A, I can uniformly approximate
> them all. There is this question of, when I do this, and then I ask
> a question which necessarily that has finitely many answers, it
> reduces to a question on A with finitely many answers, then I can
> reduce this to a mapping from answers to answers, there may be an
> efficient implementation.


> Michael Bukatin: For real-time systems we need formal correctness guarantees, but
> also the guarantee that it runs in some bounded time. Can you give
> some runtime guarantees?

> Ben: in formal topology you don't have any runtime bounds. If you want
> to apply it to a cyber-physical system, you need runtime bounds. But
> note that we do a lot of other "bad" assumptions; we use rational
> numbers, we assume that there is some sort of garbage collection.


> Dan: is there a notion of modulus of continuity in formal topology?

> Ben: so this would be for metric spaces only? There is nothing
> inherent in formal topology but you can reason about, for example,
> Lipschitz-continuity, on top of it.


> Dan Huang: can you comment on the relation with synthetic topology?

> Ben: Synthetic topology is when you essentially use something like
> Coq, working in a model where types are interpreted as topological
> spaces, plus a few axioms that are admissible in this model. So what
> is the difference with what I present here?
> 
> In all synthetic topology models I have seen, spaces have to be
> defined by their points. There is a technical distinction in formal
> topology, only some spaces have so-called spatial locales, and they
> correspond to Sober spaces in classical topology. I think that
> synthetic topology forces you to live in the intersection of these
> worlds, in only Sober spaces (from a classical point of view) or
> spatial locales (from a formal point of view). We can do more, we can
> work in spaces with non-spatial locales, that have no known points:
> all points are known to not occur. This is interesting to reason, for
> example, about probabilistic spaces: if I get a sequence of random
> point flips, I can work on the generic random point, that is known to
> be different from any given specific sequence of coin flips, that is
> useful.


> Mitch: you began the talk by saying that we would start with formal
> topology. How much of what you have told us today is existing formal
> topology, and what have you added to the story?

> Ben: the notion of overlapping pattern-matching, and giving this
> programming-language idea and investigating how it works, this is our
> idea and contribution. The formal topology part is known and
> well-understood. Our contribution would be the "overlapping" aspect of
> pattern-matching in our setting, as a language design.


> Mitch: so maybe you are coming up with a new interpretation of the
> computational content of formal topology.


> Justin: as a relative beginner in topology, there are a lots of
> topological spaces that you could construct on any given set of
> points, how do you know which ones are relevant to computation?

> Ben: that's a difference between formal and classical topology. In
> formal topology, you start from the questions that you can ask, and
> that uniquely determines the points that you can have. If you think of
> a set that has two topologies on it (Justin: rationals with distance
> metric vs. discrete topology).... I don't really understand how you
> would represent the rationals with the metric topology, I don't know
> exactly how you'd try to do it, you may end up with more things in
> there, for example having the real numbers.


> Will: two things. (1) How do you guarantee that things glue together
> properly? When two things overlap, don't two functions have to glue
> together when you overlap? And (2) what about efficiency?

> Michael: consistency of gluings is the theory of sheaves,
> mathematicians care very much about it.
