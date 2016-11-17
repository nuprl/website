    Title: Understanding Constructive Galois Connections
    Date: 2016-11-16T00:00:00
    Tags: icfp, galois connection, adjunction, category thoery, math

One of my favorite papers at ICFP 2016 (in lovely
[Nara, Japan](http://conf.researchr.org/home/icfp-2016)) was
[Constructive Galois Connections: Taming the Galois Connection Framework for Mechanized Metatheory](https://arxiv.org/abs/1511.06965)
by [David Darais](http://david.darais.com/) and
[David Van Horn](https://www.cs.umd.edu/~dvanhorn/).  The central
technical result is quite interesting, but a little intimidating, so
I'd like to share a "de-generalization" of the result that I found
helpful to understand.

<!-- more -->

# History
I won't go into much of the details of the paper, because I think it
is quite well written, but here's a short overview.
The paper is about how to do verified static analysis while taking
advantage of the calculational approach of
[Abstract Interpretation](http://www.di.ens.fr/~cousot/COUSOTpapers/Marktoberdorf98.shtml).
The problem is that the Galois connections people use for abstract
domains are not always computable.
Darais and Van Horn show however that there is a very useful class of
Galois connections that is computable, and they show how they can
exploit this to write verified static analyses that more closely
follow the "on-paper" proofs, and offload much of the details to the
proof assistant as mere calculation.

David Darais told me about these results when we were at POPL 2016 (in
less lovely but much more convenient
[St. Petersburg, Florida](http://conf.researchr.org/home/POPL-2016))
and in particular about the central theorem of the paper, which shows
that two different classes of Galois connections they define, "Kleisli"
and "Constructive" Galois connections, are actually constructively
equivalent.
I was really surprised by the result when he explained it to me, and
so I hoped to find if there was a known generalization of the result
for adjunctions of categories, rather than Galois connections of
posets.

Eventually, my usual trawling of
[Mathoverflow](http://mathoverflow.net/) and
[nlab](https://ncatlab.org/nlab/show/HomePage) led me to a
[not-quite generalization to categories](https://ncatlab.org/nlab/show/Cauchy+complete+category#InOrdinaryCatTheoryByProfunctors)
and interestingly a
[*de*-generalization to sets](http://mathoverflow.net/questions/222516/duality-between-compactness-and-hausdorffness/222524#222524)
that helped me immensely to understand the theorem.

Since I know that the original theorem is a bit technical, I'll
explain the de-generalization to sets here, which I hope will help to
understand their theorem.

# Functions and Relations

Let's start with the "Kleisli Arrows", which are monotone functions
\\(f : A \to P(B) \\) where \\(A,B \\) are posets and \\(P(B)\\)
represents the poset of downward-closed subsets of \\(B \\).

Now to "de-posetize" this, we'll take sets \\(X,Y \\) and let \\(P(Y) \\)
mean the powerset of \\(Y\\), that is the set of all subsets of
\\(Y \\). Then a function \\(f : X \to P(Y) \\) is actually exactly
the same thing as a relation \\(R \subset X \times Y \\). From \\(f :
X \to P(Y) \\) we can take \\(R = \{(x,y) \in X\times Y | y=f(x)\} \\)
and from \\(R\\) we can construct \\(f(x) = \{y \in Y | (x,y) \in R \}\\).

Furthermore, the "Kleisli composition" is the same as composition of
relations.
If \\(R \subset X \times Y \\) and \\(Q \subset Y \times Z
\\), then the composition is defined as
\\[ (R;Q) = \{(x,z) \in X \times Z | \exists y\in Y. (x,y) \in R \land (y,z) \in Q\}\\]

Then the next thing we need to understand is what is the
de-generalization of "Kleisli Galois connection"?
Well, Galois connections are an instance of what's called an
adjunction in category theory, which is usually formulated in terms of
categories, functors and natural transformations.
However, you can interpret the definition of adjunction in any
"universe" that acts like the universe of categories, functors and
natural transformations and it turns out we have such a universe.
The universe I'm talking about is called \\(\texttt{Rel}\\), and it consists of
sets, relations between sets and *inclusion of relations*, i.e. that
one relation is a subset of another.

Then what does it mean to have an adjunction between two relations
\\(R \subset X \times Y, Q \subset Y \times X\\)? Taking apart the
definition it just means

\begin{align}\tag{1}
  \Delta(X) \subset R;Q
\end{align}
\begin{align}\tag{2}
  Q;R \subset \Delta(Y)
\end{align}

where \\(\Delta \\) means the *diagonal*, or equality relation on the set:

\\[\Delta(X) = \{(x_1,x_2) \in X | x_1 = x_2 \} \\]

So we just need to unravel what (1) and (2) mean above. Unwinding
(1), we get that for any \\(x \in X\\), there exists a \\(y \in Y \\)
such that \\((x,y) \in R \\) and \\((y,x) \in Q\\). This tells us for
one that \\(R \\) is a "right-total" relation and \\(Q \\) is a
"left-total". Every \\(x \\) is related to some \\( y\\) by \\( R \\)
and \\( Q\\).

If we unwind (2), we get that for any \\(y,y' \in Y\\) if there's some
\\(x \in X \\) such that \\((x,y) \in R \\) and \\((y',x) \in Q \\)
then actually \\(y = y')\\). This one is a bit more mysterious, but
first, let's see what this tells us about the relationship between
\\(R\\) and \\(Q \\).

If \\((x,y) \in R \\), then by (1) there's some \\(y' \in Y\\) so that
\\((x,y') \in R \\) and \\((y',x) \in Q\\). Then, by (2) we know that
\\(y = y'\\), so we've shown that if \\((x,y) \in R \\) then \\((y,x)
\in Q\\). Then a completely symmetric argument shows that if \\((y,x)
\in Q \\) then \\((x,y)\in R\\)! So we've discovered that actually
\\(Q \\) is just the opposite relation of \\(R \\).

Then if we look at (2) again but replace the \\(Q\\)'s by flipped
\\(R\\)'s we get that for any \\(y,y' \in Y\\), if there's some \\(x
\in X\\) such that \\((x,y) \in R \\) and \\((x,y')\in R\\) then \\(y
= y'\\), which tells us that \\(R \\) is a partial function, i.e.,
that every \\(x \\) is related to at most one \\(y \\) by \\(R \\).

You may recognize it now, our \\(R \subset X \times Y \\) is just a
function, and saying \\(R, Q\\) are adjoint is exactly the same as
saying that \\(Q = R^{\text{op}}\\) and \\(R \\) is a function.
Adjunctions are so pervasive you saw them back in pre-algebra!

# Constructive Galois Connections

Back to constructive Galois connections, I hope if you read the paper
you can see that their theorem is a generalization of the above
argument, where instead of relations we have "monotone relations", i.e.,
downward-closed \\(R \subset A^{\text{op}} \times B \\). Then you can
interpret the definition of adjunction in that universe and get that
it's the same as a Kleisli Galois connection and that a similar
argument to that above argument shows that the "left adjoint" is
represented by a monotone function \\(f : A \to B \\):

\\[R = \{(x,y) | y \le f(x) \} \\]

Which shows that every Kleisli Galois connection is actually a
constructive Galois connection!
The details are in their paper, and I hope they are easier to follow
now.

In fact, we get a little extra from what's mentioned in their paper,
which is that the "right adjoint" is represented by \\(f \\) as well
but in the opposite way:

\\[Q = \{(y,x) | f(x) \le y \}\\]

# Category Theory Post Scriptum

If you're interested in Category theory, here's a more technical
addendum.

Remembering from Category Theory class, sets are just posets where
objects are only less than themselves and posets are (basically)
categories where there is at most 1 arrow between objects, so we might
naturally ask, does this theorem extend to categories?

Well, first we need a generalization from relations to downward-closed
relations to what are called
[distributors or profunctors](https://ncatlab.org/nlab/show/profunctor).
Then we can also generalize inclusion of relations to morphisms of
distributors and ask, is every left adjoint distributor represented by a functor?

The answer is, at least in full generality, no! For it to be true we
need a special property on the codomain of the left adjoint \\(R : C
\not\to D \\), which is called (for mind-boggling reasons)
[Cauchy completeness](https://ncatlab.org/nlab/show/Cauchy+complete+category#InOrdinaryCatTheoryByProfunctors).
Viewing sets and posets as special categories, it turns out that they
always have this property, and that's why the theorem worked out for
those adjunctions.
