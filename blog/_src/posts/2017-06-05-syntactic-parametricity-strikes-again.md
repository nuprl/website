    Title: Syntactic parametricity strikes again
    Date: 2017-06-05T14:27:44
    Tags: by Gabriel Scherer, by Li-Yao Xia

In this blog post, reporting on a collaboration with [Li-Yao
Xia](https://poisson.chat/), I will show an example of
how some results that we traditionally think of as arising from free
theorems / parametricity can be established in a purely "syntactic" way,
by looking at the structure of canonical derivations. More precisely,
I prove that
\\(
\newcommand{\List}[1]{\mathsf{List}~#1}
\newcommand{\Fin}[1]{\mathsf{Fin}~#1}
\newcommand{\Nat}[1]{\mathbb{N}}
\newcommand{\rule}[2]{\frac{\displaystyle \array{#1}}{\displaystyle #2}}
\newcommand{\judge}[2]{{#1} \vdash {#2}}
\newcommand{\emptyrule}[1]{\begin{array}{c}\\[-1em] #1 \end{array}}
  ∀α. \List α → \List \alpha
\\)
is isomorphic to
\\(
    Π(n:\Nat{}). \List{(\Fin{n})}
\\) where \\(\Fin{n}\\) is the type of integers smaller than \\(n\\),
corresponding to the set \\(\{0, 1, \dots, n-1\}\\).

<!-- more -->

Context: Last week I had the pleasure of visiting UPenn, where I had
many interesting discussions with various people. It was also an
occasion to temporarily resume a discussion/collaboration I have with
Li-Yao Xia, who is currently an intern there, and Jean-Philippe
Bernardy, about testing polymorphic programs and its relation to
canonical representations for System F.

During one of our blackboard discussion, Li-Yao and I did a manual
proof of a cool result: we proved a parametricity theorem for
\\(∀α. \List α → \List α\\) using syntactic methods, namely
proof search among canonical proofs. (This is an idea that I have been
playing with since the last year of my [PhD
thesis](http://www.ccs.neu.edu/home/gasche/phd_thesis/), where
I unsuccessfully tried to extend my work on canonical forms for the
simply-typed lambda-calculus to polymorphism. It is here worked out on
an specific example, but my end goal is to turn the manual reasoning
into an algorithm.)

You may wonder, first, why the isomorphism holds. The idea is that
a polymorphic function of type \\(\List α → \List α\\) cannot inspect
the elements of the input list; it can only use them in the resulting
list, possibly duplicating, reordering or dropping some elements. On
any input list of size \\(n\\), the behavior of the function can be
described by a list of indices in \\([0; n-1]\\). For example, if the
input \\([x, y, z]\\) (for some values of \\(x, y, z\\)) gives the
output \\([y, y, x]\\), then this relation will hold on *any* value of
\\(x, y, z\\), as the function cannot inspect their value or even test
them for equality. The behavior of this function on lists of size
3 can be fully described by the list of indices \\([1, 1, 0]\\). Its
whole behavior is then uniquely determined by one such list for each
possible size:

\\[
    ∀α. \List α → \List α  \quad≃\quad  Π(n:\Nat{}). \List{(\Fin n)}
\\]

The idea behind the "syntactic" (proof-theoretic?) proof method is the
following: the set of closed values at a type \\(A\\) is isomorphic to
the *search space* for canonical/normal derivations of
\\(\judge{}{A}\\). We have tools (in particular the notion of
*invertible* inference rules) to reason on those – in this post I will
only present the reasoning informally, but it can easily be made
formally precise.

We start by looking at the shape of the search space for

\\[
    \judge{}{∀α. \List α → \List α}
\\]
or, said otherwise, of the judgment
\\[
    \judge{}{\List α → \List α}
\\]

with a fresh/abstract type variable \\(α\\). (I will not be keeping
opened type variables in context to avoid confusing them with hypotheses.)

Any derivation of a function type, without loss of generality
(w.l.o.g), is equivalent to a derivation starting with a function
introduction. This is the η-expansion rule for functions: any proof
term \\(e\\) is equivalent to \\(λx.~(e~x)\\), a proof that starts
with a \\(λ\\). So any proof can be taken to start as follows:
\\[
\rule{
\judge{\List \alpha}{\List \alpha}
}{
\judge{}{\List \alpha \to \List \alpha}
}
\\]
we can, w.l.o.g, unfold the recursive type in the context
(\\(\List α = 1 + (α × \List α)\\)):
\\[
\rule{
\judge{1 + (α × \List α)}{\List α}
}{
\rule{
\judge{\List α}{\List α}
}{
\judge{}{\List α → \List α}
}}
\\]

A derivation with a sum type as hypothesis can, w.l.o.g, be assumed to
start by splitting on this pair (this is the η-expansion rule
for sums):
\\[
\rule{
\judge{1}{\List α}
\quad
\judge{α × \List α}{\List α}
}{
\rule{
\judge{1 + (α × \List α)}{\List α}
}{
\rule{
\judge{\List α}{\List α}
}{
\judge{}{\List α → \List α}
}}}
\\]

In the right subgoal, we can always, w.l.o.g, split a hypothesis of
product type:
\\[
\rule{
\emptyrule{\judge{1}{\List α}}
\quad
\rule{
\judge{α, \List α}{\List α}
}{
\judge{α × \List α}{\List α}
}}{
\rule{
\judge{1 + (α × \List α)}{\List α}
}{
\rule{
\judge{\List α}{\List α}
}{
\judge{}{\List α → \List α}
}}}
\\]

Now, an interesting pattern emerges. In the process of trying to prove
\\(\judge{\List α}{\List α}\\), we have to prove the (right) subgoal
\\(\judge{α,\List α}{α}\\). We can generalize this derivation by assuming that we
start with some number \\(n\\) of variables of type \\(α\\) in the context
(we write \\(α^n\\) for this):
\\[
\rule{
\rule{
\judge{\alpha^n}{\List \alpha}
}{
\judge{\alpha^n, 1}{\List \alpha}
}
\quad
\rule{
\judge{\alpha^{n+1}, \List \alpha}{\List \alpha}
}{
\judge{\alpha^n, \alpha \times \List \alpha}{\List \alpha}
}}{
\rule{
\judge{\alpha^n, 1 + (\alpha \times \List \alpha)}{\List \alpha}
}{
\judge{\alpha^n, \List \alpha}{\List \alpha}
}}
\\]

\\[
\newcommand{\llbracket}{〚}
\newcommand{\rrbracket}{〛}
\newcommand{\sem}[1]{\llbracket{} #1 \rrbracket{}}
\\]

Let us write \\(\sem{\judge{\alpha^n, \List \alpha}{\List \alpha}}\\)
for the search space corresponding to all possible derivations of the
judgment \\(\judge{\alpha^n, \List \alpha}{\List \alpha}\\).  All the
proof steps above have been done "without loss of generality"
(in terms of focusing, we only used invertible rules), so they appear
in any such derivation. Similarly, let us write
\\(\sem{\judge{\alpha^n}{\List \alpha}}\\) for the space of all
possible derivations of \\(\judge{\alpha^n}{\List \alpha}\\), then
above we have proven that
\\[
\sem{\judge{\alpha^n, \List \alpha}{\List \alpha}}
\quad=\quad
\sem{\judge{\alpha^n}{\List \alpha}}
\times
\sem{\judge{\alpha^{n+1}, \List \alpha}{\List \alpha}}
\\]

This equality can be unfolded at will
\\[
\begin{align}
& \sem{\judge{\alpha^n, \List \alpha}{\List \alpha}} \\
= & \sem{\judge{\alpha^n}{\List \alpha}}
    \times
    \sem{\judge{\alpha^{n+1}, \List \alpha}{\List \alpha}} \\
= & \sem{\judge{\alpha^n}{\List \alpha}}
    \times
    \sem{\judge{\alpha^{n+1}}{\List \alpha}}
    \times
    \sem{\judge{\alpha^{n+2}, \List \alpha}{\List \alpha}} \\
= & \sem{\judge{\alpha^n}{\List \alpha}}
    \times
    \sem{\judge{\alpha^{n+1}}{\List \alpha}}
    \times
    \sem{\judge{\alpha^{n+2}}{\List \alpha}}
    \times
    \sem{\judge{\alpha^{n+3}, \List \alpha}{\List \alpha}} \\
= & \dots \\
\end{align}
\\]

or written as an infinite product
\\[
    \sem{\judge{\alpha^n, \List \alpha}{\List \alpha}}
    \quad=\quad
    \prod_{k \in \Nat{}}{\sem{\judge{\alpha^{n+k}}{\List \alpha}}}
\\]
and, in particular,
\\[
\begin{align}
& \sem{\judge{}{\List \alpha \to \List \alpha}} \\
= & \sem{\judge{\alpha^0, \List \alpha}{\List \alpha}} \\
= & \prod_{n \in \Nat{}}{\sem{\judge{\alpha^n}{\List \alpha}}} \\
\end{align}
\\]

Now let's look at the structure of the derivations of
\\(\judge{\alpha^n}{\List \alpha}\\). A proof of this judgment cannot
start with a "left rule", inspecting the value of one of the \\(n\\)
variables of type \\(α\\), given that the structure of \\(α\\) is
unknown/abstract. It must start by choosing to either build the empty
list or a cons cell. We write this as follows (after unfolding
the type):

\\[
\rule{
\rule{
\judge{\alpha^n}{1}
\quad\oplus\quad
\judge{\alpha^n}{\alpha \times \List \alpha}
}{
\judge{\alpha^n}{1 + (\alpha \times \List \alpha)}
}}{
\judge{\alpha^n}{\List \alpha}
}
\\]

The \\(\oplus\\) notation between two judgments is non-standard; it
means that they are not two requirements of the same proof, but two
alternatives for possible proofs. All valid proofs fit that structure,
and they either have a \\(\judge{\alpha^n}{1}\\) premise or
a \\(\judge{\alpha^n}{\alpha \times \List \alpha}\\) premise. With
this syntax, we are describing a set of possible derivations, rather
than a single (partial) derivation.

Proofs of \\(\judge{\Gamma}{1}\\) are trivial, and a proof of
a product is always, w.l.o.g, a product of proofs (in intuitionistic
logic / the λ-calculus they reuse the same context), so we can
decompose further:
\\[
\rule{
\rule{
\rule{
}{
\judge{\alpha^n}{1}
}
\quad\oplus\quad
\rule
{
\judge{\alpha^n}{\alpha}
\quad
\judge{\alpha^n}{\List \alpha}
}{
\judge{\alpha^n}{\alpha \times \List \alpha}
}
}{
\judge{\alpha^n}{1 + (\alpha \times \List \alpha)}
}}{
\judge{\alpha^n}{\List \alpha}
}
\\]

There is exactly one possible proof of \\(\judge{\alpha^n}{1}\\), so
its search space is \\(1\\), the unit set
(with a single element). There are exactly \\(n\\) possible proofs of
\\(\judge{\alpha^n}{\alpha}\\), so the search space is just \\(n\\),
seen as a set, or, in type-theoretic notation, \\(\Fin{n}\\). We thus
have the recursive equation:
\\[
\sem{\judge{\alpha^n}{\List \alpha}}
\quad=\quad
1 + (\Fin n \times \sem{\judge{\alpha^n}{\List \alpha}})
\\]

This type is either \\(1\\), or a \\(\Fin{n}\\) and itself,
recursively. This is exactly a list:
\\[
\sem{\judge{\alpha^n}{\List \alpha}}
\quad=\quad
\List{(\Fin{n})}
\\]

so, plugging everything together:
\\[
\begin{align}
& \sem{\forall \alpha. \List \alpha \to \List \alpha} \\
= & \prod_{n \in \Nat{}}{\sem{\judge{\alpha^n}{\List \alpha}}} \\
= & \prod_{n \in \Nat{}}{\List{(\Fin{n})}} \\
\end{align}
\\]


### Post Scriptum

Some of reasoning steps above can be formulated in a way that is less
clear but more familiar, as a sequence of type isomorphisms. For
example, the first part on \\(\sem{\judge{\alpha^n, \List
\alpha}{\List \alpha}}\\) can written as:

\\[
\begin{align}
&
∀α. αⁿ × \List α → \List α
\\ &
= \text{(unfold List)}
\\ &
    ∀α. αⁿ × (1 + α × \List α) → \List α
\\ &
    = \text{(distribute × over +)}
\\ &
    ∀α. ((αⁿ × 1) + (αⁿ⁺¹ × \List α)) → \List α
\\ &
    = \text{(A × 1 ≃ A)}
\\ &
    ∀α. (αⁿ + (αⁿ⁺¹ × \List α)) → \List α
\\ &
    = \text{(A+B) → C ≃ (A→C)×(B→C)}
\\ &
    ∀α. (αⁿ → \List α) × (αⁿ⁺¹ × \List α → \List α)
\\ &
    = \text{(distribute ∀α below product)}
\\ &
    (∀α. αⁿ → \List α) × (∀α. αⁿ⁺¹ × \List α → \List α)
\\
\end{align}
\\]

Reading this equational sequence, it may look like we had to make the right
choice at each step; but the proof-search perspective reveals that
there were in fact no choices, as each time we apply invertible rules
("w.l.o.g. rules").

Furthermore, some parts cannot be derived in this style; in the latter
part of the proof, the isomorphism between
\\(∀\alpha. \alpha^n → \alpha\\) and \\(\Fin{n}\\), which is
immediate from a proof search perspective, cannot be justified in this
way. (In particular, \\(A×B → C\\) is *not* isomorphic to
\\((A→C)+(B→C)\\).)


### Going further

- It is an unfortunately-little-known obvious fact that many things we
  associate to "free theorems" can be recovered by proof search. For
  example, it is much simpler to prove that the only inhabitant of
  \\(\forall \alpha. \alpha \to \alpha\\) is the identity using proof
  search than parametricity. I briefly discussed the idea in the
  section 1.3 of my 2015 article, [Which simple types have a unique
  inhabitant?](http://gallium.inria.fr/~scherer/research/unique_inhabitants/unique_stlc_sums-long.pdf).

- If you are unfamiliar with proof search (or the LF community) and
  curious about what I mean by "canonical forms" and why I think this
  is an important idea, see my non-technical 2017 article [Search for
  Program
  Structure](http://www.ccs.neu.edu/home/gasche/research/canonical-forms/snapl.pdf). The
  problem of extending the notion of canonical forms to arbitrary
  polymorphic types is briefly discussed in the section 14.5 of my
  2016 [phd
  manuscript](http://www.ccs.neu.edu/home/gasche/phd_thesis/scherer-thesis.pdf).

- If you haven't heard of it yet, you would probably be interested in
  the 2010 article [Testing Polymorphic
  Properties](http://publications.lib.chalmers.se/records/fulltext/local_99387.pdf)
  by Jean-Philippe Bernardy, Patrik Jansson and Koen Claessen. Li-Yao
  has a 2016 implementation called
  [Metamorph](https://github.com/Lysxia/metamorph) that got us talking
  together. The problems of understanding canonical forms and testing
  are quite related, but yet not exactly the same...

### You might also like

- [Categorical Semantics for Dynamically Typed Programming
  Languages](http://prl.ccs.neu.edu/blog/2017/05/01/categorical-semantics-for-dynamically-typed-programming-languages/)

- [Toward Type-Preserving Compilation of Coq, at POPL17 SRC](https://williamjbowman.com/blog/2017/01/03/toward-type-preserving-compilation-of-coq-at-popl17-src/)

- [Understanding Constructive Galois
  Connections](http://prl.ccs.neu.edu/blog/2016/11/16/understanding-constructive-galois-connections/).
