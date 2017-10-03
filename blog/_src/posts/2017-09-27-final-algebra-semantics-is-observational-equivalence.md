    Title: Final Algebra Semantics is Observational Equivalence
    Date: 2017-09-27T15:44:57
    Tags: category theory, math, final encoding, observational equivalence, by Max New

Recently, "final encodings" and "finally tagless style" have become
popular techniques for defining embedded languages in functional
languages.
In a recent discussion in the Northeastern PRL lab, [Michael
Ballantyne][michaelb], [Ryan Culpepper][ryanc] and I asked "in what
category are these actually final objects"?
As it turns out our very own [Mitch Wand][mitchw] wrote one of the
first papers to make exactly this idea precise, so I read it
[available here][mitch-final-algebra] and was pleasantly surprised to
see that the definition of a final algebra there is essentially
equivalent to the definition of observational equivalence.

In this post, I'll go over some of the results of that paper and
explain the connection to observational equivalence.
In the process we'll learn a bit about categorical logic, and I'll
reformulate some of the category theory in that paper to be a bit
more modern in presentation, cleaning some things up in the process.

<!-- more -->

# Intuition: Implementing a Signature

As a running example, say we wanted to implement a datatype of finite
maps whose keys and values are both integers, i.e., finite multisets
of integers.

We could specify such a datatype by specifying a little language of
numbers and finite multisets.
We'll have two "sorts" `num` and `multiset`, a constant for every integer, and an addition function
```
'n : () -> num;
add : (num, num) -> num
```
subject to the silly-looking equation:
```
add('n,'m) = '(n + m)
```

and some operations on multisets
```
empty : () -> multiset;
singleton : (num) -> multiset;
union : (multiset, multiset) -> multiset;
remove : (num, multiset) -> multiset;
count : (num, multiset) -> num
```
subject to the computational equations:

```
count('n, empty) = '0
count('n, singleton('n)) = '1
count('n, singleton('m)) = '0
count('n, union(s,t)) = add(count('n,s), count('n, t))
count('n, remove('n,s)) = '0
count('n, remove('m,s)) = count('n,s)
```

These are "all" of the equations we need to actually run our programs
and get a number out, but not all the equations we intuitively *want*
for reasoning about our programs.
For instance, clearly `union` should be commutative, and `remove`
should be idempotent, but it's impossible to prove that with just the
equations specified.
In fact, we can make a model of this theory that refutes them by
constructing the "initial algebra". In Haskell, we could say

```Haskell
data MultiSet = Empty 
  | Singleton Integer
  | Union MultiSet MultiSet
  | Remove Integer MultiSet
  deriving (Eq)

count :: Integer -> MultiSet -> Integer
count n Empty = 0
count n (Singleton m) | n == m = 1
count n (Singleton m) | n /= m = 0
count n (Union s t) = (count n s) + (count n t)
count n (Remove m s) | n == m = 0
count n (Remove m s) | n /= m = count n s
```
Then it is completely obvious that all of our equations hold, but then
`Union` is *not* commutative, as ghci will tell us:

```Haskell
> (Singleton 1 `Union` Singleton 2) == (Singleton 2 `Union` Singleton 1) 
False
```

However, there is another encoding that will give us that `union` is
commutative and `remove n` is idempotent and actually every equation
we could possibly want!
It's called the "final encoding" or "final algebra".
In Haskell, this looks like:

```Haskell
data MultiSet' = MultiSet' { _count :: Integer -> Integer }

count' :: Integer -> MultiSet' -> Integer
count' n m = _count m n

empty :: MultiSet'
empty = MultiSet' { _count = \n -> 0 }

singleton :: Integer -> MultiSet'
singleton n = MultiSet' { _count = \m -> if n == m
                                         then 1
                                         else 0 }

union :: MultiSet' -> MultiSet' -> MultiSet'
union s t = MultiSet' { _count = \n -> (count' n s) + (count' n t) }

remove :: Integer -> MultiSet' -> MultiSet'
remove n s = MultiSet' { _count = \m -> if n == m
                                        then 0
                                        else count' n s }

test' = and [ count' n s == count' n t | n <- [0..1000]]
s = singleton 1 `union` singleton 2
t = singleton 2 `union` singleton 1
```

Now we can verify that `union` is commutative because 

```Haskell

union s t = MultiSet' { _count = \n -> (count' n s) + (count' n t) }
          = MultiSet' { _count = \n -> (count' n t) + (count' n s) }
		  = union t s
```

since `+` is commutative.
Equality isn't decidable anymore so I can't give you a simple piece of
code to witness this, but we can test our example before and we won't
be able to distinguish them, no surprise:

```Haskell
> let s = singleton 1 `union` singleton 2
> let t = singleton 2 `union` singleton 1
> and [ count' n s == count' n t | n <- [0..1000]]
True
```

How do we know this is the "best" or at least "most canonical"
implementation of our datatype?
The intuition is that we really don't care at all *how* our multisets
are implemented as long as they behave the right way with respect to
`count` since `count` returns an `Integer`, a type we do understand.
Our encoding accomplishes this by representing a multiset `s` by the
partially applied function `\n -> count n s`.

The formal name for this idea is *observational equivalence*.
We say that two closed terms `s,t` of sort `multiset` are
*observationally equivalent* if for any term `C` of type `num` that
has `s` as a subterm, we can swap `t` in for `s` and prove that the
two terms are equal.
For instance `C` might be `count(3, union(s, singleton(3)))` or
`add(4,remove(5,s))`.
Then we've reduced the possibly complicated equality for `multiset` to
the simple equality of `num`.

Proving that the final encoding above satisfies all observational
equivalences is beyond the scope of this blog post (see
[here][proving-final-encoding]), but let's see what all this talk
about "algebras", initial or final is all about.

# Formalization Attempt 1: Algebras of a Theory

First, our little language of numbers and multisets is called a
*theory*. The specific category gadget that we'll use to describe it
is a *multi-sorted Lawvere theory*, or just *Lawvere theory* for
short.

A *Lawvere theory* is a category with finite products all of whose
objects are finite products of a collection of *sorts* \\(S\\). We can
construct this category from our little language above by making the
objects be *contexts* \\(x:num,y:multiset,...\\) and morphisms \\(\Gamma \to
x_1:s_1,...,x_n:s_n\\) to be \\(n\\)-tuples of terms \\(\Gamma \vdash t_1 : s_1,...,
\Gamma \vdash t_n :  s_n\\) *modulo* the equations we've specified. We'll use the
letter \\(T\\) to mean a Lawvere theory.

Then a *\\(T\\)-algebra* is a denotational semantics of our theory \\(T\\),
i.e., a product preserving functor \\(A : T \to Set\\). This means for
every sort we get a set \\(A(s)\\) and for every term \\(x_1:s_1,...,x_n:s_n
\vdash t : s\\) a function \\(A(t) : A(s_1)\times\cdots \times A(s_n) \to
A(s)\\).

Finally a *morphism of \\(T\\)-algebras* from \\(A\\) to \\(B\\) is a way to
translate one algebra into another. Briefly, it is a natural
transformation from \\(A\\) to \\(B\\), but concretely this means for every
sort \\(s\\) we get a function \\(\alpha_s : A(s) \to B(s)\\) that translates \\(A\\)s
interpretation of \\(s\\) as a set into \\(B\\)s. The key property that we want is that the
operations according to \\(A\\) and \\(B\\) do the same thing as determined by
\\(\alpha\\). Specifically, for any term \\(x_1:s_1,...,x_n:s_n \vdash t :
s\\), and inputs \\(x_1 \in A(s_1),...,x_n \in A(s_n)\\) we should get the
same result if we evaluate \\(A(t)(x_1,\ldots,x_n)\\) and then apply
\\(\alpha_s\\) as if we first translate \\(x_1,\ldots,x_n\\) to
\\(B(s_1),\ldots,B(s_n)\\) and then apply \\(B(t)\\). If you unwind the
definitions, this is exactly what naturality says.

Then we have a category we'll call \\(T-Alg\\) of \\(T\\)-algebras and
we can ask if there are initial or final algebra.
It turns out that both of them *always* exist.

The initial algebra is most famous here, we define for each sort
\\(In(T)(s) = \cdot \vdash s\\), the closed terms of that sort modulo the
equivalence of the theory, and \\(In(T)(s_1,\ldots,s_n) =
In(T)(s_1)\times\ldots,In(T)(s_n)\\). Then the terms are just
interpreted as the functions you get by plugging closed inputs into
them. Then if we look at what what a morphism of \\(T\\)-algebras from
\\(In(T) \to A\\) is, we see that we don't have any choice, the only one
is the one that maps \\(\cdot \vdash t : s\\) to \\(A(t)\\) and this makes all
the right diagrams to commute.
This is pretty similar to our definition of "initial algebra" before,
except that this time we defined `count` as a function, not just a
case of an ADT, but that was just an easy way to satisfy the
computational equations for `count`.

However, an egregious flaw presents itself when we look at what the
*final* algebra is. It's completely trivial! We can define \\(Fin(T)\\) to
take every sort to a one element set \\(Fin(T)(s) = \{*\}\\) and every
term to the trivial function \\(\{*\}^n \to \{*\}\\).
What the hell?
This interprets numbers and multisets as trivial one-element sets.
To rule this one out, we need to add some conditions to our algebras.

# Formalization: Algebras of a Theory Extension

To rule out these boring algebras, and get a nice final algebra, we
have to recognize that the sorts `num` and `multiset` in our theory
are not really on equal footing.
While we are not sure how multisets should be defined, we know
*exactly* what numbers are!

To formalize this we'll call the full theory \\(T_1\\) and the theory with
just numbers \\(T_0\\). Then there should be a map from \\(T_0\\) to \\(T_1\\)
that is the inclusion of theories.  We'll formalize this as a
*morphism of theories*. A morphism of theories is a *strict*
product-preserving functor from one theory to another.
The strictness ensures that we don't mix up our sorts and our
contexts, a morphim of theories has to map sorts to sorts, whereas a
non-strict functor could map a sort to a context with two sorts it's
equivalent to.
What this really amounts to is a translation of one theory into
another. It maps sorts to sorts and terms to terms of the appropriate
sorts in a compositional way.
However, we don't want to consider *all* such morphisms, only the ones
that are "conservative extensions", which means

1. there are no new closed terms at old types
2. closed terms that were different before remain different.

In our example (1) ensures that we don't add any new exotic numbers
like `undefined` or `∞`, and (2) ensures that we keep \\(0\\)
different from \\(1\\), like the final algebra did before by having
all numbers have the same interpreation \\(*\\).

We can formalize this in the following way. Note that any morphism of
Lawvere theories \\(m : T \to S\\) induces a *functor* on the category of
algebras \\(m^* : S-Alg \to T-Alg\\) by just composing functors. An
\\(S\\)-algebra is a functor from \\(S\\) to sets, and \\(m\\) is a functor from
\\(T\\) to \\(S\\) so we can compose to get \\(m^*(A)(t) = A(m(t))\\).

Now, we can express the idea of a conservative extension by saying that the canonical
arrow from \\(In(T)\\) to \\(m^*(In(S))\\) is an isomorphism. Recalling the
definition of initial algebras, this says exactly that the closed
terms in \\(T\\) up to \\(T\\)-equivalence are isomorphic to the closed terms
of the type provided by \\(m\\) in \\(S\\) up to \\(S\\)-equivalence.
This is an equivalent formulation to the definition in Mitch's paper,
but there it is separated into two properties fullness and
faithfulness, and doesn't use the initial algebras and \\(m^*\\)
explicitly.

Now we can verify that the inclusion \\(i : T_0 \to T_1\\) of the number
theory into the number-multiset theory is an extension in this sense.

Finally we can define our notion of \\(i\\)-algebra, which will be our
correct notion of algebra.
An \\(i\\)-algebra is a \\(T_1\\) algebra \\(A\\) such that 

1. The canonical algebra map \\(! : In(T_0) \to m^*A\\) is an isomorphism.
2. The canonical algebra map \\(! : In(T_1) \to A\\) is surjective i.e.,
   for each sort \\(s, !_s\\) is surjective.

The first condition says again that we have a conservative extension
of \\(T_0\\), but the second is more interesting. It says that every
denotation given by \\(A\\) is represented by some term in \\(T_1\\).  In fact
what it really ensures is that \\(A\\) determines a *congruence relation*
on \\(T_1\\) given by \\(t1 \equiv_A t2\\) if \\(A(t1) = A(t2)\\).
In light of this, the first condition could be called *adequacy*.

Furthermore, the surjectivity condition ensures that any morphism of
\\(i\\) algebras, i.e., a map as \\(T_1\\)-algebras is also surjective, so a
morphism \\(A \to B\\) is a witness to the fact that \\(B\\) determines a
*stronger* congruence relation on \\(T_1\\) than \\(A\\) does: \\(t1 \equiv_B t2
\implies t1 \equiv_A t2\\).
Then asking for a final algebra is asking for exactly the:

> Strongest adequate congruence relation
	
which is exactly the definition of observational equivalence you will
find in, say Pitt's chapter of [Advanced TAPL][atapl].
There is a difference in the meaning of *adequacy*, though. Usually
adequacy is defined in terms of an operational semantics, but here
everything is based on an axiomatic notion of equality, but I think
they play the same role in the two settings, so I think it's
reasonable to use the same word.
On thing I like about this formulation is very nice though since it
makes obvious that *adequacy* is not a predetermined concept, we have
to pick \\(T_0\\) and \\(i\\) in order to know what adequacy means.

# Conclusion: Tying it back to Final Encodings

So now we've seen that

> Final algebras are equivalent to initial algebras modulo observational equivalence

Of course we haven't precisely gotten back to where we started: we
were talking about denotational semantics in terms of sets and
functions, but what we really want are implementations in our favorite
programming languages.
Fortunately, we didn't use very many properties of sets in our
definition, so it's pretty easy to swap out the category of Sets for
some category built out of the terms of our programming language.
We can also swap out sets for some much cooler category of denotations
like domains or metric spaces or time-varying values.

Another question is how to implement this when we have a proper *type
theory* and not just some boring sorts. In particular, if we have
function types, then we won't be able to get functions from functions
in our term model to functions in our denotations due to
contravariance. Perhaps logical relations are the solution?

[ryanc]: http://ccs.neu.edu/home/ryanc
[michaelb]: https://github.com/michaelballantyne
[mitchw]: http://www.ccs.neu.edu/home/wand/
[mitch-final-algebra]: https://www.cs.indiana.edu/ftp/techreports/TR65.pdf
[proving-final-encoding]: https://hal.inria.fr/inria-00076514/document
[atapl]: https://www.cis.upenn.edu/~bcpierce/attapl/
