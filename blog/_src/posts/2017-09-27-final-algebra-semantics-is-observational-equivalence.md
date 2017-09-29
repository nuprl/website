    Title: Final Algebra Semantics is Observational Equivalence
    Date: 2017-09-27T15:44:57
    Tags: DRAFT

Recently, "final encodings" and "finally tagless style" have become
popular techniques for defining embedded languages in functional
languages.
In a recent discussion in the Northeastern PRL lab, [Michael
Ballantyne][michaelb], [Ryan Culpepper][ryanc] and I asked "in what
category are these actually final objects"?
As it turns out our very own [Mitch Wand](mitchw) wrote one of the
first papers to make exactly this idea precise, so I read it
[available here][mitch-final-algebra] and was pleasantly surprised to
see that the definition of a final algebra there is essentially
equivalent to the definition of observational equivalence.

In this post, I'll go over some of the results of that paper and
explain the connection to observational equivalence.
I'll also reformulate some of the category theory in that paper to be
a bit more modern in presentation, cleaning some things up in the
process in my opinion.

<!-- more -->

# Intuition: Implementing a Signature

As a running example, say we wanted to implement a datatype of finite
maps whose keys and values are both integer, i.e., finite multisets
of integers.

We could specify such a datatype by specifying a little language of
numbers and finite multisets.
We'll have two "sorts" `num` and `multiset`, a constant for every integer, and an addition function
```
'n : () -> num
add : (num, num) -> num
```
subject to the silly-looking equation:
```
add('n,'m) = n + m
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
count('n, empty) = 0
count('n, singleton('n)) = 1
count('n, singleton('m)) = 0
count('n, union(s,t)) = add(count('n,s), count('n, t))
count('n, remove('n,s)) = 0
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
It's called the "final encoding" or "final algebra" and it looks like
this in Haskell:

```Haskell
data MultiSet' = MultiSet' { _count :: Integer -> Integer }

count' :: Integer -> MultiSet' -> Integer
count' n m = _count m n

empty :: MultiSet'
empty = MultiSet' $ \n -> 0

singleton :: Integer -> MultiSet'
singleton n = MultiSet' $ \m -> if n == m
                               then 1
                               else 0

union :: MultiSet' -> MultiSet' -> MultiSet'
union s t = MultiSet' $ \n -> (count' n s) + (count' n t)

remove :: Integer -> MultiSet' -> MultiSet'
remove n s = MultiSet' $ \m -> if n == m
                               then 0
                               else count' n s
```

And while we don't have decidable equality to help us this time, it's
pretty obvious that `union` is commutative because `+` is commutative.
Testing our example before works, no surprise:

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
This allows us to define "observational equivalence" of two multisets.

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



# Formalization: Algebras of a Theory Extension

# Conclusion: Tying it back to Final Encodings

TODO

[ryanc]: http://ccs.neu.edu/home/ryanc
[michaelb]: ???
[mitchw]: ???
[mitch-final-algebra]: https://www.cs.indiana.edu/ftp/techreports/TR65.pdf
[proving-final-encoding]: ???
