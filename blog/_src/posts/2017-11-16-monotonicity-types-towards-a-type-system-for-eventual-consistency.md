    Title: Monotonicity Types: Towards A Type System for Eventual Consistency
    Date: 2017-10-22T11:59:06
    Tags: types, monotonicity, CRDTs, eventual consistency, by Kevin Clancy

A few weeks back, we published a draft of an article entitled [_Monotonicity Types_](https://infoscience.epfl.ch/record/231867). In it, we describe a type system which we hope can aid the design of distributed systems by tracking monotonicity with types.

But first, what, precisely, do we mean by _monotonicity_? Here's a short definition:

A partially ordered set is a set \\(P\\) endowed with a relation \\(\leq\\) such that for all \\(p, q, r \in P\\) we have:

1. \\(p \leq p\\) (reflexivity)
2. \\(p \leq  q\\) and \\(q \leq r\\) implies \\(p \leq r\\) (transitivity)
3. \\(p \leq q\\) and \\(q \leq p\\) implies \\(p = q\\) (anti-symmetry)

If \\(P\\) and \\(Q\\) are partially ordered sets, we say that a function \\(f : P \to Q\\)
between them is *monotone* if for all \\(p_1, p_2 \in P\\) with \\(p_1 \leq p_2\\), we have \\(f(p_1) \leq f(p_2)\\).

So, said another way, increasing the input to a monotone function causes an increase to its output.

Particularly in the context of concurrent and distributed programming,
monotonicity has arisen time and time again as an important property. Designers of languages for coordination-free distributed programming such as Lasp \[[Meiklejohn et al. (2015)](#ref1)\] and BloomL \[[Conway et al. (2012)](#ref1)\], as well as designers of data types and abstractions for eventual consistency or determinism such as CRDTs \[[Shapiro et al. (2011)](#ref3)\] and LVars \[[Kuper et al. (2013)](#ref4)\] have noticed that monotonic evolution of program state over time is a necessary property in their designs. Lasp and BloomL in particular require the use of monotone functions as primitives of program composition.

Thus if a user would like to make use of such a language for concurrent and distributed programming, they're required to write monotonic program functions, which can actually be quite tricky, in order to get the consistency or determinism guarantees that the given language/abstraction was designed to provide.

To get a better idea of how monotonicity might be important in the context of data replicated over a distributed system, let's look at an example. Let's start with the assumption that we need a simple function that can determine whether a (potentially replicated) counter's current value is odd or even. We might write the following function to accomplish this:

```
fun IsOdd(x : Nat) = x % 2 == 1
```

However, if the counter is shared among multiple hosts in a network,
this observation could be invalid due to increments at other hosts which have not synchronized with the local one. The problem is that the value observed is not monotone with respect to the value of the counter, and so by incrementing the counter, a host can cause the value returned by IsOdd to change from true to false, without that change being visible locally. If we observe that a monotone boolean condition inferred from a replicated variable is true, we know that all hosts will eventually agree that the condition is true and, furthermore, will lack the ability to change it. It therefore can be acted upon as a global property of the system rather than a local artifact of data replication. A type system for differentiating between monotone and non-monotone conditions would therefore aid the programmer in deciding which observations they can and cannot perform on replicated data.

We believe that a type system could be used to ensure that a given program fragment is monotone. A type system for monotonicity could push the development of coordination-free distributed and concurrent applications outside of the realm of experts in distributed systems, by enabling customization and extension of such systems by non-experts.

Towards this aim, we have been designing a type system for tracking monotonicity, as an extension of the simply typed lambda calculus which adds a new function abstraction construct called the *sfun*.

Our approach alllows the programmer to write a special kind of function definition, the body of which is type checked using a richer type system, one which reasons about function composition rather than application. Such a function can then be proven monotone by utilizing the fact that the composition of two monotone functions is itself monotone, and other related principles.

Monotonicity is a relational property; that is, its a property involving multiple applications of the same function. Such properties are blind spot for traditional type systems, so our design requires some unusual and interesting features.

To give an intuition of what our type system looks like; as mentioned earlier, we introduce a special abstraction for monotone functions called an _sfun_. Since the programmer only cares about the monotonicity of a select group of functions, a special syntax construct, the sfun, serves as a signal to the type checker. Unlike the simply typed world outside of the sfun abstraction, the body of an sfun is type checked using a refinement type system, which I call the lifted type system in which monotonicity is tracked. So in our system we have a lifted (local) type system within sfuns, and a terminal (global) type system for the typed world outside of the sfun.

This terminal (global) type system is a traditional type system which is embedded as a specific kind of refinement inside of the lifted (local) type system. Applying functions is a way of projecting from the lifted (local) world into the terminal (global) world.

In more detail, the "global world” outside of an sfun abstraction is viewed as a degenerate subset of the “lifted world” inside the sfun abstraction. A globally well-typed sfun application is viewed as a projection onto this degenerate subset. Inside the sfun abstraction, we track the way in which each term depends on the sfun’s arguments (for example, monotonically or antitonically), but terms originating outside of the sfun (both literal constants and occurrences of variables from the global type environment) are not affected by sfun's arguments at all.

Lifted reduction, the operational semantics characterized by the lifted type system, performs a step-by-step composition of the components (sfun applications, variable occurrences, literal values, etc) of an sfun. Lifted normalization of an sfun's body corresponds to the complete composition of the sfun, resulting in the sfun itself. So when our lifted type system proves the body of an sfun monotone, we know the sfun will behave monotonically when applied under global reduction.

We'll leave you with an aspirational example, which demonstrates the need for a type system, rather than a more monolithic form of analysis, for proving functions monotone. This example is inspired by Lasp \[[Meiklejohn et al. (2015)](#ref1)\], where CRDTs are composed into programs through the use of monotone functions. It's a monotone function which takes two GCounter \[[Shapiro et al. (2011)](#ref3)\] values as arguments, and produces a GCounter maintaining the sum of its two arguments as an output.

```
getAt :: (m : NatMap, k : Nat) ⇒ Nat[↑ m, ? k]
joinAt :: (m : NatMap, k : Nat, n : Nat) ⇒ NatMap[↑ m, ? k, ↑ n]
span :: (x:NatMap) ⇒ Nat[↑ x]
max :: (a : Nat, b : Nat) ⇒ Nat[↑ a, ↑ b]
emptyMap :: NatMap
+ :: (x:Nat, y:Nat) ⇒ Nat[↑ x, ↑ y]
> :: (x:Nat, y:Nat) ⇒ Bool[↑ x, ↓ y]

type GCounter = { map : NatMap }

sfun sumCounters(x : GCounter, y : GCounter) 
 : GCounter[↑ x, ↑ y] =
 let xMap : NatMap[↑ x, ↑ y] = x.map
 let yMap : NatMap[↑ x, ↑ y] = y.map
 let maxSpan : Nat[↑ x, ↑ y] = max (span xMap) (span yMap)
 fun sumCell(k : Nat, acc : NatMap[↑ x, ↑ y]) 
  : NatMap[↑ x, ↑ y] =
  let cond : Bool[↑ x, ↓ y] = k > maxSpan
   if cond then
    acc
   else
    let acc' = joinAt acc k ((getAt xMap k) + (getAt yMap k))
    sumCell (k+1) acc'
 let initMap : IntArray[↑ x, ↑ y] = emptyMap
 GCounter { array = sumCell 0 initMap }
```

The \\(\uparrow\\) symbol appearing in types is used to represent monotonic dependency on an sfun's argument. \\(\downarrow\\) is for antitonic dependency, and \\(?\\) is for unknown dependency. Sfun types, which may have multiple arguments, are written with \\(\Rightarrow\\) rather than \\(\to\\).

While our system can handle much of this example, it can't handle everything yet, for several reasons. First, it involves an if condition which depends on the arguments of the enclosing sfun. To handle this, we would need to incorporate the notion of domain restriction into lifted reduction. Second, it involves recursion. This is problematic for us, because our system utilizes the fact that all well-typed programs terminate. We could partially address this by adding terminating fixpoint combinators, which allow recursion given some well-founded termination metric, as in \[[Vazou et al. (2014)](#ref5)\]. However, that would not be adequate for this particular function, which could require arbitrarily many levels of recursion depending on which values are supplied as arguments.

So there's still much to do! If you're interested in more details behind the type system, have a look at Kevin's blog article, [Monotonicity Through Types](https://kevinclancy.github.io/2017/11/09/monotonicity-through-types.html), or have a look at the full [Monotonicity Types](https://infoscience.epfl.ch/record/231867) preprint for more.

### References


<span id="ref1">C. Meiklejohn and P. Van Roy. _Lasp: A language for distributed, coordination-free programming._ In Proceedings of the 17th International Symposium on Principles and Practice of Declarative Programming, PPDP ’15, pages 184–195, New York, NY, USA, 2015. ACM.</span>

<span id="ref2">N. Conway, W. R. Marczak, P. Alvaro, J. M. Hellerstein, and D. Maier. _Logic and lattices for distributed programming_. In Proceedings of the Third ACM Symposium on Cloud Computing, SoCC ’12, pages 1:1–1:14, New York, NY, USA, 2012. ACM.</span>

<span id="ref3">M. Shapiro, N. Preguiça, C. Baquero, and M. Zawirski. _Conflict-Free replicated data types_. In Stabilization, Safety, and Security of Distributed Systems, Lecture Notes in Computer Science, pages 386–400. Springer, Berlin, Heidelberg, Oct. 2011.</span>

<span class="ref4">L. Kuper and R. R. Newton. _LVars: Lattice-based data structures for deterministic parallelism_. In Proceedings of the 2nd ACM SIGPLAN Workshop on Functional High-performance Computing, FHPC ’13, pages 71–84, New York, NY, USA, 2013. ACM.</span>

<span class="ref5">N. Vazou, E. L. Seidel, R. Jhala, D. Vytiniotis, and S. Peyton-Jones. _Refinement types for Haskell_. SIGPLAN Not. 49, 9 (August 2014), 269-282. </span>
