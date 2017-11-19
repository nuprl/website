    Title: Monotonicity Types: Towards A Type System for Eventual Consistency
    Date: 2017-10-22T11:59:06
    Tags: types, monotonicity, CRDTs, eventual consistency, by Kevin Clancy, by Heather Miller


A few weeks back, we published a draft of an article entitled [_Monotonicity Types_](https://infoscience.epfl.ch/record/231867). In it, we describe a type system which we hope can aid the design of distributed systems by tracking monotonicity with types.

<!-- more -->

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

To get a better idea of how monotonicity might be important in the context of data replicated over a distributed system, let's look at an example. Suppose we need a function to determine whether a replicated counter's current value is odd or even, and further suppose that this counter can only be incremented. To accomplish this, we might apply the following function to the counter's value:

```
fun IsOdd(x : Nat) = x % 2 == 1
```

However, the counter replica from which the argument x is obtained may not currently have an up-to-date count of the total number of increments performed in the entire system. We can't rule out the possibility that exactly one remote increment has been performed, in which case IsOdd produces the wrong answer. With this in mind, the value returned by IsOdd does not seem to tell us anything useful. In contrast, consider an application of the following function to the same replicated counter.

```
fun MoreThanTen(x : Nat) = x > 10
```

The boolean values \\(true\\) and \\(false\\) form one of the simplest partially ordered sets of all. We consider \\(false \leq false\\), \\(false \leq true \\), and \\( true \leq true \\). Under this ordering, the MoreThanTen function is monotone: an increase in x can cause the value of \\(x > 10\\) to flip from false to true, but not vice versa. When we observe that the local counter replica P's value is greater than 10, we don't know that the same observation would be drawn from remote replicas. Nonetheless, we assume that all replicas in the system will eventually become aware of all increments that P is currently aware of, at which point their values will be greater than P's current value. This is where MoreThanTen's monotonicity becomes useful. At the point when all replicas have received P's current information, every replica in the system will agree that MoreThanTen applied to the counter's value returns true.

We believe that a type system for proving functions monotone could push the development of coordination-free distributed and concurrent applications outside of the realm of distributed systems experts, by enabling customization and extension of such systems by non-experts.

Towards this aim, we have been designing a type system for tracking monotonicity, as an extension of the simply typed lambda calculus. Our approach allows the programmer to write a special kind of function definition, called an *sfun*, the body of which is type checked using a richer type system, one which reasons about function composition rather than application. Such a function can then be proven monotone by utilizing, among other principles, the fact that the composition of two monotone functions is itself monotone. Monotonicity is a relational property; that is, its a property involving multiple applications of the same function. Such properties are blind spot for traditional type systems, so our design requires some unusual and interesting features.


Reasoning about pointwise orderings on function spaces seems a bit heavy-weight and hasn’t been necessary for any of my use cases. An sfun is therefore first order; that is, both its return type and all of its argument types must be data types rather than function types. We would like to be able to prove that a multi-argument function is monotone *separately* in each of its
arguments; that is, for \\(i \in 1..n\\), if \\(p_i \leq p_i'\\) then \\(f(p_1, \ldots, p_i, \ldots, p_n) \leq f(p_1, \ldots p_i', \ldots p_n)\\).

The monotonicity of an sfun is typically derived from the monotonicity of the primitives used to implement it, which are also sfuns. Here are some example sfun primitives, addition and subtraction on integers:

1.) plus : \\( (x : Int, y : Int) \Rightarrow Int[\uparrow x, \uparrow y] \\)

2.) minus : \\( (x : Int, y : Int) \Rightarrow Int[\uparrow x, \downarrow y] \\)

An *sfun type*, written with \\(\Rightarrow\\) rather than \\(\rightarrow\\), names its formal arguments and also *qualifies* each one. A qualifier is an argument-specific constraint on the behavior of the function. In the above types, the qualifier \\(\uparrow\\) is associated with arguments that are separately monotone and \\(\downarrow\\) is associated with arguments that are separately antitone. The second argument of a binary function \\(f\\) is separately antitone if \\(p_2 \leq p_2'\\) implies \\(f(p_1, p_2) \geq f(p_1, p_2')\\).

Terms outside of sfun abstractions are typed using a *global* typing relation,
which, aside from an sfun abstraction typing rule, is not different from the
typing relations we are familiar with. A global typing judgment has the following form.

\\( \Gamma \vdash t : T \\)   

A typing judgment of the lifted type system, used to type check the body of an sfun, has the following form:

\\( \Gamma;\Omega;\Phi \vdash t : T \\)

Here the *global type environment* \\( \Gamma \\) contains all of the variables bound outside of the sfun, the *ambient type environment* \\( \Omega \\) contains the list of the sfun’s formal arguments, and the
*lifted type environment* \\( \Phi \\) contains those variables in \\( t \\)’s context which are bound inside the sfun. Before getting into the significance of lifted typing judgments, let's look
at a specific application of the global typing rule for sfun abstractions, which uses a single lifted premise.

$$\frac{\Gamma;x:Int;x:Int[=~x] \vdash plus(x,x) : Int[\uparrow~x]}
       {\Gamma \vdash \tilde{\lambda} x : Int. plus(x,x) : ( x : Int ) \Rightarrow Int[\uparrow~x]}$$

Here we type a single-argument sfun abstraction \\(\tilde{\lambda} x:Int. plus(x,x)\\). As you might
have guessed, \\(\tilde{\lambda}\\) is used rather that \\(\lambda\\) to distinguish this as an
sfun abstraction rather than a standard one. Examine the ambient and lifted type environments
used in the premise. Perhaps surprisingly, the abstraction's bound variable \\(x\\) is entered into both environments. When variables occur in types, they are considered references to formal arguments
rather than actual arguments; that is, an occurrence of \\(x\\) in a type (for example \\(Int[\uparrow x]\\)) does not refer to some integer, but instead a "slot" named \\(x\\) which expects to receive some integer from an external source.
Inside the scope of the sfun abstraction, we would like the ability to refer to the abstraction's formal argument \\(x\\), and therefore we add \\(x : Int\\) to the ambient environment.
We would also like to include occurrences of \\(x\\) as terms in the body of the abstraction; for these, we add the entry \\(x : Int[=~x]\\) into the lifted type environment, to be used as a
placeholder for the actual argument supplied to the formal argument \\(x\\). Because references to formal arguments occur only in types, and references to actual arguments occur only in terms,
we can add entries with the same name to both the ambient and lifted environments without creating any ambiguity. In particular, this means that the occurrence of \\(x\\) in Int[\\(\uparrow x\\)] refers
to the entry for \\(x\\) in the ambient type environment rather than the one in the lifted type
environment.    

The premise of the above rule application includes the strange looking types \\(Int[=~x]\\) and \\(Int[\uparrow~x]\\).
Normally, we would expect occurrences of x, which serve as placeholders for the actual argument
of the the function, to have type \\(Int\\), and we would expect our abstraction's body \\(plus(x,x)\\) to
have type \\(Int\\) as well. This traditional approach to typing a function abstraction
characterizes the operational behavior of a single function *after* it has been applied.
Unfortunately, this isn't adequate for reasoning about properties such as monotonicity,
which involve multiple calls to the same function. My approach instead takes the
perspective of inside of a function, *before* it has been applied. Lifted typing then
characterizes the structure of a function as the composition of its constituent parts.
In the above example, an occurrence of the variable \\(x\\) in the term \\(plus(x,x)\\)
has type \\(Int[=~x]\\), meaning that it is a function which takes the value provided to \\(x\\)
(the enclosing sfun's formal argument) as an input, and produces that value unchanged
as a result. We ultimately care about the input/output relation of this function,
and so the concrete values which inhabit this type are set-of-pairs function representations, called *ambient maps*.
The type \\(Int[=~x]\\) happens to be a singleton type, containing the set of pairs
\\(\{ (0,0), (1,1), (-1,-1), (2,2), (-2-2), \ldots \}\\).

The sfun application \\(plus(x,x)\\) is viewed as a function composition,
where the outputs of the functions represented by the two occurrences of \\(x\\)
are forwarded into the left and right arguments of the sfun \\(plus\\). The domain
of this composite function matches the domain \\(x:Int\\) of the enclosing sfun, which it inherits from
the two occurrences of \\(x\\). Since \\(plus\\) returns an \\(Int\\), so does the
composite function \\(plus(x,x)\\). The premise of the above typing rule application tells
us that \\(plus(x,x)\\) has type \\(Int[\uparrow~x]\\), but this premise must
be derived. We previously hinted that such a derivation may utilize the fact that the composition
of two monotone functions is itself monotone, and indeed that is one aspect of the premise's derivation,
but a full treatment is outside the scope of this post.

Since lifted typing is all about function composition, one might wonder how we treat occurrences of \\( \Gamma \\)'s variables
within the body of an sfun. Such a variable might have the type \\( Int \\), representing a data value rather than a function.
In fact, a piece of data can be viewed as a degenerate, constant-valued function, which produces the same result regardless of
which actual arguments any particular sfun is applied to. Subtyping rules enable the flexible use of terminal variables
within the body of an sfun, permitting a variable of type \\( Int \\), for example, to occur in a context where terms
of type \\( Int[ \uparrow x ] \\) are expected. A constant function \\(f\\), after all, is monotone: \\( v_1 \leq v_2 \\)
implies \\( f(v_1) = c \leq c = f(v_2) \\).

We're not building lifted typing derivations just for fun. Typically, a type system comes with a soundness theorem
stating that whenever a typing judgment of the form \\( \Gamma \vdash t : T \\) is derivable, the execution of the
term \\(t\\) (a program) under some well-defined model of computation (typically defined along with the type system) satisfies
some desirable property. In our system, a terminal typing derivation \\( \Gamma \vdash t : T \\) implies that
when the free variables of t are substituted with appropriately-typed values,
the execution of the term \\( t \\) is guaranteed to terminate, producing a value of type \\(T\\) as its result. This is not
a terribly unusual soundness guarantee. However, to provide semantics for lifted typing judgments, we introduced a new
reduction relation (or "computation model") which can be viewed in one of two ways:

1. The simultaneous reduction of an sfun, under terminal reduction, when applied to all sets of arguments in its domain.
2. The composition of an sfun's components, before the sfun is ever applied.

Point 1 is essentially the motivation for having lifted typing and lifted reduction in the first place. We want to know
how the sfun behaves under terminal reduction, across multiple applications--specifically two applications in the case of monotonicity. If the lifted reduction of an sfun's body faithfully simulates the terminal reduction of all possible
applications simultaneously, then the body of a well-typed sfun should normalize to an ambient map that is extensionally
equivalent to the sfun's applicative behavior under terminal reduction.
Therefore, if our soundness theorem guarantees that the derivability of
\\( \cdot;x:Int;x:Int[=~x] \vdash plus(x,x) : Int[\uparrow~x] \\)
implies that \\( plus(\{ (0,0), (1,1), \ldots \},\{ (0,0), (1,1), \ldots \} ) \\) normalizes under lifted reduction to a
monotone ambient map, we then know that the sfun \\( \tilde{\lambda} x : Int. plus(x,x) \\) behaves monotonically under terminal reduction. It's important to note that our model never requires us to actually perform lifted reduction; lifted reduction matters because not because we actual want to perform it, but instead because lifted typing derivations guarantee the existence of certain lifted reduction sequences which have implications for terminal reduction.  

Point 2 inspires our lifted type system. If an sfun is composed of monotone functions, we can use facts about preservation
of monotonicity across function composition to prove the sfun itself monotone. The difference between terminal reduction
and lifted reduction is demonstrated by the two mathematical expressions \\( f(g(v)) \\) and \\( (f \circ g) (v) \\). 
The expression \\( f(g(v)) \\) presents function composition as viewed by a standard type systems: to apply the composition
of \\(f\\) and \\(g\\) to a value \\(v\\), we first apply \\(g\\) to \\(v\\), and then apply \\(f\\) to the result.
This isn't wrong, but if \\( f \\) and \\( g \\) are both monotone, the monotonicity of the composite function as a whole
becomes self-evident if we first perform the "lifted reduction step" \\( f(g(v)) \to (f \circ g) (v) \\).

We'll leave you with an aspirational example, which demonstrates the need for a type system, rather than a more monolithic form of analysis, for proving functions monotone. Recall our replicated counter example from the introduction.
It isn't sufficient to store this counter as an integer. The problem is that replicas cannot synchronize properly without
knowing which how many increments were performed at each replica. Suppose that replicas X and Y each start with a count of zero.
The following actions are then performed:

1.) X increments, resulting in a count of 1
2.) X sends a synchronization message to Y, containing X's count 1
3.) X receives a synchronization message from Y containing a count of 1

At stage 3, X does not know if the received message was sent from Y before or after Y received the synchronization message
from stage 2. Replica X therefore does not know whether to set its count to 1 or 2. To avoid this problem, a replicated
counter is commonly represented as a map, which maps each replica identifier (a natural number) to the number of increments
that replica has performed (also a natural number). It is assumed that any replica id not contained in the map's finite
representation maps to 0. Such counters are called GCounters, and described in detail by \[[Shapiro et al. (2011)](#ref3)\].

GCounters are partially ordered componentwise. We write \\( v[a] \\) for the natural number to which the GCounter
\\(v\\) maps the replica identifier \\(a\\), and we write \\( \leq \\) for the standard ordering on natural numbers.
The partial order \\( \leq' \\) on GCounters is then defined such that
\\( v \leq' w \\) whenever for all replica identifiers \\(a\\) we have \\( v[a] \leq w[a] \\).

\[[Meiklejohn et al. (2015)](#ref1)\] motivates combinators for replicated data types such as the GCounter, but requires
that such combinators are monotone separately in each argument. Below is psuedocode for a monotone GCounter addition
combinator, annotated with monotonicity types. NatMap is used as the type of maps from natural numbers to natural numbers.
Several primitives are defined for working with NatMap. getAt retrieves the kth element of a NatMap m. joinAt returns
a new NatMap which is equal to the argument m, except that it maps k to the maximum of m[k] and n.
span returns the greatest key mapping to a non-zero value. emptyMap is a NatMap which maps every natural number to 0.
+ and > are standard arithmetic operators for working with natural numbers.

```
getAt :: (m : NatMap, k : Nat) ⇒ Nat[↑ m, ? k]
joinAt :: (m : NatMap, k : Nat, n : Nat) ⇒ NatMap[↑ m, ? k, ↑ n]
span :: (m:NatMap) ⇒ Nat[↑ m]
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
 GCounter { map = sumCell 0 initMap }
```

While our system can handle much of this example, it can't handle everything yet, for several reasons. First, it involves an if condition which depends on the arguments of the enclosing sfun. To handle this, we would need to incorporate the notion of domain restriction into lifted reduction. Second, it involves recursion. This is problematic for us, because our system utilizes the fact that all well-typed programs terminate. We could partially address this by adding terminating fixpoint combinators, which allow recursion given some well-founded termination metric, as in \[[Vazou et al. (2014)](#ref5)\]. However, that would not be adequate for this particular function. Since it could require arbitrarily many levels of recursion depending on which values are supplied as arguments, lifted reduction, which simulates an application to all arguments simultaneously, would diverge.

So there's still much to do! If you're interested in more details behind the type system, have a look at Kevin's blog article, [Monotonicity Through Types](https://kevinclancy.github.io/2017/11/09/monotonicity-through-types.html), or have a look at the full [Monotonicity Types](https://infoscience.epfl.ch/record/231867) preprint for more.

### References


<span id="ref1">C. Meiklejohn and P. Van Roy. _Lasp: A language for distributed, coordination-free programming._ In Proceedings of the 17th International Symposium on Principles and Practice of Declarative Programming, PPDP ’15, pages 184–195, New York, NY, USA, 2015. ACM.</span>

<span id="ref2">N. Conway, W. R. Marczak, P. Alvaro, J. M. Hellerstein, and D. Maier. _Logic and lattices for distributed programming_. In Proceedings of the Third ACM Symposium on Cloud Computing, SoCC ’12, pages 1:1–1:14, New York, NY, USA, 2012. ACM.</span>

<span id="ref3">M. Shapiro, N. Preguiça, C. Baquero, and M. Zawirski. _Conflict-Free replicated data types_. In Stabilization, Safety, and Security of Distributed Systems, Lecture Notes in Computer Science, pages 386–400. Springer, Berlin, Heidelberg, Oct. 2011.</span>

<span class="ref4">L. Kuper and R. R. Newton. _LVars: Lattice-based data structures for deterministic parallelism_. In Proceedings of the 2nd ACM SIGPLAN Workshop on Functional High-performance Computing, FHPC ’13, pages 71–84, New York, NY, USA, 2013. ACM.</span>

<span class="ref5">N. Vazou, E. L. Seidel, R. Jhala, D. Vytiniotis, and S. Peyton-Jones. _Refinement types for Haskell_. SIGPLAN Not. 49, 9 (August 2014), 269-282. </span>
