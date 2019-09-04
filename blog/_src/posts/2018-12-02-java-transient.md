    Title: Java and Migratory Typing
    Date: 2018-12-02T14:41:53
    Tags: migratory typing, java, by Ben Greenman

The _transient_ approach to migratory typing (circa [2014](http://homes.sice.indiana.edu/mvitouse/papers/dls14.pdf))
  is similar to type erasure in Java (circa [2004](https://docs.oracle.com/javase/1.5.0/docs/relnotes/features.html))
  in a few interesting ways.

<!-- more -->

## Migratory typing

The goal of _migratory typing_ is to enrich the type system of a language
 without breaking backwards compatibility.
Ideally, code that uses the enriched types:

- (G1) benefits from new ahead-of-time checks,
- (G2) benefits from stronger run-time guarantees, and
- (G3) may interact with all kinds of existing code.

There are tradeoffs involved in the implementation of a migratory typing
 system, however, and (as we will see) different implementations may focus on
 different goals than the three above.

A typical migratory typing system adds a static type checker to a dynamically
 typed language ([examples](/blog/2018/10/06/a-spectrum-of-type-soundness-and-performance/index.html)),
 but one could also extend the type system of a statically-typed language;
 for example, by [adding dependent types](https://hal.inria.fr/hal-01629909v2).
In this sense, Java 1.5.0 is a migratory typing system for pre-generics Java.
The addition of generic types enabled new ahead-of-time checks and maintained backwards
 compatibility with existing Java code.

Java's implementation of migratory typing has some interesting things in common
 with the _transient_ implementation strategy recently proposed by
 Michael Vitousek and collaborators ([DLS'14](http://homes.sice.indiana.edu/mvitouse/papers/dls14.pdf), [POPL'17](https://mail.google.com/mail/u/0/h/1atrn21qlyrrh/?&)).
The goal of this post is to demonstrate the connections.


## Erasure migratory typing

Before we compare Java 1.5.0 to transient, let's review a simpler strategy:
 the _erasure_ approach to migratory typing.

[TypeScript](https://www.typescriptlang.org/) is a great (modern) example of the erasure approach.
TypeScript is a migratory typing system for JavaScript.
A TypeScript module gets validated by an ahead-of-time type checker and
 compiles to JavaScript.
After compilation, any JavaScript program may import bindings
 from the generated code.
Conversely, a TypeScript module may import bindings from a JavaScript module
 by declaring a static type for each binding.

> The [DefinitelyTyped](http://definitelytyped.org/) repository provides
> TypeScript type definitions for many JavaScript libraries.

The TypeScript compiler erases types;
 every type `T` in the source code translates to the universal "JavaScript type".
For instance, a TypeScript function declaration compiles to an untyped
 JavaScript function:

```
(function (n0 : number, n1 : number) { return n0 + n1; })

// ==(compiles to)==>

(function (n0, n1) { return n0 + n1; })
```

TypeScript satisfies goals **G1** and **G3** for a migratory typing system
 because its type checker adds ahead-of-time checks and its
 compiler outputs JavaScript.
TypeScript does not satisfy goal **G2** because the compiler erases types.
In terms of the example above, the compiled function may be invoked with any
 pair of JavaScript values; the variable `n0` is not guaranteed to point
 to a `number` at run-time.
On one hand, this means the type annotations have no effect on the behavior
 of a program --- and in particular, cannot be trusted for debugging.
On the other hand, it means that an experienced JavaScript programmer can
 re-use their knowledge to predict the behavior of a TypeScript program.

In an ordinary program, the run-time guarantees of TypeScript are simply
 the run-time guarantees of JavaScript:

- if a TypeScript expression `e` has the static type `T` and evaluates to
 a value `v`,
- then the only guarantee is that `v` is a valid JavaScript value
  (e.g., `T` could be `number` and `v` could be an incompatible object).


## Transient migratory typing

[Reticulated](https://github.com/mvitousek/reticulated) is a migratory typing
 system for Python that follows a _transient_ implementation strategy.
A Reticulated module gets type-checked and compiles to a Python module that
 defends itself from certain type-invalid inputs through the use of
 assertions that run in near-constant time.
The type-checking addresses goal **G1**,
 the compilation to Python provides interoperability (goal **G3**),
 and the assertions partially meet goal **G2**.

> These _certain_ inputs are the ones that would cause a standard typed
> operational semantics to reach an undefined state.
> For a discussion of _near-constant_, see
> [_On the Cost of Type-Tag Soundness_, section 2](http://www.ccs.neu.edu/home/types/publications/publications.html#gm-pepm-2018).

For example, here is a Reticulated function
 that computes the average of a list of numbers:

```
# Reticulated (commit e478343)
def average(nums : List(Float)) -> Float:
  if ns:
    return sum(ns) / len(ns)
  else:
    raise ValueError("average: expected non-empty list")
```

and here is the Python code it compiles to:

```
from retic.runtime import *
from retic.transient import *
from retic.typing import *

def average(nums):
    check_type_list(nums)
    if ns:
        return check_type_float((check_type_function(sum)(ns) / check_type_function(len)(ns)))
    else:
        raise check_type_function(ValueError)('average: expected non-empty list')
```

> Note: the Reticulated syntax for type annotations is similar to the one
> proposed in [PEP 484](https://www.python.org/dev/peps/pep-0484/),
> but not identical. For example, Reticulated does not
> require forward references to be embedded in strings.

The Reticulated compiler removes all type annotations and inserts `check_type`
 assertions throughout the code.
In `average`, these assertions check that: (1) the input is a list,
 (2) the output is a `float`, (3) and the names `sum` `len` and
 `ValueError` point to callable values.
That's all.
The assertions **do not check** that `nums` contains only floating-point
 numbers.

> The assertions also do not check that the function bound to `sum` is defined
> for a single argument, which is arguably a bug.
> Scaling a model to an implementation is always challenging.

If `nums` contains something other than floating point numbers, then the
 call to `average` may cause `sum` to raise an exception or it may silently
 compute a nonsense result.
The behavior depends on the implementation of `sum` in the same way that
 the behavior of a TypeScript function depends on any JavaScript functions
 that it invokes.

Reticulated does not erase types, nor does it fully enforce types.
Every type in a Reticulated module translates to its
 top-level type constructor `C(T)`, e.g.:

```
  C(Float)                = Float
  C(List(Float))          = List
  C(List(Float) -> Float) = ->
```

Consequently, Reticulated has a slightly stronger run-time guarantee than Python:

- if `e` is an expression with static type `T` that evaluates to a value `v`,
- then `v` is guaranteed to have a top-level shape that matches the `C(T)`
  constructor.


## Java migratory typing

Java 1.5.0 added [generic types](https://www.jcp.org/en/jsr/detail?id=14)
 to the Java 1.4.0 type system.
The benefit of generics is that a programmer can:
 write one class definition,
 use the definition in a few different contexts,
 and receive specific feedback from the type checker in each context.

### Review: generic types

Suppose we want to write a `Box` class that holds some kind of value;
 the value could be an `Integer` or a `String` or anything else.
Here is a pre-generics definition:

```
class Box {
  private Object val;

  public Box(Object val) { this.set(val); }

  public void set(Object val) { this.val = val; }

  public Object get() { return this.val; }
}
```

With this definition is it possible to make boxes that hold different types
 of values:

```
// good!
Box iBox = new Box(new Integer(4));
Box sBox = new Box(new String("X"));
```

but it is also possible to "change the type" of the contents of a `Box`:

```
// maybe bad!
iBox.set(new String("not a number"));
```

and some calls to `get` must be followed by a type cast:

```
// annoying!
((String) sBox.get()).charAt(0);
```

- - -

With generics, we can give a name (e.g. `ValType`) to "the type of the value inside a box":

```
class GBox<ValType> {
  private ValType val;

  public GBox(ValType val) { this.set(val); }

  public void set(ValType val) { this.val = val; }

  public ValType get() { return this.val; }
}
```

and now we can tell the type checker to check different boxes differently (satisfying goal **G1**):

```
GBox<Integer> iBox = new GBox<Integer>(new Integer(0));
GBox<String> sBox = new GBox<String>(new String("A"));

// iBox.set(new String("not a number")); // Type Error, good!

sBox.get().charAt(0); // no cast, good!
```


### Backwards compatibility & danger

Java generics are backwards-compatible with older code (goal **G3**).
This means that pre-generics code can interact with instances of a generic
 class.
Vice-versa, generic code can interact with pre-generics classes.
Since pre-generics code is not aware of type parameters, these interactions
 are potentially unsafe.
For example, a pre-generics method can change the type of a `GBox`:

```
// Java 1.4.0 method
public static void evil(GBox b) { b.set(666); }

// Java 1.5.0 method
public static void test() {
  GBox<String> sBox = new GBox<String>(new String("A"));
  evil(sBox); // OK, but generates unchecked warning
  sBox.get().charAt(0);
}
```

The code above passes the type checker (with a warning about the `evil` method),
 and so it _seems_ as though running the code will run the nonsense
 method call `666.charAt(0)` and lead to evil behavior.
The actual result, however, is a cast error immediately after the call
 `sBox.get()` returns.

Based on the cast error, we can tell that
 the compiler does not trust the type `GBox<String>` and
 inserts a run-time check that the result of the `.get()` is a string object.

> "Calling legacy code from generic code is inherently dangerous; once you mix
> generic code with non-generic legacy code, all the safety guarantees that the
> generic type system usually provides are void."
> [Generics in the Java Programming Language, Section 6.1](https://www.oracle.com/technetwork/java/javase/generics-tutorial-159168.pdf)


### Run-time guarantees

In order to support pre-generics and post-generics code on the same
 [virtual machine](https://docs.oracle.com/javase/specs/jvms/se11/html/index.html),
 the Java compiler [erases](https://docs.oracle.com/javase/specs/jls/se11/html/jls-4.html#jls-4.6)
 generic type parameters after type-checking.
Everywhere that the compiled code depends on an erased type, such as the
 `String` in `GBox<String>` above, Java adds a cast to prove to the Java Virtual Machine (JVM)
 that the erased bytecode is type-safe.
(A smarter JVM type system might be able to prove that some casts are
 unnecessary via [occurrence typing](https://www2.ccs.neu.edu/racket/pubs/icfp10-thf.pdf).)

> "The decision not to make all generic types [not erased] is one of the most crucial, and controversial design decisions involving the type system of the Java programming language.
>
> "Ultimately, the most important motivation for this decision is compatibility with existing code."
> [Java Language Specification, section 4.7](https://docs.oracle.com/javase/specs/jls/se11/html/jls-4.html#jls-4.7)

By contrast to Reticulated's `C(T)` transformation, the following `G(T)`
 transformation describes generic-type erasure,
 where `T<T1>` describes a type `T` with parameter `T1`
 and `A[T1, T2]` describes a type variable `A` with lower bound `T1` and upper bound `T2`:

```
  G(T<T1>)     = G(T)
  G(A[T1, T2]) = G(T1)
  G(T)         = T      otherwise
```

If generic-type erasure results in a type mismatch (e.g., in `sBox.get().charAt(0)` above),
 the compiler inserts a cast.
The inserted casts led to the runtime error in the previous example, and
 provide the following run-time guarantee:

- if `e` is an expression with static type `T` that evaluates to a value `v`,
- then `v` is guaranteed to match the (bytecode) type `G(T)`


## Discussion

TypeScript, Reticulated Python, and Java 1.5.0 each improved the type system
 of an existing language, but maintained backwards compatibility with existing
 code.
The name [migratory typing](http://drops.dagstuhl.de/opus/volltexte/2017/7120/)
 describes this kind of language extension.

> [Gradual typing](http://drops.dagstuhl.de/opus/volltexte/2015/5031/)
> is a similar; a gradual type system starts with a statically-typed language
> and adds dynamic typing in a principled way ([example](https://pleiad.cl/papers/2016/garciaAl-popl2016.pdf)).

The TypeScript team had a choice between erasing types and enforcing types.
They chose to erase types and run all code (typed or untyped) at the level
 of JavaScript.
(Some TypeScript [libraries](https://lorefnon.tech/2018/03/25/typescript-and-validations-at-runtime-boundaries/), however, can enforce some types.)

> TypeScript is not the only erasure language, nor is it the first.
> The oldest (I think) is [MACLISP](http://www.softwarepreservation.org/projects/LISP/maclisp_family/).
> For an erasure manifesto, see [Pluggable Type Systems](http://bracha.org/pluggableTypesPosition.pdf).

The Reticulated team faced an analogous choice, and chose to enforce the top-level
 shape of values in typed code ([POPL 2017](http://homes.sice.indiana.edu/mvitouse/papers/popl17.pdf)).
It will be interesting to see if this guarantee helps developers maintain programs,
 or if it is too shallow to be much use.
The [Pyret](https://www.pyret.org/index.html) language has been successful with
 comparable shallow checks.

> Note: the POPL 2017 paper advertises an "open-world soundness", but I do not
> see how this idea is different from the older idea of soundness in a
> multi-language system ([TOPLAS 2009](https://www.eecs.northwestern.edu/~robby/pubs/papers/toplas09-mf.pdf), [DLS 2006](https://www2.ccs.neu.edu/racket/pubs/dls06-tf.pdf)).

Similarly, the Java team chose to erase generic types in Java 1.5.0 and use
shallow casts in the JVM.
The casts around type-erased generics provide a minimal level of safety ---
 enough to prevent the use of a generic object from corrupting the state of a
 VM instance.

Alternatively, Java could enforce full generic types at run-time.
Over the years there have been a few proposals to do so ([example 1](http://gafter.blogspot.com/2006/11/reified-generics-for-java.html),
 [example 2](https://wiki.openjdk.java.net/display/valhalla/Main)).
The C# language has a similar type system and enforces
 generics at run-time (sources:
 [blog post](https://mattwarren.org/2018/03/02/How-generics-were-added-to-.NET/),
 [PLDI 2001 paper](https://www.microsoft.com/en-us/research/publication/design-and-implementation-of-generics-for-the-net-common-language-runtime/),
 [backup link to paper](https://dl.acm.org/citation.cfm?doid=378795.378797))


## Acknowledgments

Thank you to [Ryan Culpepper](https://github.com/rmculpepper) and [Jesse Tov](http://users.eecs.northwestern.edu/~jesse/) for noticing the similarity between
 Java's generic-type erasure and transient migratory typing.
Jesse helped improve an early version of this post.
