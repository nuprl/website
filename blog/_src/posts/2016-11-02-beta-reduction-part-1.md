    Title: Beta Reduction (Part 1)
    Date: 2016-11-02T21:10:18
    Tags: lambda,calculus,beta,reduction,semantics,by Milo Davis

This post came about because of a research project I became involved with around a year and a half ago in the PRL.  At the time, I had just completed my freshman year and the field of programming languages was completely new to me.  The project involved proving confluence for a restricted subset of β-reduction.  The concepts I had to work with weren't ones that I was familiar with, so I looked online for an approachable explanation, but none existed.  This series of posts is my attempt to rectify this.  This initial post introduces the λ-calculus and β-reduction.  In a follow up post, I will discuss the proofs.

<!-- more -->


# The λ-Calculus

The λ-calculus is a simple model of computation developed by Alonzo Church.  The λ-calculus has three different syntactic forms: variables, anonymous functions (lambdas), and function application.  The BNF for the λ-calculus is as follows:

```
e ::= x
   | λx.e
   | e e
```
In the above BNF, x is a metavariable, standing for any variable.  In this post, I use x, y, z, a, and b as variables in my examples.  The λx.e term represents a function with a single parameter x.  When you apply this function to an argument, you substitute the argument for every occurrence of x in the function's body, which is the expression after the period.  We say that x is bound in e.  It is possible to have unbound variables in the λ-calculus, though for the most part, we can ignore them in our discussion of this topic.  Finally, applications consist of two expressions.  The first expression evaluates to a function and the second to its argument.  Parentheses can be added for clarity.

If you program regularly in a functional language, this might look fairly familiar to you.  In fact, you can encode every construct in whatever programming languages you use into just the above constructs.  The λ-calculus is Turing complete.  Of course, you wouldn't want to do program in this language as it's significantly harder than just using built in language constructs like numbers.  I'm not going to discuss these ideas in detail, but if you're interested in how to add numbers, booleans, conditionals, and recursion to the λ-calculus, Matt Might has a few great posts about how to do this using equivalent constructs in [Python](http://matt.might.net/articles/python-church-y-combinator/), [Scheme](http://matt.might.net/articles/church-encodings-demo-in-scheme/), and [JavaScript](http://matt.might.net/articles/js-church/).

Now that we've discussed the syntax of the language, we can look at the semantics, or how terms evaluate?  Below I have the evaluation rules for the λ-calculus.  The arrow represents β-reduction which is the subject of this post.  The semantics rely on substitution which is depicted with brackets.  Below, I define the substitution function.  I'm assuming the Barendregt variable convention which states that every bound variable is distinct from every free variable.

```
x[ x := e ] = e
x[ y := e ] = y
(λx.e1)[ x := e2 ] = (λx.e1[ x := e2 ])
(e1 e2)[ x := e3 ] = (e1[ x := e3 ] e2[ x := e3 ])
```

With the substitution function defined, we can write a semantics for evaluation:

```
------------------------------
  (λx.e1) e2 ->β e1[ x := e2 ]

    e1 ->β e1'
--------------------
  e1 e2 ->β e1' e2

    e2 ->β e2
--------------------
  e1 e2 ->β e1 e2'
```

These rules mean that if you have a call in which you have a function applied to the argument, you substitute the argument into the function.  The next two rules say that if you can perform an evaluation step in either the first or second term, you can perform that step.  Notice that both the second and third rules might apply to a single term.  These rules are nondeterministic and in these cases it is fine to use whichever rule you want.

# What is β-reduction?

More generally, what is reduction?  Intuitively, a reduction system is a set of rules that determine how a term is stepped forwards in the computation.  β-reduction is probably the most common reduction system used in programming languages.  It is essentially just the substitution semantics we saw above.  A single β-reduction is as follows

```
(λx.e1) e2 = e1[ x := e2 ]
```

Given that definition, lets look at a simple example.  I've written the examples in the λ-calculus, Racket, OCaml, and Haskell.  It's important to note that these languages have more restricted semantics than the original λ-calculus.  In Racket and OCaml, it is not possible to substitute with anything except for a value, meaning you need to evaluate the argument until it is a function before substituting.  This makes the evaluation reduce left to right before substituting.  Haskell, in contrast, no reduction occurs in arguments, meaning that we would omit the third rule.  

# Some Examples
The first example evaluates the same way in all of the languages.

```
    (λx.x) (λy.y)
->β x[ x := (λy.y) ]
=   (λy.y)
```

```racket
    ((λ (x) x) (λ (y) y))
->β x[ x := (λ (y) y) ]
=   (λ (y) y)
```

```ocaml
    (fun x -> x) (fun y -> y)
->β x[x := (fun y -> y) ]
=   x[x := (fun y -> y) ]
```

```haskell
    (\x -> x) (\y -> y)
->β x[ x := (\y -> y) ]
=   (\y -> y)
```

In our next example, we will see the differences between the languages.  They all reduce to the same thing, albeit in different ways.

```haskell
    (\x -> \y -> y) ((\z -> z) (\a -> a))
->β (\y -> y)[x := ((\z -> z) (\a -> a))]
= (\y -> y)
```

Note that the application on the right hand side is never evaluated.  In an eager language like Racket or OCaml, the term being substituted must be a value, so the evaluation follows a different path.

```racket
    ((λ (x) (λ (y) y)) ((λ (z) z) (λ (a) a)))
->β ((λ (x) (λ (y) y)) (z[ z := (λ (a) a) ]))
=   ((λ (x) (λ (y) y)) (λ (a) a))
->β (λ (y) y)[ x := (λ (a) a) ]
=   (λ (y) y)
```

```ocaml
    (fun x -> (fun y -> y)) ((fun z -> z) (fun a -> a))
->β (fun x -> (fun y -> y)) (z[ z := (fun a -> a) ])
=   (fun x -> (fun y -> y)) (fun a -> a)
->β (fun y -> y)[ x := (fun a -> a) ]
=   (fun y -> y)
```

The nondeterministic semantics of the λ-calculus lead to two possible paths through the above computation:

```
    (λx.λy.y) ((λz.z) (λa.a))
->β (λx.λy.y) (z[ z := (λa.a) ])
=   (λx.λy.y) (λa.a)
->β (λy.y)[ x := (λa.a) ]
=   (λy.y)
```

```
    (λx.λy.y) ((λz.z) (λa.a))
->β (λy.y)[ x := ((λz.z) (λa.a)) ]
=   (λy.y)
```

Lets look at a final example.  This one is more complicated than the previous ones.  I'm also going to stop showing the substitution explicitly.  In the literature, it is fairly common not to see it explicitly, but you should still be able to follow what's going on.

```racket
    ((lambda (x) x) (((lambda (a) (lambda (b) (a b))) (lambda (y) y)) (lambda (z) z)))
->β ((lambda (x) x) ((lambda (b) ((lambda (y) y) b)) (lambda (z) z)))
->β ((lambda (x) x) ((lambda (y) y) (lambda (z) z)))
->β ((lambda (x) x) (lambda (z) z))
->β (lambda (z) z)
```

The same thing happens in OCaml

```ocaml
    (fun x -> x) ((fun a -> (fun b -> a b)) (fun y -> y) (fun z -> z));;
->β (fun x -> x) ((fun b -> (fun y -> y) b) (fun z -> z));;
->β (fun x -> x) ((fun y -> y) (fun z -> z))
->β (fun x -> x) (fun z -> z)
->β (fun z -> z)
```

In Haskell, the situation is a little different:

```haskell
    (\x -> x) ((\a -> \b -> a b) (\y -> y) (\z -> z))
->β (\a -> \b -> a b) (\y -> y) (\z -> z)
->β (\b ->  (\y -> y) b) (\z -> z)
->β (\y -> y) (\z -> z)
->β (\z -> z)
```

Finally, in the λ-calculus, things can go a few different ways.

```
    (λx.x) ((λa.λb.a b) (λy.y) (λz.z))
->β (λx.x) ((λb.(λy.y) b) (λz.z))
->β (λx.x) ((λy.y) (λz.z))
->β (λy.y) (λz.z)
->β (λz.z)
```

```
    (λx.x) ((λa.λb.a b) (λy.y) (λz.z))
->β (λa.λb.a b) (λy.y) (λz.z)
->β (λb.(λy.y) b) (λz.z)
->β (λy.y) (λz.z)
->β (λz.z)
```

```
    (λx.x) ((λa.λb.a b) (λy.y) (λz.z))
->β (λx.x) ((λb.(λy.y) b) (λz.z))
->β (λb.(λy.y) b) (λz.z)
->β (λy.y) (λz.z)
->β (λz.z)
```

There's a very interesting property of all of those examples: they all evaluate to the same thing, regardless of the reduction order taken.  This is because β-reduction has the weak Church-Rosser Property.  In systems that have this property, if a reduction sequence terminates, it will always evaluate to the same term, regardless of the path taken.  A weaker property is called confluence, which states that all reduction sequences can be stepped to a common term.  At the beginning of this post, I said that the λ-calculus is a model for programming languages.  This is generally true.  There are proofs that the λ-calculus has this property, but people don't tend to prove such things about their actual languages, it is usually enough to build them knowing that their theoretical model has the property.  In a follow up post, I'll explain the proofs that the λ-calculus does indeed have these properties.
