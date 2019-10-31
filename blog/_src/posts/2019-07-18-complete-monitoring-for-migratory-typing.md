    Title: Complete Monitors for Gradual Types
    Date: 2019-07-18T14:33:47
    Tags: migratory typing, gradual typing, complete monitoring, extended abstract, by Ben Greenman

Syntactic type soundness is too weak to tell apart different ways of running
 a program that mixes typed and untyped code.
Complete monitoring is a stronger property that captures a meaningful
 distinction --- a language satisfies complete monitoring iff it checks
 all interactions between typed and untyped code.

<!-- more -->

> Note: this post is an extended abstract for the paper _Complete Monitors
> for Gradual Types_ by Ben Greenman, Matthias Felleisen, and Christos Dimoulas.
> For the full paper, proofs, and slides,
> [click here](http://www.ccs.neu.edu/home/types/publications/publications.html#gfd-oopsla-2019).


### Example: Clickable Plot

The program below has a subtle bug.
Can you find it?

<img src="/img/complete-monitoring-0.png" alt="Untyped client code, a typed API, and untyped library code."/>

First of all, this pseudocode program combines three chunks of code:

- On the left, an **untyped** client script defines a function `h` that expects
  a pair of numbers and returns an image. The client uses this function to
  create a `ClickPlot` object, and then displays the plot --- ideally in a new
  GUI window.

- In the center, a **typed** API file describes a `ClickPlot` object as
  something with one constructor and two methods. The constructor expects
  a function; according to the type, such functions can expect a pair of
  numbers and must compute an image. The `mouseHandler` method expects
  a `MouseEvt` object and returns nothing. The `show` method expects no arguments
  and returns nothing. (Presumably, these methods have side effects.)

- On the right, an **untyped** library module implements a `ClickPlot` object.
  Most of the code is omitted (`...`), but the `mouseHandler` method sends
  its input directly to the `onClick` callback.

The **bug** is in the API --- in the type `([N, N]) => Image`.
This type promises that a given function can expect a pair of numbers,
 and indeed the client script's callback `h` expects a pair.
But the library code on the right passes a `MouseEvt` object to its callback.

What happens when we run this program in a type-sound mixed-typed language?
Does `h` receive the invalid input?

As it turns out, type soundness cannot say.
A type sound language may choose to enforce or ignore the fact that the
 API promises a pair of numbers to the client.


### Type Soundness is Not Enough

Sound types are statements about the behavior of a program.
A normal type soundness theorem for a typed language says that a well-typed
 program can either compute a value of the same type, compute forever (diverge),
 or stop with an acceptable error (perhaps division by zero).
No other behaviors are possible.

> **Classic Type Soundness**
>
> If `e : T` then one of the following holds:
>
> - `e -->* v` and `v : T`
> - `e` diverges
> - `e -->* OkError`

A mixed-typed language needs two "type soundness" theorems:
 one for typed code and one for untyped code.
The **typed** soundness theorem can resemble a classic theorem.
The **untyped** soundness theorem is necessarily a weaker statement due to
 the lack of types:

> **Mixed-Typed Soundness**
>
> If `e : T` then one of the following holds:
>
> - `e -->* v` and `v : T`
> - `e` diverges
> - `e -->* OkError`
>
> And if `e` is untyped then one of the following holds:
>
> - `e -->* v` and `v` is a proper value
> - `e` diverges
> - `e -->* OkError`

Now we can see why mixed-typed soundness is not strong enough to guarantee that
 the callback `h` in the code above receives a pair value.
We have an **untyped** function called from an **untyped** context --- since
 there are no types sitting right there, type soundness has nothing to say
 about types!

<img src="/img/complete-monitoring-1.png" alt="Untyped library sends input directly to untyped client."/>

Nevertheless, this channel of communication between the library and client
 arose through the typed API.

TODO


### Complete Monitoring

from contracts,
....



<!-- http://prl.ccs.neu.edu/blog/2018/10/06/a-spectrum-of-type-soundness-and-performance/ -->

