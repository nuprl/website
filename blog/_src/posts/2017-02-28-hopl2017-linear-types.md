    Title: Linear Types for Low-level Languages
    Date: 2017-02-28T09:51:55
    Tags: HOPL, by Daniel Patterson

<!-- more -->

In this talk, we covered early papers (primarily, by Girard, Lafont, and
Abramsky) on linear logic and its reflections into computation. The goal was to
understand why linearity is often turned to as a principled way to control
resource usage, as shows up in a language like Rust. From the very beginning,
researchers realized the implications for "low-level" languages - that linear
resources would eliminate the need for garbage collection, allow in-place
mutation, and enable safe parallel computation. However, pure implementations
of linearity incur lots of copying, doing away with any efficiency gained, and
we covered a survey of papers that attempted to reconcile this contradiction by
weakening linearity in controlled ways.

Notes:

- [https://github.com/nuprl/hopl-s2017/blob/master/lecture_notes/2017-02-14.md](https://github.com/nuprl/hopl-s2017/blob/master/lecture_notes/2017-02-14.md)

- - -

Just after the talk, over lunch, we had a lab discussion about the phrase
"low level". Here are some thoughts:

- the phrase is relative, both over time and depending on the programming
  task at hand
- a "low level" task is "one that you shouldn't need to worry about" while
  solving your current task

And here are some example "low-level" tasks:

- Time and space management is "low level" when designing a new algorithm
  (the first question is correctness)
- Calling conventions and endian-ness (facets of the damn machine running
  the programs) are almost always low-level
- Whether a given value is serializable is usually low-level
- Possible side effects, thrown exceptions, and optional arguments can all
  be considered "low level" aspects of library functions. This is low-level
  in the sense that "I'd rather use a simpler type to think about this library"
- Managing type annotations is a low-level detail in ML programs
