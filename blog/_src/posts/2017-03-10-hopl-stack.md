    Title: Type Inference in Stack-Based Programming Languages
    Date: 2017-03-10T16:23:30
    Tags: HOPL, by Rob Kleffner

<!-- more -->

Stack-based languages occupy a niche in today's programming language
environment. The predominant stack-based language in use by programmers is
Forth, and is found mostly on embedded devices. These languages also find use
as compile targets for more popular languages: the CIL and JVM are both
stack-based. Less popular but highly interesting languages to mention include
[Joy](http://www.kevinalbrecht.com/code/joy-mirror/joy.html) and
[Factor](http://factorcode.org/), known for their emphasis on higher-order
stack-based programming.

The majority of stack-based languages are not statically typed, and it would be
a stretch to call Forth even dynamically typed. As such, developing large
projects in Forth or Factor can require great discipline on the part of the
programmer to avoid type errors.

In this talk, I presented the development of type inference for stack-based
languages as a linear sequence, divided into two overarching segments:

- An algebraic system known as *stack effects*
- Systems that can be encoded as *nested pairs* in standard functional
  programming languages

The thread of research on stack effects began with Jaanus Pöial in the early
1990's, and is a formalization of a commenting style well-known in the Forth
community. The nested tuple systems were first examined by Okasaki in 1993 in
the context of Haskell, and were later applied to higher-order stack-based
languages. At the end, I give some avenues for extending the research on these
systems, and list some pitfalls to be avoided in further research.

Full notes (as PDF documents) -- see the [git repository](https://github.com/nuprl/hopl-s2017/tree/master/type-inference-for-stack-languages) for more documents:

- [Talk notes](/blog/static/stack-languages-talk-notes.pdf)
- [Annotated bibliography](/blog/static/stack-languages-annotated-bib.pdf)

