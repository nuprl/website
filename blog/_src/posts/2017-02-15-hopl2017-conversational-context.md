    Title: Conversational Context and Concurrency
    Date: 2017-02-15T01:21:55
    Tags: HOPL, by Tony Garnock-Jones

<!-- more -->

When programs are written with concurrency in mind, the programmer reasons
about the interactions between concurrent components or agents in the program.
This includes exchange of information, as well as management of resources,
handling of partial failure, collective decision-making and so on.

These components might be objects, or threads, or processes, or actors, or some
more nebulous and loosely-defined concept; a group of callbacks, perhaps. The
programmer has the notion of an agent in their mind, which translates into some
representation of that agent in the program.

We think about the contexts (because there can be more than one) in which
agents exist in two different ways. From each agent's perspective, the
important thing to think about is the boundary between the agent and everything
else in the system. But from the system perspective, we often think about
*conversations* between agents, whether it's just two having an exchange, or a
whole group collaborating on some task. Agents in a conversation play different
roles, join and leave the group, and build shared conversational state.

In this talk, I used the idea of these *conversational contexts* as a lens
through which to view the development of various metaphors and mechanisms of
communication and coordination. I presented four *computational models* for
concurrent interaction:

- monitors, and shared memory concurrency generally
- the actor model
- channel-based communication
- tuplespaces

These aren't full programming languages, but there are many *programming
models* that build upon them. In some cases, development of these ideas has
progressed all the way up to *system models* including user interaction and so
forth.

The linked lecture notes include informal sketches of reduction semantics for
each of the four models, plus a handful of small examples to give a feel for
them.

Lecture Notes:

- [https://github.com/nuprl/hopl-s2017/tree/master/conversational-context-and-concurrency/index.md](https://github.com/nuprl/hopl-s2017/tree/master/conversational-context-and-concurrency/index.md)

Discussion summary:

- [https://github.com/nuprl/hopl-s2017/blob/master/lecture_notes/2017-01-31.md](https://github.com/nuprl/hopl-s2017/blob/master/lecture_notes/2017-01-31.md)

