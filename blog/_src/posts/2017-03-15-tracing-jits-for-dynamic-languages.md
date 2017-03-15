    Title: Tracing JITs for Dynamic Languages
    Date: 2017-03-15T10:54:39
    Tags: HOPL, by Ming-Ho Yee

<!-- more -->

Traditional JIT (just-in-time) compilers are method-based: they compile "hot"
(i.e. frequently executed) methods to native code. An alternative is trace-based
or tracing JITs, where the compilation unit is a (hot) sequence of instructions.
Typically, such sequences of instructions correspond to loops, where programs
spend most of their execution time.

Where did the idea of tracing come from? What was appealing about it? How was
tracing adapted for JITs and dynamic languages? What happened to Mozilla's
TraceMonkey, which used to be part of Firefox? Do any JITs today use tracing?

In this talk, I trace tracing JITs from their origins to some of their recent
developments. I cover five papers: the original tracing paper, an implementation
of a tracing JIT for Java, the TraceMonkey JIT for JavaScript, PyPy's
"meta-level" tracing, and a specific class of optimizations for tracing JITs.

*(The idea of using the phrase "trace tracing JITs" is from Matthias
Felleisen.)*

All materials can be found in the [course repository](https://github.com/nuprl/hopl-s2017/tree/master/tracing-jit):

  - [Full notes](https://github.com/nuprl/hopl-s2017/blob/master/tracing-jit/notes.pdf)
  - [Annotated bibliography](https://github.com/nuprl/hopl-s2017/blob/master/tracing-jit/annotated.txt)
