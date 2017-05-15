    Title: Artifacts for Semantics
    Date: 2017-05-15T10:08:31
    Tags: by Daniel Patterson

[Gabriel Scherer](http://gallium.inria.fr/~scherer/) and I recently wrote
an [artifact](https://dbp.io/artifacts/funtal) for a
semantics [paper](https://dbp.io/pubs/2017/funtal.pdf) on a typed assembly
language interoperating with a high-level functional language. 

<!-- more -->

We wrote a interpreter, typechecker, and parser in OCaml, compiled it to
Javascript using [js_of_ocaml](http://ocsigen.org/js_of_ocaml/), and then put it
on a webpage (with an editor with syntax highlighting and error reporting) that
allows people to step through examples from the paper or write their own. (Feel
free to start by playing a bit
with [our artifact](https://dbp.io/artifacts/funtal)).

This post will summarize the different parts to make it easier for others to
repeat this process. We think it was a total success, and have gotten feedback
that it makes understanding the (somewhat complex) language from the paper much
easier. We argue that building such interpreters / typecheckers is easy enough
that all papers should do this. Further, while our interpreter / typechecker is
completely unverified, since we wrote it in OCaml, this approach should work
equally well for semantics verified in Coq and then extracted to OCaml.

------

The paper in question,
[FunTAL: Reasonably Mixing a Functional Language with Assembly](https://dbp.io/pubs/2017/funtal.pdf) (to
appear in PLDI17), presents a multi-language that incorporates a typed assembly
language (TAL) and a simple functional language where each can be embedded within the
other. The paper then develops a logical relation that can be used to reason
about the equivalence of such mixed programs. For example in the paper we show an
imperative register-based factorial and a functional factorial equivalent.

Both the static and dynamic semantics are relatively complex. The typed assembly
has registers (which store word-sized values), a heap (which stores code-blocks
and tuples), and a stack (not a call-stack, simply a place where word-sized
values can be pushed and popped). Code-blocks have pre-conditions on the state
of the registers and the stack, and allow the tail of the stack to be abstracted
over polymorphically. This allows values to be protected on the stack before
jumping to blocks that otherwise could change them. This is used, along with a
novel notion of **return markers**, to ensure well-bracketing in the control
flow of the typed assembly. The return markers indicate the location that points
to the block that will eventually be returned to (assuming it doesn't loop
infinitely). At the top level, the return marker `end` indicates that, assuming
it does not loop, eventually the program will stop, rather than returning
somewhere else. 

Understanding the dynamic semantics requires tracking how values flow through
the registers, the heap, and the stack, and rather than a call-stack, the user
has to track the control flow through the statically-enforced return markers.
This allows a good deal of low-level control-flow while still ensuring that
calls will eventually return to the right place. This well-bracketing is vital
to be able to reason about "components" that eventually return a value of a
particular type, a necessity when embedding these components in a typed
high-level program! However, it does mean that understanding the static and
dynamic semantics from a few rules alone is a tall order. Our functional
language is more standard, though we use (iso)-recursive types to allow
recursion, which can easily trip up people, especially when you don't have a
type-checker to catch typos!

For that reason, when working through examples for the paper I implemented a
simple interpreter for the multi-language. I did this in OCaml, in the most
straightforward way possible: by translating the definitions from the paper into
type definitions
([for F](https://github.com/dbp/funtal/blob/032be70f33f77e80f4fab7e62016bfabf96476f3/ftal.ml#L835) and
[for TAL](https://github.com/dbp/funtal/blob/032be70f33f77e80f4fab7e62016bfabf96476f3/ftal.ml#L1209)),
and the reduction relation into
a
["step" function](https://github.com/dbp/funtal/blob/032be70f33f77e80f4fab7e62016bfabf96476f3/ftal.ml#L1155) that
(assuming it wasn't stuck or a value), did one step of evaluation. Later, I did
the same thing for the type-checker, translating rules into
a
[type-checking function](https://github.com/dbp/funtal/blob/032be70f33f77e80f4fab7e62016bfabf96476f3/ftal.ml#L282).
The latter had to deviate from the rules in the paper in a few minor ways, as
the typing rules we had in the paper were slightly not syntax directed.

Having the interpreter and type-checker was very useful for me, as I could check
that the examples from the paper did not contain typos, but it was much less
useful as an artifact for a reader of the paper. To use it the reader would have to
download the source, install OCaml, write out examples as OCaml data
constructors in a test file, compile it, run it, and then interpret the (quite
overwhelming) output of every step of evaluation. At each step, I printed the
current registers, current stack, current heap, what the evaluation context was
(as you might be evaluating TAL instructions that were embedded inside a
functional program that, in turn, was embedded in further TAL instructions), and
what the current reduction was.

To get from that useful-for-the-authors artifact to a useful-to-readers artifact
requires doing three things:

1. Allow reading/writing programs in a notation as close to the paper as
   possible. In our paper we use superscripts, subscripts, and a few greek
   letters, but ended up with a syntax otherwise very close to the paper -- the biggest
   differences were a few extra delimiters introduced to reduce ambiguity. 
2. Present an interface that highlights type errors at the location they
   occurred in, and allow a reader to step forward and backwards through the
   evaluation. Printing console output traces is fine for authors, but adds too
   much effort for readers.
3. Put it online! Don't require installing any software! Conveniently,
   implementing 2 is also made easier once done online, as we could use existing
   editor tooling to present the code, highlight errors, etc. By using OCaml, we
   were able to easily use the
   excellent [js_of_ocaml](http://ocsigen.org/js_of_ocaml/).

The first was done by Gabriel, who wrote a grammar
using [Menhir](http://gallium.inria.fr/~fpottier/menhir/), and then equipped it
with custom parsing error messages that provide much better feedback when there
are typos in what people are trying. We also wrote a pretty-printer using
the [PPrint](http://gallium.inria.fr/blog/first-release-of-pprint/) library, so
we could show intermediate program states through the UI. After writing this, we
were able to convert our existing suite of test cases and examples to be
written textually, which was a huge improvement for us as well! These and other
tests were used to ensure that the parser/pretty-printer would round-trip properly.

For the interface, I built a simple web page that had
the [CodeMirror](https://codemirror.net/) editor equipped with a very simple
syntax highlighter ([8 lines of code](https://github.com/dbp/funtal/blob/032be70f33f77e80f4fab7e62016bfabf96476f3/artifact/index.html#L247) to highlight keywords & atoms, plus a
CodeMirror extension to highlight matching brackets) and error highlighting
(which is triggered by the OCaml code). I then made a simple "machine state" UI
that showed, in pretty-printed format, the heap, stack, registers, context, and
redex. On the OCaml side, when the "run" button is clicked, we parse and
typecheck and, assuming no errors occur, store the current state as our "history".
As the user clicks forward or backwards, we run the step function and append to the history of
states or pop states off of the history. In total, there
are
[50 lines of Javascript](https://github.com/dbp/funtal/blob/032be70f33f77e80f4fab7e62016bfabf96476f3/artifact/index.html#L246) and
about
[150 lines of OCaml](https://github.com/dbp/funtal/blob/032be70f33f77e80f4fab7e62016bfabf96476f3/web.ml) that
handle the logic for this interactive UI.

Putting it online was very easy, based on the choice of tools used earlier. We
compile the main file
([web.ml](https://github.com/dbp/funtal/blob/032be70f33f77e80f4fab7e62016bfabf96476f3/web.ml))
to Javascript using [js_of_ocaml](http://ocsigen.org/js_of_ocaml/), and it pulls in the parser, type-checker,
interpreter, examples, etc. The rest of the artifact is a single html file, a
CSS file, and a few javascript files for CodeMirror. It requires no server
backend, is easy to archive and save, and will even run on smartphones! 

The total time spent implementing the artifact was a small fraction of the time
spent on the paper (probably 15 days of person-time), and while it was not in
any critical way essential for the success of the paper, it does make the paper
much easier to read, and we would argue that all semantics papers would be
better off with easy to use artifacts for experimentation. Also, while
implementing the artifact we found a few mistakes in the typing judgments for
the language. The most interesting one was for our `protect` TAL instruction,
which exists to protect the tail of the stack in a fresh type variable. We had
written this as a typing rule that type-checked the rest of the instruction
sequence with the abstracted tail, but this never allowed the tail to be
accessed again. By translating the typing judgments exactly into code, we
realized that there was a problem, because examples that should have worked did
not type-check! We were then able to fix the typing rule to conform to what we
originally thought it achieved -- locally abstracting, but not abstracting from
outside the component. What is interesting is that this did not come up in our
proofs because the typing rule was perfectly valid -- it just did not allow
non-trivial programs that used the `protect` instruction. It's quite possible we
would have noticed this without implementing the artifact, but the artifact
certainly made it easier!

To see the artifact online, visit:

[https://dbp.io/artifacts/funtal](https://dbp.io/artifacts/funtal)

The source code is at:

[https://github.com/dbp/funtal](https://github.com/dbp/funtal/tree/032be70f33f77e80f4fab7e62016bfabf96476f3)
