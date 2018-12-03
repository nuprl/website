    Title: Disappearing Code
    Date: 2018-11-24T09:52:58
    Tags: dear diary, by Ben Greenman

Two experiences at [SPLASH 2018](https://2018.splashcon.org/home)
reminded me that software gets thrown away and replaced.

<!-- more -->

### Story 1

The first reminder came near the end of a [talk][rinard] by
 [Martin Rinard](https://people.csail.mit.edu/rinard/).
Once upon a time, Martin was working as a consultant and a firm asked him to
 review a software package.
(The firm wanted a second opinion about how the software computed its results.)
The firm sent a zipfile; Martin found six versions of the code inside; the
 firm said "well, please check all six versions"; and it turned out:

- **Version 1** : the source code was written in a domain-specific language
  (DSL) that generated code for the application
- **Version 2** : the DSL source was the same as version 1, but the generated
  code was slightly modified
- ...
- **Version 6** : the generated code was the source code and the DSL was gone

The moral of Martin's story was: 
 (1) the creators of a software system are often different from the maintainers,
 and (2) researchers need to build tools to help these maintainers.
 

### Story 2

The second reminder came from a teaching assistant who said the
 [functional programming course](https://www.cs.cornell.edu/courses/cs3110/2018fa/)
 at their institution was currently using a Python script
 to test students' code.
Once upon a time, I was a teaching assistant for the
 [same course](https://www.cs.cornell.edu/courses/cs3110/2014sp/) at the same
 institution.
We had trouble testing students' code via the Python script
 left by the pre-2013 course staff, so I wrote a
 [command-line tool][cs3110-cli] to handle the tests and other
 compile/run/grade tasks.
To keep history from repeating itself, I used the same language the course
 teaches (OCaml) and wrote some documentation --- but it seems like that was
 not enough.
At any rate, writing the tool was a good exercise.

> _In the end, everybody must understand for himself._ -- [Per Martin-Löf](https://dl.acm.org/citation.cfm?id=3731)


### Reflection

In each story, the maintainers of a software system threw away some old
 code to make their job easier in the short term.
How can we stop this "re-inventing the wheel" from happening?

Martin Rinard's solution is to let maintenance programmers keep their current
 habits, but provide tools to make the short-term, pragmatic solutions into a
 more robust systems.
Search for "[failure-oblivious computing](https://people.csail.mit.edu/rinard/paper/osdi04.pdf)"
 to learn more (this was the topic of his [talk][rinard]).

In Story 1, the maintainers were able to avoid the DSL by modifying an
 inherited blob of DSL-generated code.
If the DSL did not generate code, history might have taken a different course;
 it might be best to start with a language that offers tools for linguistic
 re-use, and to build a DSL from these tools --- so there is no generated code.
The Racket programming language is exploring this path.
For a recent example, see the [video-lang paper](https://www2.ccs.neu.edu/racket/pubs/icfp17-acf.pdf).

The Story 2 test harness, however, was not generating code.
Its maintainers discarded a "big" program written in a typed
 functional language in favor of a script.
Perhaps we need a language that allows mixing statically-typed and
 dynamically-typed code (shouts out to
 [my own research](https://www2.ccs.neu.edu/racket/pubs/icfp18-gf.pdf)).

The best solution is probably to start with a team and keep the culture alive.
Always pair program!

- - -

#### Addendum: comment from Mitch Wand

> The best solution is probably to start with a team and keep the culture alive. Always pair program!  

Ermm, this works better for sourdough bread than for people.

Even in the not-so-real world of checking student solutions, there's often no way of guaranteeing that one half of a pair will be around for the second round.   They may be on co-op.  Or the course will not be offered the next semster/year/etc.  Or the course will change at the next offering (from OCaml to Python or from Racket to Java) so that large chunks of the infrastructure will have to be discarded or rewritten.

The "real" solution is to write literate code (as we preached incessantly in PDP), so that the next reader will have at least some clue as about what you wrote.   This just may be sufficient incentive to modify rather than rebuild from scratch.

Ever the optimist,
--Mitch

[rinard]: https://conf.researchr.org/event/sle-2018/papers-a-new-approach-for-software-correctness-and-reliability
[cs3110-cli]: https://gitlab.com/bengreenman/ocaml_tools/
