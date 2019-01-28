    Title: On-Stack Replacement
    Date: 2019-01-28T10:29:57
    Tags: by Ming-Ho Yee

Last semester, I took [a course][cs7600] where the final project was to write a
survey paper on "a topic in the intersection between computer systems and your
area." So I wrote about on-stack replacement.

<!-- more -->

**Abstract**

> On-stack replacement (OSR) is a programming language implementation technique
> that allows a running program to switch to a different version of code. For
> example, a program could start executing optimized code, and then transfer to
> and start executing unoptimized code. This was the original use case for OSR,
> to facilitate debugging of optimized code.
>
> After its original use was established, OSR shifted to a different use case:
> optimizing programs. OSR allows the run-time system to detect if a program is
> executing an inefficient loop, recompile and optimize the method that contains
> the loop, and then transfer control to the newly compiled method. Another
> strategy is to optimize code based on some assumptions, then, if the
> assumptions are invalidated at run-time, transfer control back to the
> original, unoptimized code.
>
> In this survey paper, we study how OSR was first introduced as a means for
> debugging, how it came to be used for program optimizations, its
> implementation as a reusable library, and other directions of research.

If you're interested, you can find a copy [here][] or on [Overleaf][].

---

_If you liked this post, you may also be interested in [tracing JITs for dynamic
languages][tracing]._

[cs7600]: https://course.ccs.neu.edu/cs7600/
[here]: /img/cs7600-mhyee-survey-paper-osr.pdf
[Overleaf]: https://www.overleaf.com/read/smcmsnksxfdk
[tracing]: http://prl.ccs.neu.edu/blog/2017/03/15/tracing-jits-for-dynamic-languages/
