    Title: CompCert Overview
    Date: 2016-10-11T17:41:16
    Tags: tutorial, coq, compiler correctness, by Ben Greenman

If you are interested in learning about the _internals_ of the CompCert C
compiler but would rather not read its source code, this post is for you.

<!-- more -->

(This is a public service announcement.)

Last fall, I gave a short lecture on the 2006 paper ["Formal Certification of a Compiler Back-End"](http://gallium.inria.fr/~xleroy/publi/compiler-certif.pdf) by Xavier Leroy for Amal Ahmed's ["Special Topics in Programming Languages"](http://www.ccs.neu.edu/home/amal/course/7480-f15/) class.
Rather than present CompCert as it existed in 2006, I read the documentation and source code for [CompCert 2.5](https://github.com/AbsInt/CompCert/releases/tag/v2.5) (released June 2015).
The lecture then focused on three questions:

- What subset of C does CompCert handle, today?
- What optimizing passes does CompCert perform?
- What is the "correctness theorem" for CompCert, and what does this theorem mean?

My notes for the lecture give a "mid-level" summary of the compiler --- there are more details than you'll find in papers, but it's (hopefully!) easier to read than the source code.
The document is also hyperlinked to locations in the [CompCert GitHub repository](https://github.com/AbsInt/CompCert).

Here is the document:

>   [http://www.ccs.neu.edu/home/types/resources/notes/compcert/cc.pdf](http://www.ccs.neu.edu/home/types/resources/notes/compcert/cc.pdf)

And here is a table-of-contents:

  1. Motivation, details of the source and target languages, high-level guarantees
  2. Compiler pipeline, optimizing passes, links intermediate language grammars and Coq theorems
  3. Background on compiler correctness
  4. CompCert's correctness, properties that CompCert does __not__ guarantee
  5. Recent (2006 - 2015) work in the CompCert ecosystem

The document ends with a short description of two other research projects that have grown into "industry software" and a link to Xaver Leroy's [OPLSS lectures on certified compilers](https://www.cs.uoregon.edu/research/summerschool/summer12/curriculum.html).
Enjoy!

