    Title: Writing a paper with Scribble
    Date: 2019-02-17T16:20:50
    Tags: Scribble, tutorial, by Ben Greenman

This post explains how to get started using Scribble to write a research paper.

<!-- more -->

- - -

> This post was written using [Racket 7.1](http://download.racket-lang.org/all-versions.html)
> and [Scribble 1.29](https://github.com/racket/scribble/releases/tag/v7.1)

Writing about research is always difficult,
  but a compile-to-LaTeX tool can make the task easier.
If your research code is written in the same language as the paper, then:

- the paper can import definitions from the research,
  keeping a single point of control;
- the language's functional abstractions can help manage the writing;
- the language's drawing and/or plotting libraries can replace [TikZ](https://ctan.org/pkg/pgf?lang=en);
- and you can write unit tests to validate the claims made in the paper.

Scribble, [the Racket documentation tool](http://docs.racket-lang.org/scribble/index.html),
 comes with a to-LaTeX compiler and a [scribble/acmart][scribble/acmart]
 library tailored to the new [ACM paper format](https://ctan.org/pkg/acmart?lang=en).
I have been a pretty happy user of these tools.
In the interest of attracting more happy users, this post
 presents a short "getting started" guide
 and links to some larger examples.

> For a Scribble tutorial, see the links in: [Building a Website with Scribble](/blog/2017/05/23/building-a-website-with-scribble/index.html)


## Getting started with [scribble/acmart][scribble/acmart]

The first line of a [scribble/acmart][scribble/acmart] document sets the
formatting options (similar to a LaTeX file using `acmart.cls`).
For example, the [GPCE 2018 call for papers](https://conf.researchr.org/track/gpce-2018/gpce-2018#Call-for-Papers)
 asks for anonymized `sigplan`-format submissions with line numbers and 10 point font.
The proper Scribble incantation is:

```
#lang scribble/acmart @sigplan @anonymous @review @10pt
```

Next, you may want to import some definitions.
If we have a file `references.rkt` (see below for a definition), we can import
it as follows:

```
@require{references.rkt}
```

The third main ingredient is the title and author information:

```
@(define neu (affiliation #:institution "Northeastern University"))
@(define anon (email "anon@anon.net"))

@title{Writing a paper with Scribble}
@author[#:affiliation neu #:email anon]{Ben Greenman}

@; optional: set the author names in the page headers
@elem[#:style "Sshortauthors"]{B. Greenman}
```

The paper is now ready to be written.
You can forge ahead with a new [section](http://docs.racket-lang.org/scribble/base.html#%28def._%28%28lib._scribble%2Fbase..rkt%29._section%29%29)
 and start adding content to the same file;
 alternatively, you can organize the writing across different modules.
In this post, we will use the main document as an outline and [import](http://docs.racket-lang.org/scribble/base.html#%28form._%28%28lib._scribble%2Fbase..rkt%29._include-section%29%29)
 content from other modules:

```
@include-abstract{abstract.scrbl}
@include-section{introduction.scrbl}
```

Finally, the main page is a good place to [generate the bibliography](https://docs.racket-lang.org/scriblib/autobib.html).
Assuming this document imports a file like the `references.rkt` below,
 this expression inserts a bibliography titled "References":

```
@generate-bibliography[#:sec-title "References"]
```

To build the document, invoke `scribble` on the command-line with the `--pdf` or `--latex`
 options:

```
$ raco scribble --pdf FILE.scrbl
```

If all goes well, this command generates a `FILE.pdf` with properly-linked
cross references.


### Auxiliary Files

If you save the code above to a file `example.scrbl` and save the files below
 in the same directory, then you should be able to build an `example.pdf`.

These files are available in a slightly different format at this link:
 
- <https://gitlab.com/bengreenman/scribble-acmart-example>


#### `references.rkt`

```
#lang racket/base

(provide
  ~cite citet generate-bibliography
  fbf-icfp-2009)

(require
  scriblib/autobib)

(define-cite ~cite citet generate-bibliography
  #:style author+date-square-bracket-style)

(define icfp "ICFP")

(define fbf-icfp-2009
  (make-bib
    #:title "Scribble: Closing the Book on Ad Hoc Documentation Tools"
    #:author (authors "Matthew Flatt" "Eli Barzilay" "Robert Bruce Findler")
    #:location (proceedings-location icfp #:pages '(109 120))
    #:date 2017))
```

#### `abstract.scrbl`

```
#lang scribble/acmart

A simple Scribble document.
```

#### `introduction.scrbl`

```
#lang scribble/acmart
@require{references.rkt}

@; start with `title` instead of `section`, because importing via
@;  `include-section` shifts all title/section/subsections down one level
@title{Introduction}

Scribble creates a connection between a stand-alone document and the artifact
it describes@~cite[fbf-icfp-2009].
```


### Q. How to debug Scribble error messages?

If something goes wrong building a Scribble document, Racket is usually able
to give a helpful error message.

As a compile-time example, adding `@ foo` to a document produces the message
 `unexpected whitespace after @` and you can either delete the whitespace
 or change the `@` to `@"@"` for a literal `@`-sign.

As a run-time example, adding `@(+ 2 2)` produces this message:

```
not valid in document body (need a pre-part for decode) in: 4
```

One fix is to convert `4` to a string, as in `@~a[(+ 2 2)]`.

But if something goes wrong when Scribble renders a generated document to
 PDF, the default error output is **not** likely to help.
For example, adding `@elem[#:style "oops"]` to a document produces a giant
 message:

```
$ raco scribble --pdf FILE.scrbl
[[ ... 84K of output ... ]]
Output written on example.pdf (1 page, 277876 bytes).
PDF statistics:
 53 PDF objects out of 1000 (max. 8388607)
 37 compressed objects within 1 object stream
 7 named destinations out of 1000 (max. 500000)
 36877 words of extra memory for PDF output out of 42996 (max. 10000000)

run-pdflatex: got error exit code
  context...:
  [[ ... 17 more lines ... ]]
```
 
 
The best way to debug these messages is to **ignore them** and use a LaTeX
 compiler directly.
For the "oops" mistake, LaTeX stops at the undefined control sequence --- giving
 a hint about how to find the problem:

```
$ raco scribble --latex FILE.scrbl
$ pdflatex FILE.tex
[[ ... 12KB of output ... ]]
! Undefined control sequence.
l.549 \oops
           {}
? 
```


### Q. How to add a LaTeX style file?

To add extra LaTeX code to the final document, create a new file and include
 it with the `++style` command-line flag.
This copies the contents of the style file into the generated document
 (the copy appears near the top of the generated code).

```
$ raco scribble ++style style.tex --pdf FILE.scrbl
```

Here is an example style file.


#### `style.tex`

```
\settopmatter{printfolios=true,printccs=true,printacmref=true}
% add page numbers etc.

\overfullrule=1mm
% draw a black rectangle near lines that overflow the margin
```

Another way to add extra LaTeX code is to add a [`tex-addition`](https://docs.racket-lang.org/scribble/core.html#%28def._%28%28lib._scribble%2Flatex-properties..rkt%29._tex-addition%29%29)
 style property to the main title.
This second approach makes it easy to include more than one file:

```
#lang scribble/acmart

@require[
  (only-in scribble/core make-style)
  (only-in scribble/latex-properties make-tex-addition)]

@(define extra-style-files
   (list (make-tex-addition "style.tex")))

@title[#:style (make-style #f extra-style-files)]{Writing a paper with Scribble}

@; ....
```



### Q. How to make a figure?

Use the [scriblib/figure][scriblib/figure]
 library to add figures to a document.

```
@require[pict scriblib/figure]
@figure[
  "fig:fish"  @; figure tag, see `figure-ref`
  @elem{A Standard Fish}  @; figure caption, appears below the content
  @elem{fish = @(standard-fish 90 40)}]  @; content
```

The content of a figure can be almost anything that would work in the toplevel
 of the document.


### Q. How to include extra files (pictures, LaTeX)?

The `++extra` command-line flag names an auxilliary file that Scribble should
 include when rendering the document.
This flag may be supplied more than once.

For example, if a document includes the content of an external LaTeX file:

```
@elem[#:style "input"]{inline-this.tex}
```

then make sure to build the document with a command like this:

```
$ raco scribble ++style style.tex ++extra inline-this.tex FILE.scrbl
```

#### `inline-this.tex`

```
% Raw LaTeX allowed here
$\lambda x.\, x$
```


### Q. What about in-line LaTeX?

An [element](https://docs.racket-lang.org/scribble/core.html#%28def._%28%28lib._scribble%2Fcore..rkt%29._element%29%29)
 with the [`'exact-chars`](https://docs.racket-lang.org/scribble/core.html#%28idx._%28gentag._60._%28lib._scribblings%2Fscribble%2Fscribble..scrbl%29%29%29)
 [style property](https://docs.racket-lang.org/scribble/core.html#%28tech._style._property%29)
 renders directly to LaTeX.

```
@(define (exact . stuff)
   @; the style name "relax" puts a `\relax` no-op in front of the stuff
   (make-element (make-style "relax" '(exact-chars)) stuff))

@exact|{$\lambda x.\, x$}|
@; ==> \relax{$\lambda x.\, x$}

@(define ($ . math-stuff)
   (apply exact (list "$" math-stuff "$")))

@${\lambda x.\, x}
@; ==> \relax{$\lambda x.\, x$}
```


## Creating a [#lang](http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._hash-lang%29) for a paper

For a Scribble document that is split across multiple files, it can be helpful
 to make a `#lang` that [provides a common environment](http://blog.racket-lang.org/2017/03/languages-as-dotfiles.html).
Instead of starting each file with a `require`, e.g.:

#### `paper.scrbl`

```
#lang scribble/acmart
@require["references.rkt" "helper-functions.rkt" scriblib/figure]

....
```

files can start with a name that describes their common purpose:

#### `paper.scrbl`

```
#lang conference-2018-submission

....
```

As a bonus, if the language is defined as a package then the Scribble document
 can use Racket's dependency management tools:

```
# to install the paper and interactively install dependencies:
$ cd conference-2018-submission;
$ raco pkg install

# To check that the paper builds with no dependency issues:
$ raco setup --check-pkg-deps conference-2018-submission

# To run all unit tests
$ raco test -c conference-2018-submission
```

To create a package and language:

1. Move the Scribble document to a directory with the language name, i.e.,
   `conference-2018-submission/`
2. Write a simple `info.rkt` to configure the package
3. Create a normal Racket module that exports the common environment
4. Create a `conference-2018-submission/lang/reader.rkt` module

Details below.
For a full example, visit:

- <https://gitlab.com/bennn/scribble-acmart-example>

- - -

#### `conference-2018-submission/info.rkt`

This file defines the basic metadata for a package.
For more about `info.rkt`, see: [Tutorial: Creating a Package](http://blog.racket-lang.org/2017/10/tutorial-creating-a-package.html).

```
#lang info
(define collection "conference-2018-submission")
(define deps '("base" "scribble-lib" "at-exp-lib"))
(define build-deps '("racket-doc" "scribble-doc"))
(define pkg-desc "Paper for Conference 2018")
(define version "0.1")
```


<br/>

#### `conference-2018-submission/main.rkt`

This file defines and exports the common environment for every file in our
 Scribble document.
In this example, the common environment is:
 the [scribble/acmart][scribble/acmart] language,
 the file "references.rkt",
 and the [scriblib/figure][scriblib/figure] library.

```
#lang racket/base

(provide
  (all-from-out
    scribble/acmart
    scribble/acmart/lang
    scriblib/figure
    "references.rkt"))

(require
  scribble/acmart
  scribble/acmart/lang
  scriblib/figure
  "references.rkt")
```


<br/>

#### `conference-2018-submission/lang/reader.rkt`

This file:
 (1) tells Racket to use the Scribble reader on `#lang conference-2018-submission`
 modules, and (2) wraps the result of such modules in a shape that Scribble
 expects.

```
#lang s-exp scribble/base/reader
conference-2018-submission
#:wrapper1 (lambda (t) (cons 'doc (t)))
```


## Links to Example Documents

These documents use the `#lang` approach to writing a paper with Scribble.
Check their `main.rkt` for example formatting functions and unit tests,
 and check the `.scrbl` files to see how the ideas above look in a larger document.

- <https://github.com/nuprl/retic_performance/tree/master/gm-pepm-2018>
- <https://github.com/nuprl/tag-sound/tree/master/gf-icfp-2018>

Finally, this repository provides a tool to start a new Scribble document:

- <https://pkgd.racket-lang.org/pkgn/package/gtp-paper>

## Further Reading

- [Checking Machine-Checked Proofs](https://project.inria.fr/coqexchange/checking-machine-checked-proofs/)


[scribble/acmart]: http://docs.racket-lang.org/scribble/ACM_Paper_Format.html
[scriblib/figure]: http://docs.racket-lang.org/scriblib/figure.html#%28def._%28%28lib._scriblib%2Ffigure..rkt%29._figure%29%29
