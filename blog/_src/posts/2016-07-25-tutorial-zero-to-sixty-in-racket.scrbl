#lang scribble/manual

@; https://yanniss.github.io/law.html
@; @hyperlink["http://www.ccs.neu.edu/racket/pubs/dls06-tf.pdf"]{scripts to programs}

Title: Tutorial: Zero to Sixty in Racket
Date: 2016-08-02T01:29:11
Tags: Racket, tutorial, by Ben Greenman

@(require scribble/example
          scribble/racket
          "../../utils/utils.rkt"
          (for-label racket/base)
          (for-syntax racket/base))
@(define ev (make-base-eval))


@; -----------------------------------------------------------------------------

Racket is excellent for incrementally growing scripts into full-fledged programs.
This post steps through the evolution of one small program and highlights the
 Racket tools that enable incremental advances.

<!-- more -->

Why should anyone use @hyperlink["http://racket-lang.org/"]{Racket}?
There are two reasons:
@itemlist[#:style 'ordered
  @item{
    You have a problem that can only be solved with Racket's language-building tools
     @; e.g., Rosette, Pollen
  }
  @item{
    Racket is a nice language to program in.
    (Has lexical scope, parentheses, @hyperlink["http://con.racket-lang.org"]{active users}...)
  }
]

My favorite part of Racket is how it supports a certain development style of
 evolving scripts into programs.
When I start coding (after design, before debugging), I can focus the problem at hand.
Next come examples, unit tests, and types to be sure the solution is correct.
Finally, I worry about aesthetics, efficiency, and how the solution can be used as a library in a larger context.

Bottom line: with Racket, my coding is aligned with my priorities.
And as I transition from "no code" to "working code" to "robust code" to "re-usable code",
 the program is almost always runnable.


@section*{Problem: A KWIC Index Production System}

A KWIC index system
@;@itemlist[
@;  @item{
    reads input from a file,
@;  }
@;  @item{
    divides each line of the file into whitespace-separated words,
@;  }
@;  @item{
    and outputs (in alphabetical order) all circular shifts of all lines.
@;  }
@;]

The first circular shift of a line @racket{A B C} is the line @racket{B C A}.
The second circular shift is @racket{C A B}.

Building a KWIK index is a historical problem.
According to @hyperlink["https://www.cs.umd.edu/class/spring2003/cmsc838p/Design/criteria.pdf"]{D.L. Parnas (1972)}:

@nested{Except under extreme circumstances (huge data base, no supporting software)
  such a system could be implemented by a good programmer within a week or two.
}

See also: @hyperlink["https://yanniss.github.io/law.html"]{Yannis's Law}.

Today, I bet only @hyperlink["http://wiki.portal.chalmers.se/agda/pmwiki.php"]{Agda} and @hyperlink["https://scratch.mit.edu/"]{Scratch}
 programmers would need the full two weeks.
We'll be done in 20 minutes.


@section*{A Script}

To start, open a file and type:

@codeblock{
  #lang racket
}

You can name the file anything, like @tt{kwik.rkt} or @tt{rkt.kwik} or @tt{foo}.
Racket doesn't care,
 but it does need the @tt{#lang} line to read the contents of the file.

Though, you should use the @tt{.rkt} extension.

The first part of the solution is a function to read input from a file into a
 list of strings for further processing.
The built-in function @hyperlink["http://docs.racket-lang.org/reference/Filesystem.html#%28def._%28%28lib._racket%2Ffile..rkt%29._file-~3elines%29%29"]{file->lines}
 does exactly this, but we'll for-loop instead.

@(begin
  #reader scribble/comment-reader
  (racketblock
    (define (kwik-read filename)
      (with-input-from-file filename
        (λ ()
          (for/list ([line (in-lines)])
            line))))
))

@; The code is a little slanted, but Python has the same problem.

When called with a filename like @racket{heart-of-darkness.txt}, the function
 uses @hyperlink["http://docs.racket-lang.org/reference/for.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._for%2Flist%29%29"]{for/list} to build a list of lines by reading from a port
 with @hyperlink["http://docs.racket-lang.org/reference/sequences.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._in-lines%29%29"]{in-lines}.
The port is the data from @racket[filename], thanks to @hyperlink["http://docs.racket-lang.org/reference/file-ports.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._with-input-from-file%29%29"]{with-input-from-file}.

Next is a function to convert a list of strings into a list of lists of words.
Here we'll just use library functions.

@(begin
  #reader scribble/comment-reader
  (racketblock
    (define (kwik-split lines)
      (map string-split lines))
))

By default, @hyperlink["http://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-split%29%29"]{string-split} divides a string into a list of whitespace-separated substrings.
You can always supply a different delimiter, or use @hyperlink["http://docs.racket-lang.org/reference/regexp.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._regexp-split%29%29"]{regexp-split}
 to divide by a regular expression.

Two tasks left!
First we generate all circular shifts for a list of strings @racket[words]
 by folding up a list with one shift of @racket[words] for each word.

@(begin
  #reader scribble/comment-reader
  (racketblock

    (define (circular-shift words)
      (append (rest words) (list (first words))))

    (define (all-circular-shifts words)
      (for/fold ([all-shifts (list words)])
                ([i (in-range 1 (length words))])
        (cons (circular-shift (first all-shifts))
              all-shifts)))
))

Second, we alphabetize and print the shifts.

@(begin
 #reader scribble/comment-reader
 (racketblock
  (define (alphabetize all-shifts)
    (sort all-shifts shift<?))

  (define (shift<? shift1 shift2)
    (match* (shift1 shift2) ; destruct multiple values
     [('() _) ; first list empty, don't care about second
      #t]
     [(_ '()) ; first list non-empty, second empty
      #f]
     [((cons s1 shift1-rest) (cons s2 shift2-rest))
      (or (string<? s1 s2)
          (and (string=? s1 s2)
               (shift<? shift1-rest shift2-rest)))]))

  (define (kwik-display all-sorted-shifts)
    (define (display-words words)
      (display (first words))
      (for ([word (in-list (cdr words))])
        (display " ")
        (display word))
      (newline))
    ; for-each is like map, but returns (void)
    (for-each display-words all-sorted-shifts))
))

Gluing it all together, here's the full script (with type annotations in comments).

@codeblock{
  #lang racket

  ; type Words = (Listof String)
  ; type Lines = (Listof Words)

  ; Path-String -> (Listof String)
  (define (kwik-read filename)
    (with-input-from-file filename
      (λ ()
        (for/list ([line (in-lines)])
          line))))

  ; (Listof String) -> Lines
  (define (kwik-split lines)
    (map string-split lines))

  ; Words -> (Listof Words)
  (define (all-circular-shifts words)
    (for/fold ([all-shifts (list words)])
              ([i (in-range 1 (length words))])
      (cons (circular-shift (first all-shifts))
            all-shifts)))

  ; Move first element to last position
  ; Words -> Words
  (define (circular-shift words)
    (append (rest words) (list (first words))))

  ; Lines -> Lines
  (define (alphabetize all-shifts)
    (sort all-shifts shift<?))

  ; Lexicographic order on equal-length lists of words
  ; Words Words -> Boolean
  (define (shift<? shift1 shift2)
    (match* (shift1 shift2)
     [('() _) ; first list empty, don't care about second
      #t]
     [(_ '()) ; first list non-empty, second empty
      #f]
     [((cons s1 shift1-rest) (cons s2 shift2-rest))
      (or (string<? s1 s2)
          (and (string=? s1 s2)
               (not (null? shift1-rest))
               (shift<? shift1-rest shift2-rest)))]))

  ; Lines -> Void
  (define (kwik-display all-sorted-shifts)
    (define (display-words words)
      (display (first words))
      (for ([word (in-list (cdr words))])
        (display " ")
        (display word))
      (newline))
    (for-each display-words all-sorted-shifts))

  ; Lines -> (Listof Lines)
  (define (all-circular-shifts* lines)
    (map all-circular-shifts lines))

  ; Path-String -> Void
  (define (kwik-index file-name)
    (define all-lines (kwik-split (kwik-read file-name)))
    (define all-shifts (append* (all-circular-shifts* all-lines)))
    (kwik-display (alphabetize all-shifts)))

  ; End-to-end test
  ; -> Void
  (define (run-test)
    (define test-file "test.txt")
    ; Make a file and test
    (unless (file-exists? test-file)
      (with-output-to-file test-file
        (λ ()
          (displayln "imagine if this")
          (displayln "took 2 weeks to write"))))
    (kwik-index test-file))

  (run-test)
}

Running the file should print:

@codeblock{
 2 weeks to write took
 if this imagine
 imagine if this
 this imagine if
 to write took 2 weeks
 took 2 weeks to write
 weeks to write took 2
 write took 2 weeks to
}


@section*{Testing and Submodules}

Any top-level expressions in a file can work as unit tests.
The @hyperlink["http://docs.racket-lang.org/reference/booleans.html%3F#%28def._%28%28quote._~23~25kernel%29._equal~3f%29%29"]{equal?} statement below checks whether the first circular shift
 of @racket['("A" "B" "C")] is @racket['("B" "C" "A")].

@(begin
  #reader scribble/comment-reader
  (racketblock
    (define (circular-shift words)
      (append (rest words) (list (first words))))

    (equal? (circular-shift '("A" "B" "C")) '("B" "C" "A"))
))

Running the file now prints @racket[#t] to the console, meaning the test passed.
We can use @racket[error] or @racket[raise-user-error] to make failures easier
 to notice.
Or we can use the @hyperlink["http://docs.racket-lang.org/rackunit/api.html"]{RackUnit} testing library.

@(begin
  #reader scribble/comment-reader
  (racketblock
    (define (circular-shift words)
      (append (rest words) (list (first words))))

    (require rackunit) ; import the testing library
    (check-equal?
      (circular-shift '("A" "B" "C"))
      '("B" "C" "A"))
))

These tests run each time the module does.
If you prefer to run tests only in a specific context, and not when the
 module is run or imported as a library, you can move them to a separate
 file or into a @hyperlink["http://docs.racket-lang.org/reference/eval-model.html#%28tech._submodule%29"]{submodule}.

@(begin
  #reader scribble/comment-reader
  (racketblock
    (define (circular-shift words)
      (append (rest words) (list (first words))))

    (module+ test ; Open a submodule named 'test'
      (require rackunit)
      (check-equal?
        (circular-shift '("A" "B" "C"))
        '("B" "C" "A")))
))

Running the module normally via @tt{racket kwik.rkt} will not run code
 in the submodule.
Instead, use @tt{raco test} to run the tests.

@codeblock{
>  raco test kwik.rkt
raco test: (submod "kwik.rkt" test)
1 test passed
}

The reason we used @racket[module+], instead of Racket's @racket[module] and @racket[module*]
 forms is that @racket[module+] inherits the language and namespace of its
 containing module and can be incrementally extended.
This way, we can keep tests near the relevant code.

@(begin
  #reader scribble/comment-reader
  (racketblock
    (module+ test
      (require rackunit))

    (define (circular-shift words)
      (append (rest words) (list (first words))))

    (module+ test
      (check-equal?
        (circular-shift '("A" "B" "C"))
        '("B" "C" "A")))

    (define (alphabetize all-shifts)
      (sort all-shifts shift<?))

    (module+ test
      (check-equal?
        (alphabetize '(("racket" "is")
                       ("as")
                       ("racket" "does")))
        '(("as")
          ("racket" "does")
          ("racket" "is"))))
))

@hyperlink["http://docs.racket-lang.org/rackunit/api.html"]{RackUnit} in a
 separate file or @racket[test] submodule is the unofficial standard for testing
 Racket programs.


@section*{Recognizing Patterns, Avoiding Repetition}

Every unit test we've written uses @hyperlink["http://docs.racket-lang.org/rackunit/api.html#%28def._%28%28lib._rackunit%2Fmain..rkt%29._check-equal~3f%29%29"]{check-equal?}.

@(begin
  #reader scribble/comment-reader
  (racketblock

    (module+ test
      (check-equal?
        (kwik-split '())
        '())
      (check-equal?
        (kwik-split '("hello    world"))
        '(("hello" "world")))
      (check-equal?
        (kwik-split '(" lost " " in " "space"))
        '(("lost") ("in") ("space")))
      (check-equal?
        (kwik-split '("something"))
        '()))
))

These tests follow a simple pattern that we can express as a @emph{syntax rule}.

@(begin
  #reader scribble/comment-reader
  (racketblock

    (module+ test
      (define-syntax-rule (check-equal?* [i o] ...)
        (begin
          (check-equal? i o)
          ...))

      (check-equal?*
        [(kwik-split '())
         '()]
        [(kwik-split '("hello    world"))
         '(("hello" "world"))]
        [(kwik-split '(" out " " in " "the ozone"))
         '(("out") ("in") ("the" "ozone"))]
        [(kwik-split '("something"))
         '()]))
))

The @racket[...] are not pseudocode!
They denote Kleene-star repetition, like a sextile (@racket{*}) in a regular expression.
In this case, the input pattern is a sequence of lists with two S-expressions, @racket[i] and
 @racket[o].
Uses of @racket[i] and @racket[o] in the rule must be followed by one @racket[...] to splice
 the captured S-expressions into the result.

Many languages offer higher-order functions and polymorphism to abstract common
 behaviors.
Syntax extensions are a different way to avoid repeating yourself.
After 30 years, we are still discovering what syntax extensions are useful for.

See this @hyperlink["https://groups.google.com/forum/#!topic/racket-users/ss20lwfUhjs/discussion"]{recent Racket mailing list post} for some applications.


@section*{Adding Static Types}

Changing the @hyperlink["http://docs.racket-lang.org/reference/reader.html#%28idx._%28gentag._79._%28lib._scribblings%2Freference%2Freference..scrbl%29%29%29"]{@tt{#lang}} line to @racket[typed/racket] adds static type-checking to our program.
If we only change the language and run the code as-is, there will be type errors.
But we can use submodules again to incrementally check our design with types.

Note: @hyperlink["http://docs.racket-lang.org/ts-reference/Typed_Regions.html"]{typed regions}
 are another way to embed typed code into untyped contexts.

@codeblock{
   #lang racket

    (module t typed/racket
      ; Need to annotate:
      ; - function parameters
      ; - for-loop return types

      (: kwik-read : Path-String -> (Listof String))
      (define (kwik-read filename)
        (with-input-from-file filename
          (λ ()
            (for/list ([line (in-lines)])
                      : (Listof String)
              line))))

      ; Next migration step: move other untyped functions here

      (provide (all-defined-out)))
    (require 't)

    (define (kwik-split lines)
      (map string-split lines))

    ; <rest of file omitted>
}

After scooping all functions into the Typed Racket bubble, we can remove the
 submodule declaration and change @tt{#lang racket} to @tt{#lang typed/racket}.


@section*{Finally, a Library}

Other modules can import our functions if we use a @hyperlink["http://docs.racket-lang.org/reference/require.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._provide%29%29"]{provide} statement.
By @hyperlink["https://docs.racket-lang.org/style/Units_of_Code.html"]{convention}, exports belong at the top of a file.

@codeblock{

    #lang typed/racket

    (provide kwik-index)

    ; <definitions here>
}

Then any typed or untyped module can use @racket[kwik-index] by writing
 @racket[(require "kwik.rkt")].

As a finishing touch, we can use the @hyperlink["http://docs.racket-lang.org/reference/Command-Line_Parsing.html"]{racket/cmdline} library
 inside a @tt{main} submodule to give a basic front-end interface.
Similar to @tt{module+ test}, a @tt{module+ main} declares code that
 inherits the file's bindings and language but is only run when the program
 is executaed.

Here is the complete typed and tested code listing.
The @racket[main] submodule is at the bottom.

@codeblock{
  #lang typed/racket
  (module+ test
    (require typed/rackunit)

    (define-syntax-rule (check-equal?* [i o] ...)
      (begin
        (check-equal? i o)
        ...)))

  (define-type Words (Listof String))
  (define-type Lines (Listof Words))

  (: kwik-read : Path-String -> (Listof String))
  (define (kwik-read filename)
    (with-input-from-file filename
      (λ ()
        (for/list ([line (in-lines)])
                  : (Listof String)
          line))))

  (module+ test
    (let ([tmpfile (make-temporary-file)])
      (with-output-to-file tmpfile #:exists 'replace
        (λ ()
          (displayln "The Nellie,")
          (displayln "a cruising yawl,")
          (displayln "swung to her anchor without a flutter of sails,")
          (displayln "and was at rest.")))
      (define actual (kwik-read tmpfile))
      (define expect (file->lines tmpfile))
      (delete-file tmpfile)
      (check-equal? actual expect)))

  (: kwik-split : (Listof String) -> Lines)
  (define (kwik-split lines)
    (map #{string-split :: (String -> Words)} lines))

  (module+ test
    (check-equal?*
      [(kwik-split '())
       '()]
      [(kwik-split '("hello    world"))
       '(("hello" "world"))]))

  ; Move first element to last position
  (: circular-shift : Words -> Words)
  (define (circular-shift words)
    (append (rest words) (list (first words))))

  (module+ test
    (check-equal?*
      [(circular-shift '("A" "B" "C"))
       '("B" "C" "A")]))

  (: all-circular-shifts : Words -> (Listof Words))
  (define (all-circular-shifts words)
    (for/fold ([all-shifts (list words)])
              ([i (in-range 1 (length words))])
              : (Listof Words)
      (cons (circular-shift (first all-shifts))
            all-shifts)))

  (module+ test
    (check-equal?*
      [(all-circular-shifts '("A" "B" "C"))
       '(("C" "A" "B") ("B" "C" "A") ("A" "B" "C"))]))

  (: alphabetize : Lines -> Lines)
  (define (alphabetize all-shifts)
    (sort all-shifts shift<?))

  (module+ test
    (check-equal?*
      [(alphabetize '(("A" "B" "C") ("B" "C") ("A")))
       '(("A") ("A" "B" "C") ("B" "C"))]))

  ; Lexicographic order on equal-length lists of words
  (: shift<? : Words Words -> Boolean)
  (define (shift<? shift1 shift2)
    (match* (shift1 shift2)
     [('() _) ; first list empty, don't care about second
      #t]
     [(_ '()) ; first list non-empty, second empty
      #f]
     [((cons s1 shift1-rest) (cons s2 shift2-rest))
      (or (string<? s1 s2)
          (and (string=? s1 s2)
               (shift<? shift1-rest shift2-rest)))]))

  (module+ test
    (check-equal?*
      [(shift<? '() '())
       #t]
      [(shift<? '("A" "B") '("A" "C"))
       #t]))

  (: kwik-display : Lines -> Void)
  (define (kwik-display all-sorted-shifts)
    (: display-words : Words -> Void)
    (define (display-words words)
      (display (first words))
      (for ([word (in-list (cdr words))])
        (display " ")
        (display word))
      (newline))
    (for-each display-words all-sorted-shifts))

  (module+ test
    (parameterize ([current-output-port (open-output-string)])
      (kwik-display '(("A") ("B" "C")))
      (check-equal?
        (get-output-string (current-output-port))
        "A\nB C\n")))

  (: all-circular-shifts* : Lines -> (Listof Lines))
  (define (all-circular-shifts* lines)
    (map all-circular-shifts lines))

  (module+ test
    (check-equal?
      (all-circular-shifts* '(("A" "B" "C") ("D")))
      '((("C" "A" "B") ("B" "C" "A") ("A" "B" "C")) (("D")))))

  (: kwik-index : Path-String -> Void)
  (define (kwik-index file-name)
    (define all-lines (kwik-split (kwik-read file-name)))
    (define all-shifts (append* (all-circular-shifts* all-lines)))
    (kwik-display (alphabetize all-shifts)))

  (module+ test
    (parameterize ([current-output-port (open-output-string)])
      (define tmpfile (make-temporary-file))
      (with-output-to-file tmpfile #:exists 'replace
        (λ ()
          (displayln "imagine if this")
          (displayln "took 2 weeks to write")))
      (kwik-index tmpfile)
      (delete-file tmpfile)
      (check-equal?
        (get-output-string (current-output-port))
        (string-join '(
          "2 weeks to write took"
          "if this imagine"
          "imagine if this"
          "this imagine if"
          "to write took 2 weeks"
          "took 2 weeks to write"
          "weeks to write took 2"
          "write took 2 weeks to\n") "\n"))))

  (module+ main
    (require racket/cmdline)
    (: *output-to* (Parameterof Any))
    (define *output-to* (make-parameter #f))
    (command-line
      #:program "kwik index"
      #:once-each
      [("-o" "--output")
       output-to ; user-supplied input
       "Write output to file"
       (*output-to* output-to)] ; update the parameter *output-to*
      #:args (file-name)
      (define output-to (*output-to*)) ; read from parameter *output-to*
      (define out-port
        (if (string? output-to)
          (open-output-file output-to #:exists 'replace)
          (current-output-port)))
      (parameterize ([current-output-port out-port])
        (kwik-index (cast file-name Path-String)))
      (when (string? output-to)
        (close-output-port out-port))))
}

Sample interactions:
@codeblock{
> racket kwik.rkt
kwik index: expects 1 <file-name> on the command line, given 0 arguments

> echo "It is a truth universally acknowledged" > pride-and-prejudice.txt
> racket kwik.rkt -o index.out pride-and-prejudice.txt
> wc -l index.out
6 index.out
}


@section*{Closing}

We started with functions, wrote (and quarantined) unit tests,
 reinforced our design with types, and added a command-line interface.
Going forward we could add @hyperlink["http://docs.racket-lang.org/scribble/index.html"]{Scribble} documentation and share our work as a @hyperlink["http://pkgn.racket-lang.org/"]{package}.

For more on building languages with Racket:
@itemize[
  @item{
    @hyperlink["http://www.hashcollision.org/brainfudge/"]{Fudging up a Racket (html)}
  }
  @item{
    @hyperlink["http://dl.acm.org/authorize?6529547"]{Creating Languages in Racket (pdf)}
  }
  @item{
    @hyperlink["http://www.ccs.neu.edu/home/matthias/manifesto/"]{The Racket Manifesto (html)}
  }
  @item{
    @hyperlink["http://www.terohasu.net/hasu-flatt--els16--preprint.pdf"]{Source-to-Source Compilation via Submodules (pdf)}
  }
]


