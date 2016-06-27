#lang scribble/manual

Title: Tutorial: Using Racket's FFI
Date: 2016-06-27T16:22:11
Tags: Racket, FFI, tutorial, by Asumu Takikawa

@(require scribble/example
          scribble/racket
          "../../utils/utils.rkt"
          (for-syntax racket/base)
          (for-label racket/base ffi/unsafe))
@(define ev (make-base-eval))
@(ev '(require racket/class))

@(define-dummy _cairo_surface_t)
@(define-dummy _cairo_t)
@(define-dummy _cairo_line_cap_t)

I've seen several people ask for a tutorial on Racket's foreign
function interface (FFI), which allows you to dynamically load
C libraries for use in Racket code. While I think the
@hyperlink["http://docs.racket-lang.org/foreign/index.html"]{documentation}
for the FFI is quite good, it is a lot of information to process and
the overview examples may be tricky to run for a beginner.

With that in mind, this blog post will provide a step-by-step tutorial
for Racket's FFI that requires minimal setup. All that you will need to
follow along is a copy of Racket and ideally a DrRacket window.

<!-- more -->

Before getting into the details, I wanted to note that the FFI library
is based on the work of Eli Barzilay and Dmitry Orlovsky. They have
a Scheme Workshop @hyperlink["http://www.ccs.neu.edu/racket/pubs/scheme04-bo.pdf"]{paper}
that you can read if you're curious about the design.

The tutorial will focus on using the @hyperlink["https://www.cairographics.org/"]{Cairo}
graphics library, mainly because it comes bundled with Racket.

To start, let's aim to reproduce the output of the "multi segment caps"
C sample code on Cairo's
@hyperlink["https://www.cairographics.org/samples/"]{samples page}:

@codeblock[#:keep-lang-line? #f]|{
#lang honu
cairo_move_to (cr, 50.0, 75.0);
cairo_line_to (cr, 200.0, 75.0);

cairo_move_to (cr, 50.0, 125.0);
cairo_line_to (cr, 200.0, 125.0);

cairo_move_to (cr, 50.0, 175.0);
cairo_line_to (cr, 200.0, 175.0);

cairo_set_line_width (cr, 30.0);
cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND);
cairo_stroke (cr);
}|

In order to actually draw this example to the screen, we will need a Cairo
surface to draw on. So here is some boilerplate for you to execute before we
actually play with the FFI:

@examples[#:eval ev #:label #f
(require racket/draw)

(define bt (make-bitmap 256 256))
(define bt-surface (send bt get-handle))
]

This uses the Racket drawing library @racketmodname[racket/draw] to construct
a bitmap object that we'll draw on. The @racket[get-handle] method just extracts a
low-level Cairo surface value that we can use.

NB: these code snippets don't come with a @tt{#lang} declaration because
they simulate interactions at the REPL/interaction area in DrRacket.
When following along, just copy & paste the snippets into your REPL.

Our first real step is to import the FFI itself:

@examples[#:eval ev #:label #f (require ffi/unsafe)]

As the module name suggests, the FFI is @emph{unsafe} and can cause your Racket process
to segfault. If you're following along in DrRacket, you will want to save your file
frequently.

Next, we can load the Cairo library to obtain a @ffitech{foreign-library value}, which
is a handle that we use to access C values and functions:

@examples[#:eval ev #:label #f
(define cairo-lib (ffi-lib #f))
]

Since Cairo has already been loaded by the Racket process because of the
@racketmodname[racket/gui] import earlier, we can supply @racket[#f] here as an
argument to @racket[ffi-lib]. Normally you supply the name of a shared
library file such as @racket["libcairo"]:

@racketblock[
(define cairo-lib (ffi-lib "libcairo" '(#f 2)))
]

The last list argument specifies the accepted versions (@racket[#f] allows
a version-less library). For this post, those details aren't important but
see the docs on @racket[ffi-lib] if you're curious.

@section[#:style 'unnumbered]{Extracting functions}

Since the Racket FFI is a dynamic interface, we can pull out C functions
at run-time using the @racket[get-ffi-obj] function. The @racket[get-ffi-obj]
function takes three arguments:

@itemize[
  @item{The name of the value as a string (or symbol or bytestring)}
  @item{a foreign library value, and}
  @item{a @ffitech{C type}, which is a type description that tells
        the FFI how to marshall between Racket and C.}
]

C types are a crucial concept for the FFI. They range from relatively
simple types like @racket[_int] and @racket[_pointer] to more complicated
type constructors such as @racket[_enum] and @racket[_fun]. As you probably noticed,
C types are prefixed with an underscore by convention. You can also define
your own types by calling @racket[make-ctype] with two functions that
handle marshalling between C and Racket code.

To make progress with our Cairo code, we need to create a drawing context from
the surface object @racket[bt-surface] that we defined a while ago. The
relevant function in the Cairo docs is
@hyperlink["https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-create"]{@tt{cairo_create}},
which has the following type signature:

@codeblock[#:keep-lang-line? #f]|{
#lang honu
/* NB: this is C code */
cairo_t * cairo_create (cairo_surface_t *target);
}|

To use this function from Racket, we will need to create a C type that describes
its behavior. As you can see, the function takes a pointer to a @tt{cairo_surface_t} and
returns a pointer to a @tt{cairo_t}. Let's start with a very simple C type
that matches up with this behavior: @racketblock[(_fun _pointer -> _pointer)]
This type provides very little safety (in particular, it lets you mix up different
kinds of pointers), but it will work as a first step.
Note that the FFI library uses infix arrow notation for its @racket[_fun] type.

The following definition shows how to use this type to obtain a foreign
function:

@examples[#:eval ev #:label #f
(define cairo-create
  (get-ffi-obj "cairo_create" cairo-lib
               (_fun _pointer -> _pointer)))
]

Then we can use @racket[cairo-create] as an ordinary racket function:

@examples[#:eval ev #:label #f
(define ctx (cairo-create bt-surface))
ctx
]

@section[#:style 'unnumbered]{Interlude: more type safety}

Before we move on to completing the Cairo sample, lets consider the safety of the
C type we used again. Since we only specified @racket[_pointer] types, it is easy
to accidentally misuse the function:

@racketblock[
(code:comment "You may not want to actually run this")
(code:comment "a cairo_t is not a cairo_surface_t")
(cairo-create (cairo-create bt-surface))
]

To prevent such bad uses, it is good practice to use @emph{tagged} pointer types
using @racket[define-cpointer-type]. Here are two example definitions that
correspond to the @tt{cairo_t} and @tt{cairo_surface_t} types from earlier:

@examples[#:eval ev #:label #f
(code:comment "The leading underscores are mandatory")
(define-cpointer-type _cairo_t)
(define-cpointer-type _cairo_surface_t)
]

We can then redefine @racket[cairo-create] with a better type, which will
prevent ill-typed calls:

@examples[#:eval ev #:label #f
(define cairo-create
  (get-ffi-obj "cairo_create" cairo-lib
               (_fun _cairo_surface_t -> _cairo_t)))
(eval:error (cairo-create (cairo-create bt-surface)))
]

Unfortunately our old definition of @racket[ctx] doesn't have this tag:

@examples[#:eval ev #:label #f
(cpointer-has-tag? ctx 'cairo_t)
]

Which means we will see errors if we try to use it in future interactions with
the more precise C type. To get around this, it's also possible to update
existing pointers with a tag like this:

@examples[#:eval ev #:label #f
(cpointer-push-tag! ctx 'cairo_t)
(cpointer-has-tag? ctx 'cairo_t)
]

Executing the tag push above is necessary to get some of the following snippets
to work (if you are following along step-by-step).

@section[#:style 'unnumbered]{Macros for reducing boilerplate}

Now let's start building the FFI bindings for the functions in the Cairo sample.
First, let's go ahead and look at all of the types for the sample functions
from the C API docs:

@codeblock[#:keep-lang-line? #f]|{
#lang honu
void cairo_move_to (cairo_t *cr, double x, double y);
void cairo_line_to (cairo_t *cr, double x, double y);
void cairo_set_line_width (cairo_t *cr, double width);
void cairo_set_line_cap (cairo_t *cr, cairo_line_cap_t line_cap);
void cairo_stroke (cairo_t *cr);
}|

Starting with @tt{cairo_move_to}, we can set up a definition like we did
with @tt{cairo_create} before:

@examples[#:eval ev #:label #f
(define cairo-move-to
  (get-ffi-obj
   "cairo_move_to"
   cairo-lib
   (_fun _pointer _double _double -> _void)))
]

This starts to look awfully verbose once you start writing more of these
definitions. Luckily, the FFI library comes with some definition forms
in the @racketmodname[ffi/unsafe/define] library that help reduce the
verbosity. Here's an alternative definition of @racket[cairo-move-to]
using the @racket[define-ffi-definer] form from
@racketmodname[ffi/unsafe/define].

@examples[#:eval ev #:label #f
(require ffi/unsafe/define)

(define-ffi-definer define-cairo cairo-lib)
(define-cairo cairo-move-to
  (_fun _cairo_t _double _double -> _void)
  #:c-id cairo_move_to)
]

As you can see, the @racket[define-ffi-definer] form lets you define a
new macro that lets you avoid writing the library value over and over.
If you stick to using C-style identifiers with underscores (e.g.,
@racket[cairo_move_to]) you also don't need to supply the C name either.

The definitions for @tt{cairo_line_to}, @tt{cairo_set_line_width}, and
@tt{cairo_stroke} aren't very interesting, so I'll just include them
below without comment:

@examples[#:eval ev #:label #f
(define-cairo cairo-line-to
  (_fun _cairo_t _double _double -> _void)
  #:c-id cairo_line_to)
(define-cairo cairo-set-line-width
  (_fun _cairo_t _double -> _void)
  #:c-id cairo_set_line_width)
(define-cairo cairo-stroke
  (_fun _cairo_t -> _void)
  #:c-id cairo_stroke)
]

The @tt{cairo_set_line_cap} case is more interesting because the type
@tt{cairo_line_cap_t} is a C enumeration type. Racket's FFI comes with
convenience forms for defining enumeration types---though it's possible
to encode them yourself too. The general philosophy of the Racket FFI
is to keep the C parts to a minimum and let you build abstractions
in Racket libraries. Here's a quote from the Barzilay and Orlovsky
paper on that:

@nested{Our design follows a simple principle: keep C-level
functionality to a minimum.}

and specifically about enumerations:

@nested{
For example, the C level part of our interface does not commit to a
specific implementation for enumerations — it simply exposes C integers.
}

To define an enumeration, we can use the @racket[_enum] form. This procedure
sets up a new C type which converts between Racket symbols and the underlying
integer representations. For the @tt{cairo_line_cap_t} type, it suffices to
just supply the cases as a list of symbols:

@examples[#:eval ev #:label #f
(define _cairo_line_cap_t
  (_enum '(butt round square)))
]

The exact symbols that we specify are not important, since they just map to
integers anyway. The choice depends on what is convenient for the Racket interface.
It's also possible to specify how the symbols map to integers more precisely
(see the docs on @racket[_enum] for those details).

Given this type, we can specify the type for the line cap function:

@examples[#:eval ev #:label #f
(define-cairo cairo-set-line-cap
  (_fun _cairo_t _cairo_line_cap_t -> _void)
  #:c-id cairo_set_line_cap)
]

@section[#:style 'unnumbered]{Putting it all together}

Now that we have foreign function definitions for all of the relevant procedures,
we can just transcribe the example from the beginning into Racket syntax:

@examples[#:eval ev #:label #f #:no-result
(cairo-move-to ctx 50.0 75.0)
(cairo-line-to ctx 200.0 75.0)
code:blank
(cairo-move-to ctx 50.0 125.0)
(cairo-line-to ctx 200.0 125.0)
code:blank
(cairo-move-to ctx 50.0 175.0)
(cairo-line-to ctx 200.0 175.0)
code:blank
(cairo-set-line-width ctx 30.0)
(cairo-set-line-cap ctx 'round)
(cairo-stroke ctx)
]

Executing these procedure calls will draw into the Cairo surface we set up earlier,
which is connected to our original Racket bitmap object @racket[bt]. To see the
results of what we drew, we can just evaluate @racket[bt] at the REPL. But it's
a little nicer if we use the @racketmodname[pict] library to draw a frame around
it to distinguish it from the background:

@examples[#:eval ev #:label #f
(require pict)
(linewidth 2 (frame (bitmap bt)))
]

And we're done! Of course, there is a lot more to the FFI. For example, I haven't
covered how to handle C functions that return multiple results through pointer
arguments. Or how to interoperate between Racket and C structs. I'm hoping to
cover these in a future blog post, but in the meantime happy FFI hacking!
