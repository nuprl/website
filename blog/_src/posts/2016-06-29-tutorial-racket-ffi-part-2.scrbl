#lang scribble/manual

Title: Tutorial: Racket FFI, Part 2
Date: 2016-06-29T18:48:17
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
@(define-dummy _cairo_text_extents_t)
@(define-dummy _cairo_text_extents_t-pointer)

This is part 2 of my tutorial on using the Racket FFI. If you haven't read
part 1 yet, you can find it
@hyperlink["http://prl.ccs.neu.edu/blog/2016/06/27/tutorial-using-racket-s-ffi/"]{here}.

Part 2 will continue with more Cairo examples. In this installment, I plan to
go over some more advanced FFI hacking such as handling computed argument
values, custom return arguments, and using C structs.

<!-- more -->

First, here's the core code from part 1 condensed and re-arranged into an
example that you can copy and paste into your definitions area:

@examples[#:eval ev #:lang
racket
(require racket/draw
         racket/gui/base
         ffi/unsafe
         ffi/unsafe/define
         pict)
code:blank
(code:comment "bitmap magic")
(define bt (make-bitmap 256 256))
(define bt-surface (send bt get-handle))
code:blank
(code:comment "C types")
(define-cpointer-type _cairo_t)
(define-cpointer-type _cairo_surface_t)
(define _cairo_line_cap_t
  (_enum '(butt round square)))
code:blank
(define cairo-lib (ffi-lib #f))
(define-ffi-definer define-cairo cairo-lib)
code:blank
(code:comment "the foreign functions")
(define-cairo cairo-create
  (_fun _cairo_surface_t -> _cairo_t)
  #:c-id cairo_create)
(define-cairo cairo-move-to
  (_fun _cairo_t _double _double -> _void)
  #:c-id cairo_move_to)
(define-cairo cairo-line-to
  (_fun _cairo_t _double _double -> _void)
  #:c-id cairo_line_to)
(define-cairo cairo-set-line-width
  (_fun _cairo_t _double -> _void)
  #:c-id cairo_set_line_width)
(define-cairo cairo-stroke
  (_fun _cairo_t -> _void)
  #:c-id cairo_stroke)
(define-cairo cairo-set-line-cap
  (_fun _cairo_t _cairo_line_cap_t -> _void)
  #:c-id cairo_set_line_cap)
code:blank
(define ctx (cairo-create bt-surface))
code:blank
(code:comment "Bitmap -> Pict")
(code:comment "a helper for displaying the bitmap")
(define (show bt)
  (linewidth 2 (frame (bitmap bt))))
]

To start off, let's look at another C example from the Cairo
@hyperlink["https://www.cairographics.org/samples/"]{samples page}.
This time we will look at the "dash" example, which has a
use of an input array:

@codeblock[#:keep-lang-line? #f]|{
#lang honu
double dashes[] = {50.0,  /* ink */
                   10.0,  /* skip */
                   10.0,  /* ink */
                   10.0   /* skip*/
                  };
int    ndash  = sizeof (dashes)/sizeof(dashes[0]);
double offset = -50.0;

cairo_set_dash (cr, dashes, ndash, offset);
cairo_set_line_width (cr, 10.0);

cairo_move_to (cr, 128.0, 25.6);
cairo_line_to (cr, 230.4, 230.4);
cairo_rel_line_to (cr, -102.4, 0.0);
cairo_curve_to (cr, 51.2, 230.4, 51.2, 128.0, 128.0, 128.0);

cairo_stroke (cr);
}|

The most interesting function here is @tt{cairo_set_dash}, which takes an
array argument. The only other new functions are @tt{cairo_rel_line_to}
and @tt{cairo_curve_to} which have very straightforward C types:

@examples[#:eval ev #:label #f
(define-cairo cairo-rel-line-to
  (_fun _cairo_t _double _double -> _void)
  #:c-id cairo_rel_line_to)
(define-cairo cairo-curve-to
  (_fun _cairo_t
        _double _double
        _double _double
        _double _double
        -> _void)
  #:c-id cairo_curve_to)
]

Meanwhile, the C type signature for @tt{cairo_set_dash} from the Cairo
docs looks like this:

@codeblock[#:keep-lang-line? #f]|{
#lang honu
void cairo_set_dash (cairo_t *cr,
                     const double *dashes,
                     int num_dashes,
                     double offset);
}|

Something to note about the arguments is that @tt{num_dashes}
encodes the length of the array @tt{dashes}. This will come up later when
we want to make the C type for this function more convenient.

On the Racket side, it's natural to represent the array of dashes as either a
list or @reftech{vector} of numbers. Given that, a fairly literal translation of
the C type above might look like the following:

@examples[#:eval ev #:label #f
(define-cairo cairo-set-dash
  (_fun _cairo_t
        (_list i _double)
        _int
        _double
        -> _void)
  #:c-id cairo_set_dash)
]

This type includes a type constructor we haven't seen yet: @racket[_list].
This is a so-called @ffitech{custom function type} that has special meaning
inside of a @racket[_fun] type. It lets you convert between a Racket list and
a C array. Since arrays are often used for both input and output of a C function,
the constructor requires you to specify the mode in which you are using the
array.

Since we only want to provide a list from the Racket side to C, we'll use
the @racket[i] input mode. We can then call the function like this:

@racketblock[
(cairo-set-dash ctx
                (list 50.0 10.0 10.0 10.0)
                4
                -50.0)
]

Note that because of how we defined the type of @racket[cairo-set-dash] we had to
provide the length of the input list as a separate argument! This seems pretty
silly since it's very easy to get the length of a Racket list and because this
is a likely source of mistakes. It would be preferable to compute the length
argument automatically.

Luckily, the @racket[_fun] type constructor actually lets you do this with the
@racket[(name : type)] syntax for naming arguments in combination with the
@racket[(type _= expr)] syntax for supplying computed arguments:

@examples[#:eval ev #:label #f
(define-cairo cairo-set-dash
  (_fun _cairo_t
        (code:comment "name this argument for later uses in the type")
        [dashes : (_list i _double)]
        (code:comment "a computed argument position")
        [_int = (length dashes)]
        _double
        -> _void)
  #:c-id cairo_set_dash)
]

When a computed argument is specified with a @racket[_=], it's not necessary to provide
the argument on the Racket side. So @racket[cairo-set-dash] is now an arity 3
function that can be called like this:

@racketblock[
(cairo-set-dash ctx
                (list 50.0 10.0 10.0 10.0)
                -50.0)
]

This means we'll never make a mistake in passing the length argument to Cairo.
Just as an aside, it's also possible to use Racket vectors instead of lists by using
the @racket[_vector] type constructor.

Putting it all together, we can reproduce the dashes example like this:

@examples[#:eval ev #:no-prompt #:label #f
(define dashes '(50.0 10.0 10.0 10.0))
(define offset -50.0)
code:blank
(cairo-set-dash ctx dashes offset)
(cairo-set-line-width ctx 10.0)
code:blank
(cairo-move-to ctx 128.0 25.6)
(cairo-line-to ctx 230.4 230.4)
(cairo-rel-line-to ctx -102.4 0.0)
(cairo-curve-to ctx 51.2 230.4 51.2
                    128.0 128.0 128.0)
code:blank
(cairo-stroke ctx)
code:blank
(show bt)
]

@section{Result arguments and C structs}

For some more advanced FFI hacking, let's consider the problem of drawing some
text into a predetermined space. In particular, we have our usual 256x256
bitmap that we want to draw some text into:

@examples[#:eval ev
(define txt-bt (make-bitmap 256 256))
(define txt-surface (send txt-bt get-handle))
(define txt-ctx (cairo-create txt-surface))
]

Our challenge is to make a Racket function that takes a string (let's
assume we can draw it in one line) and draws it into this bitmap.
Since we are taking an arbitrary string, we will need to figure out
how to scale the text to fit. To make it simple, let's just scale it the
text to fit the width and assume the height will be okay.

To implement the key step of measuring the text size, we can use the
@hyperlink["https://www.cairographics.org/manual/cairo-text.html#cairo-text-extents"]{@tt{cairo_text_extents}}
function. Its type signature is as follows:

@codeblock[#:keep-lang-line? #f]|{
#lang honu
void
cairo_text_extents (cairo_t *cr,
                    const char *utf8,
                    cairo_text_extents_t *extents);
}|

The interesting part of this signature is that
@hyperlink["https://www.cairographics.org/manual/cairo-cairo-scaled-font-t.html#cairo-text-extents-t"]{@tt{cairo_text_extents_t}}
is a struct type:

@codeblock[#:keep-lang-line? #f]|{
#lang honu
/* from the Cairo docs */
typedef struct {
    double x_bearing;
    double y_bearing;
    double width;
    double height;
    double x_advance;
    double y_advance;
} cairo_text_extents_t;
}|

We haven't yet seen how to handle C structs with the FFI, but that won't be much
of a problem: support for structs
comes built-in and is pretty straightforward too. We can directly translate the documented
definition above into a @racket[define-cstruct] declaration:

@examples[#:eval ev #:label #f
(code:comment "the leading underscore is mandatory")
(define-cstruct _cairo_text_extents_t
  ([x-bearing _double]
   [y-bearing _double]
   [width _double]
   [height _double]
   [x-advance _double]
   [y-advance _double]))
]

This declaration does a couple of things. First, it defines a bunch of handy C types
for us for the struct:

@examples[#:eval ev #:label #f
_cairo_text_extents_t
(code:comment "pointer to struct")
_cairo_text_extents_t-pointer
(code:comment "allows NULL pointer")
_cairo_text_extents_t-pointer/null
]

Along with functions that look like regular Racket struct operations:

@examples[#:eval ev #:label #f
make-cairo_text_extents_t
cairo_text_extents_t-width
cairo_text_extents_t?
set-cairo_text_extents_t-width!
]

With the struct type defined, it's easy to come up with a rudimentary
interface for @racket[cairo-text-extents]:

@examples[#:eval ev #:label #f
(define-cairo cairo-text-extents
  (_fun _cairo_t
        _string
        _cairo_text_extents_t-pointer
        -> _void)
  #:c-id cairo_text_extents)
]

In order to actually use this function, we need to create a text extents struct
and provide it as a pointer. Conveniently, the FFI treats instances of C structs
as pointers so this is pretty straightforward:

@examples[#:eval ev #:label #f
(define extents
  (make-cairo_text_extents_t
   0.0 0.0 0.0 0.0 0.0 0.0))
(cairo-text-extents
 txt-ctx "hello world" extents)
(cairo_text_extents_t-width extents)
]

This style of programming feels awfully imperative though. Since we're in a
functional language, it would be nice to avoid the manual creation of the struct.
We can define an alternate version of the @racket[cairo-text-extents] FFI wrapper
by combining named arguments, a new @racket[_ptr] type constructor,
and a neat feature of @racket[_fun] that lets you customize
the return result:

@examples[#:eval ev #:label #f
(define-cairo cairo-text-extents*
  (_fun _cairo_t
        _string
        (code:comment "named args and _ptr")
        [ext : (_ptr o _cairo_text_extents_t)]
        (code:comment "the return result of the C function")
        -> _void
        (code:comment "custom return result for the wrapper")
        -> ext)
  #:c-id cairo_text_extents)
]

The @racket[_ptr] constructor works like the @racket[_list] constructor we saw
earlier in this blog post but typically for a single object. Since we are passing
in a value to use as an output, we specify the @racket[o] mode to @racket[_ptr].
In output mode, this type will automatically allocate a new instance of the type
(using the @racket[malloc] function) and arrange for it to be passed in as
a pointer.

The strangest part of this example is that there are now two uses of the
@racket[->] form! By providing a second arrow, we can customize what the FFI wrapper
returns. The expression to the right of the second arrow is just Racket code that can
reference previously named arguments. In this case, we return the struct that
was allocated for us.

Using this new version of the wrapper is much simpler:

@examples[#:eval ev #:label #f
(cairo_text_extents_t-width
 (cairo-text-extents* txt-ctx "hello world"))
]

With that in hand, it's pretty easy to write the function we set out to write:

@examples[#:eval ev #:label #f #:no-result
(define-cairo cairo-show-text
  (_fun _cairo_t _string -> _void)
  #:c-id cairo_show_text)
code:blank
(define-cairo cairo-scale
  (_fun _cairo_t _double _double -> _void)
  #:c-id cairo_scale)
code:blank
(code:comment "String -> Void")
(code:comment "draws a string scaled horizontally")
(define (fit-text str)
  (define padding 20)
  (cairo-move-to txt-ctx (/ padding 2.0) 128.0)
  (define extents
    (cairo-text-extents* txt-ctx str))
  (define x-bearing
    (cairo_text_extents_t-x-bearing
     extents))
  (define width
    (cairo_text_extents_t-width
     extents))
  (define scale (/ (- 256.0 padding)
                   (+ x-bearing width)))
  (cairo-scale txt-ctx scale scale)
  (cairo-show-text txt-ctx str))]

And to conclude part 2 of this tutorial, here's an example use
of the new @racket[fit-text] function:

@examples[#:eval ev #:label #f
(fit-text "Saluton, Mondo / Hallo, mundo")
(show txt-bt)
]
