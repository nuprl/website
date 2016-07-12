#lang scribble/manual

Title: Tutorial: Racket FFI, part 3
Date: 2016-07-11T17:33:40
Tags: Racket, FFI, tutorial, by Asumu Takikawa

@(require scribble/example
          scribble/racket
          "../../utils/utils.rkt"
          (for-syntax racket/base)
          (for-label racket/base ffi/unsafe))
@(define ev (make-base-eval))
@(ev '(require racket/class
               racket/list
               racket/match))

@(define-dummy _cairo_surface_t)
@(define-dummy _cairo_t)
@(define-dummy _cairo_line_cap_t)
@(define-dummy _cairo_text_extents_t)
@(define-dummy _cairo_text_extents_t-pointer)
@(define-dummy _cairo_path_data_type_t)
@(define-dummy _cairo_path_data_t)
@(define-dummy _cairo_status_t)
@(define-dummy _cairo_path_t)
@(define-dummy _simple_cairo_path_t)
@(define-dummy _cairo_path_t*)

This is part 3 of my tutorial for using the Racket FFI. You can find part 1
@hyperlink["http://prl.ccs.neu.edu/blog/2016/06/27/tutorial-using-racket-s-ffi/"]{here}
and part 2
@hyperlink["http://prl.ccs.neu.edu/blog/2016/06/29/tutorial-racket-ffi-part-2/"]{here}.

In this post, we will experiment with some low-level operations with pointers,
union types, and custom C types. The main takeaway will be the custom C types,
which let you define abstractions that hide the details of the C representation
when manipulating data in Racket.

<!-- more -->

As in the second post, let's start with some prologue code that establishes
the definitions from the previous two posts. But first, I'm getting tired of
writing the @tt{#:c-id identifier} notation for the underscored C function
names.

Instead, let's use a third-party package that I wrote that lets you avoid the
boilerplate. To install the package, you can either invoke the following
incantation in a command-line:

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/text
$ raco pkg install ffi-definer-convention
}|

or you can just execute the following snippet in Racket:

@examples[#:eval ev #:label #f
(require pkg)
(pkg-install-command #:skip-installed #t "ffi-definer-convention")
]

This will install the package and compile its contents. If you're curious, the
docs for the package are available 
@hyperlink["http://docs.racket-lang.org/ffi-definer-convention/index.html"]{here}.

@bold{Note:} if you've never installed a package before, you may want to glance
at the
@hyperlink["http://docs.racket-lang.org/pkg/getting-started.html"]{package system docs}.
A tl;dr of packages is that they bundle Racket @reftech{collections}, which are
sets of modules that you can refer to in a location-independent fashion such as
@racketmodname[pict] or @racketmodname[racket/list].

Anyhow, here's the prologue code:

@examples[#:eval ev #:lang
racket
(require racket/draw
         ffi/unsafe
         (code:comment "avoid conflict with below")
         (except-in ffi/unsafe/define
                    define-ffi-definer)
         (code:comment "the new 3rd-party pkg")
         ffi-definer-convention
         pict)
code:blank
(code:comment "C types")
(define-cpointer-type _cairo_t)
(define-cpointer-type _cairo_surface_t)
(define _cairo_line_cap_t
  (_enum '(butt round square)))
code:blank
(define cairo-lib (ffi-lib #f))
(define-ffi-definer define-cairo cairo-lib
  (code:comment "describes how to transform from")
  (code:comment "Racket to C ids")
  #:make-c-id convention:hyphen->underscore)
code:blank
(code:comment "the foreign functions")
(code:comment "note lack of #:c-id keyword arguments")
(define-cairo cairo-create
  (_fun _cairo_surface_t -> _cairo_t))
(define-cairo cairo-move-to
  (_fun _cairo_t _double _double -> _void))
(define-cairo cairo-line-to
  (_fun _cairo_t _double _double -> _void))
(define-cairo cairo-set-line-width
  (_fun _cairo_t _double -> _void))
(define-cairo cairo-stroke
  (_fun _cairo_t -> _void))
(define-cairo cairo-set-line-cap
  (_fun _cairo_t _cairo_line_cap_t -> _void))
code:blank
(code:comment "(_cairo_t -> Void) -> Pict")
(code:comment "do some drawing and give us the pict")
(define (do-cairo f)
  (define bt (make-bitmap 256 256))
  (define bt-surface (send bt get-handle))
  (f (cairo-create bt-surface))
  (linewidth 2 (frame (bitmap bt))))
]

Notice that the @racket[define-cairo] forms don't have any @tt{#:c-id} keywords
anymore. Instead, the prologue code uses an overriden @racket[define-ffi-definer]
from my package that supports a @tt{#:make-c-id} keyword that lets you specify
a naming convention to follow.

Also, instead of creating a single bitmap and drawing into it, we now have a
@racket[do-cairo] function that takes a drawing function. When called,
@racket[do-cairo] will call the given function with a new bitmap object and
return the result.

Now let's get to the main point of this blog post. Let's say that we want to play
with Cairo @hyperlink["https://www.cairographics.org/manual/cairo-Paths.html"]{path}
objects this time. A path is
@hyperlink["https://www.cairographics.org/manual/cairo-Paths.html#cairo-path-t"]{defined}
as a struct with the following structure:

@codeblock[#:keep-lang-line? #f]|{
#lang honu
typedef struct {
    cairo_status_t status;
    cairo_path_data_t *data;
    int num_data;
} cairo_path_t;
}|

To manipulate paths, we want to define a FFI C type that corresponds to this
struct definition. But before that, it's
useful to define C types for the types of values in the path struct's fields. First,
let's specify that a @tt{cairo_status_t} is an integer type:

@examples[#:eval ev #:label #f
(define _cairo_status_t _int)
]

It's actually an enum, but for the examples in this post we don't care about
distinguishing different statuses. Next, the data field of a path struct is an
array of
@hyperlink["https://www.cairographics.org/manual/cairo-Paths.html#cairo-path-data-t"]{path data objects}.
Each path data object is a @tt{cairo_path_data_t},
which is specified with a C union:

@codeblock[#:keep-lang-line? #f]|{
#lang honu
union _cairo_path_data_t {
    struct {
	cairo_path_data_type_t type;
	int length;
    } header;
    struct {
	double x, y;
    } point;
};
}|

Helpfully, the FFI library comes with support for unions with the
@racket[_union] type constructor. The constructor takes arbitrarily
many arguments, one for each sub-case in the union. It's pretty
straightforward to specify this type too:

@examples[#:eval ev #:label #f
(code:comment "the path data type is just an enum")
(define _cairo_path_data_type_t
  (_enum '(move-to line-to curve-to close-path)))
(define _cairo_path_data_t
  (_union (code:comment "the header case")
          (_list-struct _cairo_path_data_type_t
                        _int)
          (code:comment "the point case")
          (_list-struct _double _double)))
]

There's a new type constructor here so let me explain that first.
The @racket[_list-struct] constructor translates between a C struct
and a fixed-length list of C objects on the Racket side. Unlike
@racket[define-cstruct], this constructor doesn't define any selectors
or anything like that. Instead, you can manipulate the struct as an
ordinary list.

Each of the path data structs in the path data array will be manipulated with
the @racket[_cairo_path_data_t] type. Union types are a bit cumbersome unfortunately
because the programmer has to distinguish the cases in the union manually
on the Racket-side. Let me illustrate this with some code:

@examples[#:eval ev #:label #f
(code:comment "create a union from a list of doubles")
(define a-union-val
  (cast (list 1.3 5.8)
        (code:comment "source type")
        (_list-struct _double _double)
        (code:comment "target type")
        _cairo_path_data_t))
a-union-val
]

This snippet first construct a union object (via the @racket[_cairo_path_data_t]
type) using a @racket[cast]. A cast is an operation that lets you coerce
from one C type to another. We use it in this example since it's an easy way
to generate a union object.

The second line shows that a union prints as an opaque object. You can't do
anything with a union in Racket unless you project it to one of the sub-cases with
the @racket[union-ref] function.
This projection is @emph{unsafe}, in the sense that if you don't know which of
the sub-cases in the union is the correct one, you will get potentially non-sensical
data out of the union.

More concretely, let's see what happens if we try to extract a value out of the
union both correctly and incorrectly:

@examples[#:eval ev #:label #f
(code:comment "correct (matches construction)")
(code:comment "cases are zero-indexed and ordered as written")
(union-ref a-union-val 1)
(code:comment "incorrect, error")
(eval:error (union-ref a-union-val 0))
]

Note that in the incorrect case we get an error saying that the FFI failed
to convert the C value to a Racket value following the given C type. We were
lucky in this case, but in general you can have silent failures where the
data is nonsense.

With union types like these, there is usually some way to figure out which case
of the union you are in. This may be accomplished in C using an extra struct
field or a variable that indicates the variant. Alternatively, there may be some
set order that cases appear in data structures.

With this Cairo API in particular, the position of the elements in the array
tells you which of the union cases it's in. The array always starts with a
header element, and then follows with some number of data elements (the exact
number is determined by the type indicated in the header). We can therefore
reference the appropriate element of the union based on this ordering.

So before moving on, let's recap: so far we have made C types that describe
the data elements in a cairo path with unions. Next we'll figure out how to
deal with the array itself.

@section[#:style 'unnumbered]{Some low-level operations}

Since we still don't have a C type for @tt{cairo_path_t}, let's go ahead
and make a simple one where we punt on the work of specifying the array
type:

@examples[#:eval ev #:label #f
(define _simple_cairo_path_t
  (_list-struct _cairo_status_t
                _pointer
                _int))
]

In this type, we have specified the array as a bare @racket[_pointer].
For some added safety, we could also use something like
@racket[(_cpointer 'cairo_status_t)], which sets up a tagged pointer
type like we saw in the first blog post with
@racket[define-cpointer-type].

We've seen the @racket[_pointer] type before, but haven't actually done
anything with values of those types except pass them around as arguments.
It turns out it is possible to do a bit more with pointers.

Before we get to that, let's go ahead and set up an FFI binding for
@tt{cairo_copy_path} so that we can obtain a path struct to manipulate:

@examples[#:eval ev #:label #f
(define-cairo cairo-copy-path
  (_fun _cairo_t -> _pointer))

(define a-path #f)

(do-cairo (λ (ctx)
            (code:comment "Do stuff to make the current")
            (code:comment "path non-empty")
            (cairo-move-to ctx 50.0 50.0)
            (cairo-line-to ctx 206.0 206.0)
            (cairo-move-to ctx 50.0 206.0)
            (cairo-line-to ctx 115.0 115.0)
            (code:comment "Get the current path")
            (set! a-path (cairo-copy-path ctx))
            (code:comment "Stroke clears the path")
            (code:comment "so do it last")
            (cairo-stroke ctx)))

a-path
]

Note that @racket[cairo-copy-path] gives us a pointer to a path struct
rather than a path struct directly. Because of that, we need to know
how to manipulate pointers.
The most useful function for pointers is @racket[ptr-ref], which
lets you dereference a pointer and access it at some concrete C type.

@bold{Note:} the @racket[ptr-ref] function also takes an optional
offset argument which we will be used in an example later.

For example, we can use @racket[a-path] as a @racket[_simple_cairo_path_t]:

@examples[#:eval ev #:label #f
(define simple-path
  (ptr-ref a-path _simple_cairo_path_t))
simple-path
]

And now we have a Racket representation of the struct that the pointer
points to. Now notice that the data array field of the struct is also
a pointer as we specified earlier. To convert this to a more useful form,
we can use @racket[ptr-ref] again with an array type:

@examples[#:eval ev #:label #f
(define array
  (ptr-ref (code:comment "the pointer")
           (second simple-path)
           (_array/list _cairo_path_data_t
                        (code:comment "length field")
                        (third simple-path))))
array
]

The elements of the array are all unions, as we would expect. This is
a bit annoying to use though. We have to know the structure of the
array and reference the correct variant appropriately:

@examples[#:eval ev #:label #f
(union-ref (first array) 0)
(union-ref (second array) 1)
(code:comment "nonsense data here, wrong union case")
(union-ref (third array) 1)
]

One thing we could do is write a helper function that converts this array
into a more useful format. It would look at each header, and then consume
the number of data elements specified in the header element (e.g., 1
in the example above because the length includes the header)
and convert them appropriately.

An alternative is to define a @emph{custom C type} that handles all of
this conversion automatically for us, so that as a user of the Cairo
FFI bindings we don't need to think about applying helper functions and
dereferencing pointers.

@section[#:style 'unnumbered]{Custom C types}

I briefly remarked on how to create custom C types in the first blog post,
but let me go over that again in more detail. A custom C type is constructed
by providing a base C type to use along with two conversion functions.
The first function converts from a Racket value to a value that fits the
base C type. The second converts in the other direction from a value of
the base C type to a Racket value.

In this way, it's possible to conduct interesting conversions, such as
dereferencing union objects automatically.

Now let's make a custom C type for Cairo paths that will represent the
data elements as a sequence in which each item in the sequence is a list
with an action symbol followed by the data elements for that action.

First, we'll start by defining a struct type for the Racket representation
of Cairo paths:

@examples[#:eval ev #:label #f
(struct cairo-path (ptr)
  #:property prop:sequence
  (λ (p) (in-cairo-path p)))
]

The representation will store one field @racket[ptr] which, as the name
suggests, will store a pointer value. We'll see what to do with this
pointer later.

This definition uses a @reftech{structure type property} to make instances of
@racket[cairo-path] automatically work as sequences. This means that you
can iterate over them with a @racket[for] loop or apply @racket[sequence-ref]
on them. The property takes a function that takes an instance of the struct
type itself (here @racket[p]) and that returns a sequence.

We'll later define the @racket[in-cairo-path] function that will actually
construct the relevant sequence for us. For now, let's see how to construct
the C type given this struct type:

@examples[#:eval ev #:label #f
(define _cairo_path_t
  (let ()
    (code:comment "Extract pointer out of representation")
    (define (racket->c rkt)
      (cairo-path-ptr rkt))
    (code:comment "Just apply the Racket constructor")
    (define (c->racket cobj)
      (cairo-path cobj))
    (make-ctype _pointer
                racket->c
                c->racket)))
]

The base type for this @racket[_cairo_path_t] is a @racket[_pointer] type. Since
the Cairo API returns pointers to new path values, it's hard to avoid using some
kind of pointer type as the base type here.

This definition right-hand-side defines the two conversion functions between
Racket and C. Both are very simple because of how we've set up the representation.
In the Racket to C case, we simply extract the pointer field of the struct. In
the other direction, we just stuff the pointer into a struct.

The real work is done by the helper function that makes a
@racket[cairo-path] instance work as a sequence.

Starting top-down, let's look at the definition of @racket[in-cairo-path]:

@examples[#:eval ev #:label #f
(code:comment "Cairo-Path -> Sequence")
(define (in-cairo-path path)
  (define pp (cairo-path-ptr path))
  (match-define
    (list _ array-ptr len)
    (ptr-ref pp _simple_cairo_path_t))
  (make-do-sequence
    (λ ()
      (values (pos->element array-ptr)
              (next-pos array-ptr)
              0
              (λ (pos) (< pos len))
              #f #f))))
]

The first thing the function does is extract the pointer out of the
representation, and then immediately calls @racket[ptr-ref] on it. This
lets us manipulate the C path struct using the simple representation we
defined in the first part of the blog post.

@bold{Note:} in case you're not very familiar with Racket pattern matching, the
@racket[match-define] form lets you define potentially multiple variables
using a pattern, similar to Haskell or OCaml's @tt{let} statement.
The first argument clause
is a pattern and the second is an expression to match on. Check it out
in the
@hyperlink["http://docs.racket-lang.org/reference/match.html#%28form._%28%28lib._racket%2Fmatch..rkt%29._match-define%29%29"]{docs}.

After extracting the array pointer and the array length from the
path value, we pass them onto some helper functions that define the
sequence. The usual way to define a new kind of sequence is to use the
@racket[make-do-sequence] function. Essentially, @racket[make-do-sequence]
takes a bunch of arguments that specify how to get the an element of
a sequence, how to advance a sequence, how to start, and how to end
the sequence.

@bold{Note:} technically @racket[make-do-sequence] actually takes a thunk which
produces a number of values. These values are effectively like arguments
though. The reason why it's a thunk is that you may wish to
run some initialization code that runs when the sequence is started
(e.g., imagine opening a network connection), and your sequence functions
(like advancing the sequence) may depend on the result of that
initialization code.

In our case, we supply some curried functions that can extract elements
out of the underlying C array. Here is the @racket[pos->element] function
and its helpers:

@examples[#:eval ev #:label #f
(code:comment "CPointer -> Integer -> Element")
(define ((pos->element ptr) pos)
  (code:comment "Extract the data path header")
  (define header
    (union-ref
     (ptr-ref ptr _cairo_path_data_t pos)
     0))
  (define type   (first header))
  (code:comment "Length includes header, so subtract 1")
  (define len    (sub1 (second header)))
  (define pos*   (add1 pos))
  (define points (get-points ptr pos* len))
  (cons type points))

(code:comment "CPointer Integer Integer -> (Listof Data)")
(define (get-points ptr pos num-points)
  (for/list ([i (in-range num-points)])
    (union-ref (ptr-ref ptr
                        _cairo_path_data_t
                        (code:comment "offset argument")
                        (+ pos i))
               1)))
]

This code encodes the API usage protocol that Cairo specifies, where each
header element in the path is followed by some number of data elements.
Each header specifies the length, so we can loop in @racket[get-points]
from the position after the header until we reach the given length. At
each point, we dereference the appropriate union element.

Advancing the sequence is simpler, since all we need to do is some arithmetic
on the length given by header elements:

@examples[#:eval ev #:label #f
(define ((next-pos ptr) pos)
  (define header
    (union-ref
     (ptr-ref ptr _cairo_path_data_t pos)
     0))
  (define len (second header))
  (+ len pos))
]

Note that determining the end of the sequence is very easy. It's just
a matter of comparing the current position to the total length given in the
path struct, encoded in the expression @racket[(λ (pos) (< pos len))].

Now we can try using a path as a sequence:

@examples[#:eval ev #:label #f
(define-cairo cairo-copy-path
  (_fun _cairo_t -> _cairo_path_t))

(do-cairo (λ (ctx)
            (cairo-move-to ctx 50.0 50.0)
            (cairo-line-to ctx 206.0 206.0)
            (cairo-move-to ctx 50.0 206.0)
            (cairo-line-to ctx 115.0 115.0)
            (define path (cairo-copy-path ctx))
            (code:comment "Using path as a sequence")
            (for ([elem path])
              (displayln elem))
            (cairo-stroke ctx)))
]

Notice how the sequence prints out as an intuitive list of commands
instead of a bunch of opaque union values as we saw before when using
the @racket[_array/list] type.

That concludes part 3 of the FFI tutorial. Hopefully you're now equipped
to deal with union types and custom C types. If not, see the
@hyperlink["http://docs.racket-lang.org/foreign/index.html"]{FFI reference}
for more details on
@hyperlink["http://docs.racket-lang.org/foreign/C_Union_Types.html"]{unions}
and
@hyperlink["http://docs.racket-lang.org/foreign/ctype.html"]{custom C types}.

@emph{Thanks to Ben Greenman for suggestions/feedback and to Sam
Tobin-Hochstadt for suggesting to cover union types!}
