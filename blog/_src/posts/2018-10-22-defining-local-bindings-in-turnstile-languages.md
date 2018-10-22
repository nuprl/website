    Title: Defining Local Bindings in Turnstile Languages
    Date: 2018-10-22T15:05:17
    Tags: turnstile, tutorial, language, dsl, by Sam Caldwell

In [Racket][2], programmers can create powerful abstractions by bundling
together a family of values, functions, and syntax extensions in the form of a
new language. These languages, however, are typically untyped. [Turnstile][1] is a new
Racket {library,language} for creating typed languages by
integrating type checking with Racket's existing tools for
describing languages. The technique is described by fellow PRL'ers
in the paper [*Type Systems as Macros*][3].

Racket encourages language developers to take full advantage of [linguistic
reuse][21] by defining new language forms in terms of existing constructs.
Unsurprisingly, language extensions often retain some of the Racket-y flavor
from the underlying constructs. Implementors save time and energy while users of
the language benefit from the familiarity they already have with the Racket
ecosystem.

Unfortunately, Turnstile does not lend itself to expressing one of Racket's most
ubiquitous idioms: naming local bindings with `define`. Early experience reports
from Turnstile, including my own, suggest that language implementors very much
desire to include `define`-like binding forms in their languages.

This blog post provides a brief overview of what Turnstile is and how it works,
an introduction to defining typed language forms, and how to equip these
languages with a `define` binding form.

<!-- more -->

The code for this blog post can be found [in this gist][14]. To run it, you will
need the Turnstile package, which can be installed with `raco pkg install
turnstile`.

## Turnstile: Typechecking Intertwined with Elaboration

Turnstile provides a convenient way of defining syntax transformations that also
perform typechecking. Since processing the syntax of a form typically involves
some amount of analysis, such as for error checking, it is a natural place to
put the logic for typechecking. With forms defined as such, macro expanding a
program determines both a type and an elaborated term in the target language.

While macro expansion proceeds outside-in, type information typically flows up
from the leaves of the AST during checking. To reconcile the two directions,
Turnstile language forms invoke the macro expander on subexpressions when their
types are needed for the current rule. This expansion yields both the
elaboration of the term and its type, or fails with an error. Turnstile
abstracts over the process of invoking the expander on subterms, allowing
implementors to describe the language in terms of high-level type checking and
elaboration specifications.

## Type & Elaboration Rules

To get a feel for defining language forms in Turnstile, this section walks
through the core of a simply-typed functional language.

### Functions

```racket
(define-type-constructor → #:arity >= 1)
(define-typed-syntax (λ ([x:id (~datum :) τ_in:type] ...) e) ≫
  [[x ≫ x- : τ_in.norm] ... ⊢ e ≫ e- ⇒ τ_out]
  -------------------------------------------------
  [⊢ (#%plain-lambda- (x- ...) e-) ⇒ (→ τ_in.norm ... τ_out)])
```

Looking at this item by item, we see:

1. `define-type-constructor` creates a new type. Here, we say the `→` requires
   at least one parameter.
2. `define-typed-syntax` is the primary way to define a language form in terms
   of its syntactic shape, how it is type checked, the target language term it
   expands to, and its type.
3. The next part is a [syntax-pattern][18] describing the the shape of
   the syntax this rule applies to. In this case, we're defining `λ` as a macro
   that expects a parenthesized sequence of identifier-colon-type triples,
   describing the formal arguments to the procedure, followed by the body `e`. The
   `type` [syntax class][19] is provided by Turnstile, and describes the surface
   syntax of types (such as those created with `define-type-constructor`);
   internal operations over types use the expanded version of the type, which is
   accessed via the `norm` attribute.
4. The chevron `≫` on the first line signifies that there is only one case in
   this type rule. Some rules, which we will see later, use multiple
   cases to check different kinds of uses.
5. The body of the rule is a sequence of premises, that usually check and
   analyze the types of sub-expressions, followed by a dashed line, and
   then the conclusion, describing the output syntax and its type.
6. Here, the single premise describes how to check the body of the function. The
   context, which associates variables with types, goes to the left of the
   turnstile (`⊢`). For each formal `x`, this lets us know what type `x` has
   when we find a reference to it in `e`. In this rule, we are saying "while
   checking the right-hand-side, assume `x`---which elaborates to
   `x-`---has type `τ_in`, for each triple in the input syntax (signified by the
   ellipses `...`)". More on the "elaborates to `x-`" below.
7. To the right of the turnstile, we write the expression we are checking, `e`,
   and patterns `e-` and `τ_out` matching the elaboration of `e` and its type,
   respectively.
8. After the dashes comes the conclusion, which begins with `⊢`. The next part
   specifies the elaboration of the term. Here, the meaning of the typed `λ` is
   given in terms of Racket's [`#%plain-lambda`][5]. Turnstile uses the
   convention of a `-` suffix for forms in the untyped/target language to avoid
   conflicting names and confusion. Suffixed names are usually bound using
   `postfix-in`, such as in `(require (postfix-in - racket/base))` to bind
   `#%plain-lambda-`.
9. Finally, we give the type of the term to the right of the `⇒`, referring to
   pattern variables bound in the premises.
   
#### Renaming Typed Variables
   
Turnstile lets the Racket expander take care of the details of variable scope,
shadowing, etc. To associate identifier `x` with type `τ`, Turnstile binds `x`
to a macro that knows `τ` when it expands. References to `x` now become
references to that macro, and expanding them provides access to `τ`. Concretely,
the underlying Racket code implementing this behavior looks roughly like this:

```racket
(let ([x- (assign-type (generate-temporary #'x) #'τ)])
  (let-syntax ([x (make-rename-transformer x-)])
    ... expand and check forms that may reference x ...))
```

The type `τ` is attached as [metadata][6] for a new identifier `x-`, which is
what `x` will transform to at any reference site. In order for this to work,
`x-` must be distinct from `x`---hence the `generate-temporary`---to avoid an
infinite expansion loop.

### Application

We can define a version of `#%app` that type checks function applications to 
accompany our typed `λ`:

```racket
(define-typed-syntax (#%app e_fn e_arg ...) ≫
  [⊢ e_fn ≫ e_fn- ⇒ (~→ τ_in ... τ_out)]
  #:fail-unless (stx-length=? #'[τ_in ...] #'[e_arg ...])
                (num-args-fail-msg #'e_fn #'[τ_in ...] #'[e_arg ...])
  [⊢ e_arg ≫ e_arg- ⇐ τ_in] ...
  --------
  [⊢ (#%plain-app- e_fn- e_arg- ...) ⇒ τ_out])
```

1. The syntax pattern on the first line describes the shape of applications.
2. On the second line, we pattern match the result of expanding and checking
   `e_fn`, checking that it produces an arrow type. More specifically, when we defined the arrow
   type `→` above, `define-type-constructor` also implicitly defined a [pattern
   expander][15] `~→` (which uses the Racket `~` prefix convention for syntax patterns)
   that matches instances of the type.
3. The next clause checks that the number of provided arguments matches the
   arity of the function as specified by its type.
4. Line 5 checks that each argument expression has the required type. Turnstile
   uses [bidirectional typechecking rules][17], which either infer the type of a
   term or checks that a term satisfies a given type. We write `⇐ τ_in` in the
   premise to switch to checking mode.
5. Finally, typed function application elaborates to Racket's function application,
  `#%plain-app`, with the usual suffix, and produces type `τ_out` for the application
   
We can try out these new typed forms on a few examples:

* `((λ ([x : Int]) (+ x 1)) 2)` successfully typechecks and yields `3`.
* `((λ ([x : Int]) (+ x 1)))` raises an error based on the check on lines 3 and
  4 in the rule: "#%app: (λ ((x : Int)) (+ x 1)): wrong number of arguments: expected
  1, given 0."
* `((λ ([x : (→ Int Int)]) (x 1)) 2)` raises an error: "#%app: type mismatch:
  expected (→ Int Int), given Int" as a consequence of using checking mode on
  line 5 of the rule.

## Extending Our Language with Local Bindings

When writing functional programs, we often want to name various
sub-computations. One way to do that is with a `let` construct, which Turnstile
allows us to easily create:

```racket
(define-typed-syntax (let ([x:id e-x] ...) e-body) ≫
  [⊢ e-x ≫ e-x- ⇒ τ-x] ...
  [[x ≫ x- : τ-x] ... ⊢ e-body ≫ e-body- ⇒ τ-body]
  -------------------------------------
  [⊢ (let- ([x- e-x-] ...) e-body-) ⇒ τ-body])
```

Unsurprisingly, this looks very similar to the definition of `λ` above. Now we
can write functions with named intermediate results:

```racket
(λ ([x : Int])
  (let ([almost-there (+ x 1)])
    (+ almost-there 1)))
```

However, in Racket it's common to name such intermediate results using `define`
rather than `let`. In fact, it's [prescribed by the style guide][8]. Naturally,
we would like to do so in our Racket language extension as well, which would
allow us to write the above function as:

```racket
(λ ([x : Int])
  (define almost-there (+ x 1))
  (+ almost-there 1))
```

Unfortunately, this is not nearly as easy to do in Turnstile as `let`.

## Sequences

At first glance, the issue seems to be that the definition of `λ` above limits
the body to be a single expression when what we want to put there is a sequence
of definitions and expressions. To reach our goal, we need to change the
definition of `λ` to allow its body to be a sequence.

The first step is to create a typed form for sequences of definitions and
expressions, which can then be used by rules like `λ`:

```racket
(define-typed-syntax (begin e ...+) ≫
  [⊢ e ≫ e- ⇒ τ] ...
  #:with τ-final (stx-last #'(τ ...))
  -----------------------------------
  [⊢ (begin- e- ...) ⇒ τ-final])
```

This directs type checking to:

1. Check each `e` in the sequence individually, obtaining an expanded `e-` and
   inferred type `τ` for each.
2. Take the last type in the sequence and call it `τ-final`; Turnstile allows
   using `syntax-parse` [directives][23] such as `#:with` as premises.
3. Expand to Racket's `begin` (with the usual `-` suffix) and give the whole
   expression the type of the last term in the body.
   
Now, we can use `begin` in a revised definition of `λ`. The new rule takes a
non-empty sequence of forms in the body and wraps them in our new `begin` form
for typechecking.

```racket
(define-typed-syntax (λ ([x:id (~datum :) τ_in:type] ...) e ...+) ≫
  [[x ≫ x- : τ_in.norm] ... ⊢ (begin e ...) ≫ e- ⇒ τ_out]
  -------------------------------------------------
  [⊢ (#%plain-lambda- (x- ...) e-) ⇒ (→ τ_in.norm ... τ_out)])
```

Now we need a way to include definitions in these sequences and we're set!

## The Difficulty With Define

If we think about how type information is communicated between a binder and its
reference we can see why `define` is a different beast than `let`

```
(let ([x 5]) (+ x 1))
       ^        ^
       |        |- TO HERE
FROM HERE
```

When the rule for our `let` is invoked, it has access to both the binding sites
and the place where references may occur. The situation lends itself to a
straightforward implementation strategy: create an environment of
identifier/type associations to use when analyzing the body. Turnstile directly
accommodates this scenario in its language for creating type rules with the
optional context appearing on the left of the `⊢`, as in our rules for `λ` and
`let` above.

Define is different.

```
(define x 5)
        ^
        |------ TO WHERE?
FROM HERE
```

The problem is apparent: we can't see where the reference to `x` occurs! The
information about the binding needs to escape from the `define` to the
surrounding context. In other words, when we implement `define`, we don't have a
body term available that contains all the possible references. Instead, we
will have to find a way of communicating the existence of the `x` binding and
its type to the surrounding context.

Above, in the subsection on "Renaming Typed Variables", we saw that the context
in Turnstile type rules is implemented as syntax transformers with `let`-like
scope (created with `let-syntax`). One idea would be to mimic this approach, but
instead of using `let-syntax` to achieve `let`-like scope, use `define-syntax`
to achieve `define`-like scope.

Fortunately for us, someone has already tried their hand at writing a `define`
form for Turnstile languages using a `define-syntax` rename, found in the
[Turnstile examples][9]. We can take that as our starting point:

```racket
(define-base-type Void)
(define- a-deep-dark-void (#%app- void-))
(define-typed-syntax (define x:id e) ≫
  [⊢ e ≫ e- ⇒ τ]
  #:with x- (assign-type (generate-temporary #'x) #'τ #:wrap? #f)
  -----------------------------------------------------
  [⊢ (begin-
       (define-syntax x (make-variable-like-transformer #'x-))
       (define- x- e-)
       a-deep-dark-void)
     ⇒ Void])
```

Let's break it down.

1. Create a new type, `Void`, to assign definitions.
2. Create a constant to serve as the canonical value of type `Void`.
3. Define a new typed form, `define`, used as in `(define x e)`.
4. Check the type of the expression `e`, getting its
   expansion `e-` and type `τ`.
5. Create a new name, `x-`, and attach the type `τ` as metadata.
6. Expand to Racket's `begin`. Unlike `let`, `begin` does not create a new
   scope; definitions inside a `begin` are also visible in the surrounding
   context. That behavior is needed for scenarios like this one that expand to
   multiple definitions.
7. Create a macro binding for `x` that rewrites to `x-`. By using a define-like
   form, the macro has the same scoping rules as `define`, so it will apply to
   references to `x` in the surrounding context---exactly what we want. (We are
   using `make-variable-like-transformer` to avoid the special treatment the
   expander gives to `rename-transformer`s. The specifics are beyond the scope
   of this post.)
8. Define `x-` to refer to the supplied expression. Note that here `define-` is
   Racket's `define`.
9. Keep the result of evaluating this form in line with the type by yielding a
   value of type `Void`.
    
This implementation of `define` gets us pretty far. If we put definitions at the
top-level of a module in our language, we can reference them within other terms
in the module:

```racket
;; module top level
(define x 5)
(+ x 1)
;;=> 6
```
Unfortunately, we encounter a problem if we try to create *local* definitions:

```racket
(define add2
  (λ ([x : Int])
     (define almost (+ x 1))
     (+ almost 1)))
;;==> almost: unbound identifier...
```

Pointing to the reference on the final line. The problem is that our `define`
and `begin` forms are not interacting in the way we might have hoped.

When we expand the body of the function above, we associate `x` with type `Int`
then start checking the body, wrapped in a `begin`:

```racket
(begin
  (define almost (+ x 1))
  (+ almost 1))
```

Consulting the definition of `begin`, we see that it checks/expands each
sub-expression in seqence. First in the sequence is a use of `define`, yielding

```racket
(begin-
  (define-syntax almost ...)
  (define- almost- ...)
  a-deep-dark-void)
```

Crucially, the expansion of our `define` form **stops** at this point, without
examining the `begin-` form and its contained definitions. The interface through
which Turnstile invokes the macro expander, [`local-expand`][16], takes a
parameter referred to as the *stop list* for stopping expansion at certain
points. The stop list contains identifiers which, when encountered by the
expander, halt expansion.

The syntax output from typed forms created using Turnstile are wrapped with a
particular macro, named `erased`, that serves (only) to orchestrate stopping
expansion. So, the output of our `define` form actually looks like

```racket
(erased
  (begin-
    (define-syntax almost ...)
    (define- almost- ...)
    a-deep-dark-void))
```

And since Turnstile includes `erased` in the stop list for `local-expand`,
expansion stops before analyzing the rest of the output. The point of all this
`erased` business, if you are wondering, is to improve the
performance of Turnstile languages by avoiding unnecessary re-expansions.

Control returns to the `begin` transformer, which turns to checking/expanding
the subsequent `(+ almost 1)`, where it will encounter the identifier `almost`
without a corresponding binding. Even though our `define` form produced a
binding as part of its output, the expander hasn't actually analyzed it before
reaching the reference in the next expression.

The problem is a symptom of analyzing the sequence of forms using an ellipses,
which corresponds to mapping the typechecking/expanding process over each
individually. The mapping operation stipulates that checking each item is
independent of checking the others. But when we add `define` to the language
that is no longer the case. A definition form influences how we typecheck its
neighbors by introducing a new name and its type. This information must be
communicated to the following forms in order to properly check references. That
is, instead of setting up binding information and then checking, analyzing
bindings must be interleaved with type checking. Unfortunately, Turnstile
doesn't provide a fold-like mechanism for threading binding information through
the checking of a sequence of typed forms. We're going to need to implement our
own solution, requiring us to dive underneath the abstractions provided by
Turnstile and get intimate with Racket's syntax model.

## Internal Definition Contexts

In order for the `(+ almost 1)` expression from above to successfully
typecheck/expand, we need to be able to associate `almost` with a suitable type.
Turnstile provides a way to set up such an association, but as we saw before,
Turnstile's interface doesn't suit this scenario.

Racket has the notion of an [internal definition context][20] that allows
definitions to be mixed with expressions. The syntax system exposes tools for
creating and manipulating such contexts programmatically, allowing macro writers
a great deal of power for manipulating the bindings in a program.

When using `local-expand`, we can optionally pass in a definition context
containing binding information. If we create a definition context for the body
of the function and extend it with each definition, then `local-expand`-ing
references such as the above one should work out. Normally, Turnstile calls
`local-expand` internally in accordance with the type rules we write down, but
in order to use our own definition context we're going to have to call it
ourselves.

We can create a definition context with
[`syntax-local-make-definition-context`][10], as in

```racket
(define def-ctx (syntax-local-make-definition-context))
```

And then (imperatively) add bindings to `def-ctx` with
[`syntax-local-bind-syntaxes`][11]. The first argument is a list of identifiers
to bind; we will only be binding one identifier at a time, consequently only
passing singleton lists. The second argument dictates what the given identifier
*means*. Passing `#f` corresponds to a run-time/phase 0 binding, such as that of
a procedure argument, `let`, or `define`; alternatively, we can provide syntax
that evaluates to a function, establishing a transformer binding invoked on
references to the identifier. Using both alternatives, we can define a renaming
macro and give a meaning to the new name:

```racket
(define-for-syntax (int-def-ctx-bind-type-rename! x x- t ctx)
  (syntax-local-bind-syntaxes (list x)
                              #`(make-variable-like-transformer
                                 (assign-type #'#,x- #'#,t #:wrap? #f))
                              ctx)
  (syntax-local-bind-syntaxes (list x-) #f ctx))
```

The first call binds `x` to a transformer that renames to `x-`; the second lets
the expander know that we are taking care of making sure that `x-` will actually
be bound to something.

Our `define` form must communicate the information needed to call
`int-def-ctx-bind-type-rename!` back out to the surrounding context. One way to
do this is to add an intermediate step to the expansion of `define` that
includes the necessary information as part of its syntax. Then, the surrounding
context can analyze the expansion of each term, looking for that form.

Concretely, `define` will expand to `define/intermediate`, which will in turn
expand to what `define` originally expanded to:

```racket
(define-typed-syntax (define x:id e) ≫
  [⊢ e ≫ e- ⇒ τ]
  #:with x- (generate-temporary #'x)
  #:with x+ (syntax-local-identifier-as-binding #'x)
  --------------------------------------------------
  [⊢ (define/intermediate x+ x- τ e-) ⇒ Void])

(define-syntax (define/intermediate stx)
  (syntax-parse stx
    [(_ x:id x-:id τ e)
     #:with x-/τ (assign-type #'x- #'τ #:wrap? #f)
     #'(begin-
         (define-syntax x (make-variable-like-transformer #'x-/τ))
         (define- x- e)
         a-deep-dark-void)]))
```

(The reason we create an `x+` using [`syntax-local-identifier-as-binding`][13] is
[due to a bug in the expander][12]. The explanation is rather involved and
frankly I only barely understand what's going on myself (if at all), so let's
just leave it at that and move on.)

Then, for each form `e` in a sequence, we can call `local-expand` with `def-ctx`
and then check the expansion, `e-`, for `define/intermediate`. In those cases,
we can use `int-def-ctx-bind-type-rename!` to add it to the context. The
procedure `add-bindings-to-ctx!` performs this check on an expanded form `e-`
(remembering that Turnstile will wrap the output of `define` in an `erased`
macro):

```racket
(define-for-syntax (add-bindings-to-ctx! e- def-ctx)
  (syntax-parse e-
        #:literals (erased)
        [(erased (define/intermediate x:id x-:id τ e-))
         (int-def-ctx-bind-type-rename! #'x #'x- #'τ def-ctx)]
        [_
         ;; when e expands to something other than a definition there's nothing to bind
         (void)]))
```

We now have the key ingredients to define a procedure, `walk/bind`, that will
serve as the primary vehicle to type check a sequence of forms,
threading binding information through using a definition context. Processing
sequences of defintions and expressions will iterate through them one at a time,
and for each form `e`:

1. `local-expand` using our internal definition context, resulting in an `e-`.
2. Retrieve the type of `e` from the metadata of `e-` using Turnstile's
   [`typeof`][22] helper.
3. Check if `e` defined a binding, in which case add it to the context.

Aggregating the expanded syntax and type of each form as we go along, we get

```racket
(define-for-syntax (walk/bind e...)
  (define def-ctx (syntax-local-make-definition-context))
  (define unique (gensym 'walk/bind))
  (define-values (rev-e-... rev-τ...)
    (for/fold ([rev-e-... '()]
               [rev-τ... '()])
              ([e (in-syntax e...)])
      (define e- (local-expand e (list unique) (list #'erased) def-ctx))
      (define τ (typeof e-))
      (add-bindings-to-ctx! e- def-ctx)
      (values (cons e- rev-e-...)
              (cons τ rev-τ...))))
  (values (reverse rev-e-...)
          (reverse rev-τ...)))
```

The value `unique` and its use as an argument is dictated by the documentation
of [`local-expand`][16]: "For a particular internal-definition context, generate a
unique value and put it into a list for context-v." By using `#'erased` in the
stop list for `local-expand`, we stop expansion at the same points that Turnstile
does.

Now we can implement `begin` in terms of `walk/bind`:

```racket
(define-typed-syntax (begin e ...+) ≫
  #:do [(define-values (e-... τ...) (walk/bind #'(e ...)))]
  #:with τ-final (last τ...)
  --------------------
  [⊢ (begin- #,@e-...) ⇒ τ-final])
```

and voilà!

```racket
(define (add2 [x : Int])
  (define almost (+ x 1))
  (+ almost 1))

(add2 3)
;;=> 5
```

# But Wait, There's More

I believe this design is can be dropped in 'as-is' and with a few extensions be
useful for a wide variety of Turnstile languages. However, there are a few
shortcomings (that I am aware of) that I will leave as exercises for the
interested reader:

- The `define` form here doesn't provide the useful shorthand for creating
  functions, `(define (f x) e ...)`. Extending it to do so is relatively
  straightforward.
- Supporting *recursive* (and mutually recursive) function definitions is a bit
  more complicated, but shouldn't require many changes to the above code.
- There's an extensibility issue---macros that expand to multiple uses of
  `define` inside a `begin` won't work (why not?), such as

```racket
(define-syntax (define/memo stx)
  (syntax-parse stx
    [(_ (f [x (~datum :) τ] ...) e ...+)
     #'(begin
         (define memo ... memo table ...)
         (define (f [x : τ] ...)
           ... check memo table ...
           e ...))]))
```

Finally, there's some question as to how to lift these ideas to an abstraction
at the Turnstile level, so that future language authors don't have to muck
around with `syntax-local-bind-syntaxes` and friends. If you have any ideas on
this front, feel free to reach out.

<!-- References -->

[1]: http://docs.racket-lang.org/turnstile/The_Turnstile_Guide.html
[2]: http://racket-lang.org/
[3]: http://www.ccs.neu.edu/home/stchang/pubs/ckg-popl2017.pdf
[4]: http://docs.racket-lang.org/syntax/stxparse.html
[5]: http://docs.racket-lang.org/reference/lambda.html?q=%23%25plain-lambda#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._~23~25plain-lambda%29%29
[6]: http://docs.racket-lang.org/reference/stxprops.html
[7]: https://groups.google.com/forum/#!topic/racket-users/LE66fKtcJMs
[8]: https://docs.racket-lang.org/style/Choosing_the_Right_Construct.html#%28part._.Definitions%29
[9]: https://github.com/stchang/macrotypes/blob/c5b663f7e663c564cb2baf0e0a352d5fde4d2bd7/turnstile/examples/ext-stlc.rkt#L55
[10]: http://docs.racket-lang.org/reference/stxtrans.html#%28def._%28%28quote._~23~25kernel%29._syntax-local-make-definition-context%29%29
[11]: http://docs.racket-lang.org/reference/stxtrans.html#%28def._%28%28quote._~23~25kernel%29._syntax-local-bind-syntaxes%29%29
[12]: https://github.com/racket/racket/pull/2237
[13]: http://docs.racket-lang.org/reference/stxtrans.html#%28def._%28%28quote._~23~25kernel%29._syntax-local-identifier-as-binding%29%29
[14]: https://gist.github.com/howell/e2d4501e24db503e4cd9aa368172a502
[15]: http://docs.racket-lang.org/syntax/stxparse-patterns.html?q=pattern%20expander#%28part._.Pattern_.Expanders%29
[16]: http://docs.racket-lang.org/reference/stxtrans.html?q=local-expand#%28def._%28%28quote._~23~25kernel%29._local-expand%29%29
[17]: http://davidchristiansen.dk/tutorials/bidirectional.pdf
[18]: http://docs.racket-lang.org/syntax/stxparse-patterns.html
[19]: http://docs.racket-lang.org/syntax/stxparse-specifying.html
[20]: http://docs.racket-lang.org/reference/syntax-model.html?#%28tech._internal._definition._context%29
[21]: https://scholarship.rice.edu/handle/1911/17993
[22]: http://docs.racket-lang.org/turnstile/The_Turnstile_Reference.html?q=typeof#%28def._%28%28lib._turnstile%2Fmain..rkt%29._typeof%29%29
[23]: http://docs.racket-lang.org/syntax/stxparse-specifying.html?#%28tech._pattern._directive%29
