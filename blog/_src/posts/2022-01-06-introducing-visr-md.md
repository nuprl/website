    Title: Introducing Visual and Interactive-Syntax realized (VISr) for ClojureScript
    Date: 2022-01-06T17:56:08
    Authors: Leif Andersen
    Tags: visr, clojure, clojurescript, interactive syntax
    
<style>
.caption {
  display: none;
}
</style>

Visual and interactive-syntax is a type of language-oriented programming that
allows developers to use view and edit portions of a textual program with
graphics. This gives you all of the benefits of a graphical programming
language, while keeping all of the benefits of a purely textual language. Here
is an example of a small network embedded in a program, next to the plain text
rendering of the same program:

![Graphical network embedded in text](/img/intro-visr/visr-and-text.png)

Because visual and interactive-syntax only exists when writing and editing
programs, all of the tools involved in software development work with
interactive-syntax extensions. For example:

* version control, such as git, works with interactive-syntax;
* programs using interactive-syntax can be written and edited with your favorite
  text editor or IDE;
* cut/copy/paste works with interactive-syntax using your operating system's
  native clipboard;
* code analysis tools, like diff and refactor, still work with
  interactive-syntax; and
* you can use interactive-syntax in any language or environment that supports
  language-oriented programming.
  
To learn more about interactive-syntax, watch [this video][is-video] or read the
[accompanying paper][is-paper].

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/8htgAxJuK5c" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

[VISr (Visual and Interactive-Syntax realized) for ClojureScript][visr] is a
practical implementation of interactive-syntax in web browsers. In addition to
ClojureScript, the VISr environment works with any [NPM package][npm]. This
article is a brief introduction to VISr. It will discuss how to insert a VISr
into code and create a new type of VISr. Future articles will discuss more
advanced uses such as integrating NPM packages and using VISr to other
languages.

<!-- more -->

# Getting started with VISr
    
    Start by going to [visr.pl][visr], which is a web-based IDE that directly
supports VISrs. Once in the IDE, press `Insert VISr` to place a VISr at the
cursor's current position. The VISr contains two buttons: the first shows the
VISr's visual representation, and the second shows its textual representation.

![VISr](/img/intro-visr/visr.png)

Opening the code shows that the new VISr is an instance of
`visr.core/empty-visr`, a default VISr provided by the IDE. This VISr expects a
message to display in the visual view, in this case "Endless Possibility".

![Open Visr](/img/intro-visr/visr-open.png)

Remember that while this VISr is displayed graphically, it still is text. You
can see this by highlighting the VISr and copying it. When placed back into the
IDE a copy of the same VISr will appear. However, pasting it into other text
editors that don't natively support VISrs yields the following human readable,
and editable, text:

```clojurescript
^{:editor visr.core/empty-visr}(visr.core/empty-visr+elaborate 
                                 {:message "Endless Possibility"})
```

This operation works in reverse too. Writing out similar text and pasting it
into [visr.pl][visr] yields its visual representation.

# Making a new VISr

The `defvisr` form creates a VISr type. This form has two methods: a `render`
method that runs as code is edited, and an `elaborate`/`elaborate-fn` method
that gives the VISr compile-time and run-time semantics. The following is the
signature for a simple VISr:

```clojurescript
(ns example.core)

(defvisr Counter
  (elaborate-fn [this] "TODO-elaborate")
  (render [this] "TODO-render"))
```

This example uses `elaborate-fn`, a simplified version of `elaborate` that gives
the `VISr` the same semantics as a function application. It also allows the
`defvisr` form to work in the same file as the VISr itself. Additionally, VISrs
that use `elaborate-fn` can be defined and used in the same file.

![Example of elaborate-fn semantics](/img/intro-visr/sig.png)


# The Render Method for Edit-Time Semantics

The `render` method is given the VISr state [as an atom][atom]; updating this
atom also updates the code to reflect the new state. The return value for
`render` must be a [Reagent form][reagent] that is the visual view for the VISr.
A render method for a counter VISr might look as follows:

```clojurescript
(render [this]
  [:button {:on-click #(swap! this inc)} @this])
```

And in action:

![Simple Count Example](/img/intro-visr/simpl-count.png)

This VISr doesn't match the theme of the page, and requires its state to be a
single number. Using [React Bootstrap][react-bootstrap] and Reagent cursors
fixes both of these issues:

```clojurescript
(ns example.core
  (:require [reagent.core :refer [cursor]]
            [react-bootstrap :refer [Button]]))
            
(defvisr Counter
  (elaborate-fn [this] "TODO-elaborate")
  (render [this]
    (let [count (cursor this [:count])]
      (when-not @count (reset! count 0))
      [:> Button {:on-click #(swap! count inc)} @count])))
```

# Elaboration and Run-Time Semantics

The elaborate method takes the VISr state, and is expected to provide a
compile-time or run-time semantics. In the simplified case of `elaborate-fn`,
this semantics takes the form of a function:

```clojurescript
(elaborate-fn [{:keys [count]}] count)
```

This `elaborate` method expects a dictionary with the key `:count`, it evaluates
to the value associated with that key. It makes use of [ClojureScript's
Destructuring][destructure] for brevity. The following code is equivalent:

```clojurescript
(elaborate-fn [this] (get this :count))
```

# Putting it all together

The final result is:

```clojurescript
(ns test.core
  (:require [reagent.core :refer [cursor]]
            [react-bootstrap :refer [Button]]))

(defvisr Counter
  (elaborate-fn [{:keys [count]}] count)
  (render [this]
    (let [count (cursor this :count)]
      [:> Button {:on-click (swap! count inc)} @count])))
```


Here is the VISr in action:

![Full Count Example](/img/intro-visr/full-count.png)

That's all there is to it. From here, you can go to [visr.pl][visr] to make your
own programs using VISr. You can also [take this survey][survey], which contains
more advanced example uses for VISr. If you find any bugs or want to contribute,
you can also head to [the visr project page][github].

Thanks for reader, happy coding!


[visr]: https://visr.pl
[npm]: https://www.npmjs.com/
[is-video]: https://www.youtube.com/watch?v=8htgAxJuK5c
[is-paper]: https://dl.acm.org/doi/10.3115/981732.981776
[reagent]: https://reagent-project.github.io/
[atom]: https://clojure.org/reference/atoms
[destructure]: https://clojure.org/guides/destructuring
[react-bootstrap]: https://react-bootstrap.github.io/
[survey]: https://study.visr.pl
[github]: https://github.com/LeifAndersen/interactive-syntax-clojure
