    Title: Introducing Visual and Interactive-Syntax realized (VISr) for ClojureScript (and JavaScript)
    Date: 2022-01-06T17:56:08
    Authors: Leif Andersen
    Tags: visr, clojure, clojurescript, interactive syntax
    
<style>
.caption {
  display: none;
}
</style>

Visual and interactive-syntax is a type of language-oriented programming that
allows developers to use, view, and edit portions of a textual program with
graphics. Using interactive-syntax provides the benefits of a graphical
programming language, while keeping all of the benefits of a purely textual
language. For example, the following is an example of a small network embedded
in a program:

![Graphical network embedded in text](/img/intro-visr/visr-and-text.png)

Interactive-syntax is backed by human readable code; the visual components
exists purely when writing and editing code. This backing means all of the tools
involved in software development work with interactive-syntax extensions. For
example:

* version control, such as git, works with interactive-syntax;
* programs using interactive-syntax can be written and edited with your favorite
  text editor or IDE;
* cut/copy/paste works with interactive-syntax using your operating system's
  native clipboard;
* code analysis tools, like diff and refactor, still work with
  interactive-syntax; and
* you can use interactive-syntax in any language or environment that supports
  language-oriented programming.
  
To learn more about interactive-syntax, watch [this video][is-video] or read
[the accompanying paper][is-paper].

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/8htgAxJuK5c" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

[VISr (Visual and Interactive-Syntax realized) for ClojureScript][visr] is a
practical implementation of interactive-syntax in web browsers. The VISr
environment is a full-featured IDE that supports interactive-syntax components
called VISrs. Additionally, the VISr environment comes with a package manager
that supports [NPM packages][npm].

This article is a brief introduction to both the VISr environment and the
components that make up a VISrs. It discusses how to insert a VISr into code,
how to manipulate a VISr, and how to create a new types of VISr. Future articles
will discuss more advanced uses such as integrating NPM packages and using VISrs
in other languages.

<!-- more -->

# Getting started with VISr
    
Start by going to [visr.pl][visr], which is a web-based IDE that directly
supports VISrs. Once in the IDE, press `Insert VISr` to place a VISr at the
current cursor position. This VISr contains two buttons: 

- clicking the first displays the VISr's visual representation, and
- clicking the second shows its textual representation. 

![VISr](/img/intro-visr/visr.png)

Opening the code shows that the new VISr is an instance of
`visr.core/empty-visr`, a default VISr provided by the IDE. This VISr expects a
map with the key `:message` to display in the visual view. Changing the value
associated with `:message` changes what is displayed, in this case "Endless
Possibility":

![Open Visr](/img/intro-visr/visr-open.png)

Remember that, although this VISr is displayed graphically, it still exists as
human-readable text. One way to see this text is by copying and pasting the
VISr. A copy of the same VISr will appear when it is placed back into the IDE.
However, pasting it into other text editors that do not natively support VISrs
yields the following human readable, and editable, text:

```clojurescript
^{:editor visr.core/empty-visr}(visr.core/empty-visr+elaborate 
                                 {:message "Endless Possibility"})
```

This operation works in reverse too. Writing out similar text and pasting it
into [visr.pl][visr] yields its visual representation.

# Making a new VISr

The `defvisr` form creates a VISr type. This form expects two methods:

1. a `render` method that provides visualization and interaction when code is
   edited, and 
2. an `elaborate`/`elaborate-fn` method that gives the VISr compile-time and
   run-time semantics.

The following is the signature for a simple VISr type:



```clojurescript
(ns example.core)

(defvisr Counter
  (elaborate-fn [this] "TODO-elaborate")
  (render [this] "TODO-render"))
```

This example uses `elaborate-fn`, a simplified version of `elaborate` that gives
the `VISr` the same semantics as a function application. It also allows the
`defvisr` form to work in the same file as the VISr itself. 

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

This VISr doesn't match the theme of the page; it also requires the state to be
a single number. Using [React Bootstrap][react-bootstrap] and Reagent cursors
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
the VISr semantics takes the form of a function application:

```clojurescript
(elaborate-fn [{:keys [count]}] count)
```

This `elaborate` method expects a dictionary with the key `:count` and returns
the value associated with that key. It makes use of [ClojureScript's
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
    (let [count (cursor this [:count])]
      (when-not @count (reset! count 0))
      [:> Button {:on-click #(swap! count inc)} @count])))
```


Here is the VISr in action:

![Full Count Example](/img/intro-visr/full-count.png)

That's all there is to it. From here, you can go to [visr.pl][visr] to make your
own programs using VISr. You can also [take this survey][survey], which contains
more advanced example uses for VISr. If you find any bugs or want to contribute,
you can also head to [the visr project page][github].

Thanks for reading, happy coding!


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
