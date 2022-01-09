    Title: Introducing Visual and Interactive-Syntax realized (VISr) for ClojureScript
    Date: 2022-01-06T17:56:08
    Authors: Leif Andersen
    Tags: visr, clojure, clojurescript, interactive syntax

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

[VISr (Visual and Interactive-Syntax realized) for ClojureScript][visr] is a
practical implementation of interactive-syntax in web browsers. In addition to
ClojureScript, the VISr environment works with any [NPM package][npm]. This
article is a brief introduction to VISr. It will discuss how to insert a VISr
into code and create a new type of VISr. Future articles will discuss more
advanced uses such as integrating NPM packages and using VISr to other
languages.

<!-- more -->

# Inserting a VISr

The ![Insert VISr button]{/img/intro-visr/insert.png} button

![VISr]{/img/intro-visr/visr.png}


```
^{:editor visr.core/empty-visr}(visr.core/empty-visr+elaborate 
                                 {:message "Endless Possibility"})
```

# Making a new VISr


```
(ns example.core)
```

```
(ns example.use
  (:require [example.core :include-macros true]))
```

```
(defvisr Counter
  (elaborate [this] "TODO")
  (render [this] "TODO"))
```

# The Render Method for Edit-Time Semantics

```
(render [this]
  [:button {:on-click 
```

## A Nicer Render 

```
(ns test.core
  (:require [reagent.core :refer [cursor]]
            [react-bootstrap :refer [Button]]))
```


```
(render [this]
  (let [count (cursor this :count)]
    [:> Button {:on-click (swap! count inc)} @count]))
```

# Elaboration and Run-Time Semantics

```
(elaborate [this] (get this :count))
```

```
(elaborate [{:keys [count]}] count)
```

# Putting it all together

```
(ns test.core
  (:require [react-bootstrap :refer [Button]]))

(defvisr Counter
  (elaborate [{:keys [count]}] count)
  (render [this]
    (let [count (cursor this :count)]
      [:> Button {:on-click (swap! count inc)} @count])))
```

[visr]: https://visr.pl
[npm]: https://www.npmjs.com/
