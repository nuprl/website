# nuprl.github.io

Overview
========

This is the **source code** for the website for the
Northeastern University Programming Research Lab,
hosted at prl.ccs.neu.edu.

Any changes made to the `master` branch here will get deployed
automatically to `prl.ccs.neu.edu`.

Building
========

Build Prerequisites: [Racket](http://racket-lang.org/)

To build the site + blog and open a new window:

```
  make preview
```

To build the site, but not the blog:

```
make prl
```

To build one page, for example `index.html`:

```
  make index.html
```


Blog
====

See `blog/README.md`
