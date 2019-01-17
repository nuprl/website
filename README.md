# nuprl.github.io

Overview
========

This is the **source code** for the website for the
Northeastern University Programming Research Lab,
hosted at prl.ccs.neu.edu.

Any changes made to the `master` branch here will get deployed
automatically to `prl.ccs.neu.edu`.

Note that we configure a [cron job](https://docs.travis-ci.com/user/cron-jobs/)
to rebuild so that the list of seminars shows up in the right order (as it
needs to change after every seminar).


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
