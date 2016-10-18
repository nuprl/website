    Title: Emacs daemon for fast editor startup
    Date: 2016-10-17T21:48:25
    Tags: System Administration, Emacs, by Gabriel Scherer

In the early days of the famous Emacs/Vim debates, Emacs was often
ridiculed for its bulkiness (Eight Megabytes-of-RAM And Constantly
Swapping, etc.). The computational power of our computer has grown
much faster than Emacs' bloat: it takes exactly one second to load on
my machine. However, our workflows have also changed, and my workflow
implies frequently starting new text editors -- on each git commit for
example, or when I use a [Firefox
extension](https://addons.mozilla.org/en-US/firefox/addon/its-all-text/)
to edit a textarea content in a proper editor.

In this blog post, I describe how to use `emacsclient` to reuse an
existing Emacs process when creating a new editor window, which
reduces editor startup times from 1s to 0.150s on my machine.

<!-- more -->

Emacs has long supported a client/server mode: a daemon emacs instance
is loaded in the background, and whenever you request a new emacs
window you can creates a new frame (~ window) using the same
instance. This means that the startup time is dramatically reduced
after the daemon is launched, as for example the execution of your
personal configuration code does not have to be repeated.

To use it, I have this code as `/usr/bin/editor`:

```sh
#!/bin/bash
emacsclient -a "" -c "$@"
```

The empty `-a` parameter means that if no daemon exists, it should start one in the background and retry.
The `-c` option means that a new frame (window) should be created
instead of reusing an existing one. `"$@"`means that when the script
is invoked with a path as command-line parameter (`editor /tmp/foo.txt`),
the corresponding file will be opened.

Finally, my `.bash_profile` sets the `EDITOR` variable to
`editor` (`export EDITOR=/usr/bin/editor`); this environment
variable is what most tools (git included) will use to invoke
a text editor.

On my machine, starting the daemon takes 1.4s. Creating a client windows takes around 0.150s.

If you want to control the environment in which the daemon process is
started, you can launch it explicitly by running `emacs --daemon`.

Cool kids use [Spacemacs](http://spacemacs.org/) these days, which comes
with all sort of convenient settings built in, and I'm told that it
does daemonization out of the box. I haven't taken the time to give
Spacemacs a try yet.

Finally, sometimes having all editor windows share the same process is
not the right thing, because I do stuff which makes Emacs a bit
unstable. (It's not very good at asynchronous communication with the
rest of the world, so for example accessing a file through SSH from
Emacs can hang the process when network goes bad.). I've been bitten
a few times by a crash of all editor windows at the same time, and
since then, when I know I'm going to do "heavy stuff", I launch
a separate process for it (just `emacs` instead of `editor` or
`emacsclient`).

P.S.: To precisely measure the startup time, ask Emacs to evaluate
a Lisp expression on startup, to kill it immediately.:

```sh
$ time emacs --eval "(save-buffers-kill-terminal)"
$ time emacsclient -a '' -c -e "(save-buffers-kill-terminal)"
```