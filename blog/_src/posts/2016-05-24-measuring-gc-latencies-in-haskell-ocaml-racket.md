    Title: Measuring GC latencies in Haskell, OCaml, Racket
    Date: 2016-05-24T10:51:34
    Tags: garbage collection,latency,instrumentation,haskell,ghc,ocaml,racket,by Gabriel Scherer

James Fisher has a blog post on a case where GHC's runtime system
imposed unpleasant latencies on their Haskell program:

> [Low latency, large working set, and GHC's garbage collector: pick two of three][ghc-latency-post]

[ghc-latency-post]: https://blog.pusher.com/latency-working-set-ghc-gc-pick-two/

The blog post proposes a very simple, synthetic benchmark that
exhibits the issue -- basically, latencies incurred by copy time --
with latencies of 50ms that are considered excessive. I thought it
would be amusing to reproduce the synthetic benchmark in OCaml and
Racket, to see how other GCs handle this.

Without further ado, the main take-away are as follows: the OCaml GC
has no issue with large objects in its old generation, as it uses
a mark&sweep instead of copying collection, and exhibits less than 3ms
worst-case pauses on this benchmark.

The Racket GC also does not copy the old generation, but its
incremental GC is still in infancy (compared to the
throughput-oriented settings which works well) so the results are less
good. It currently suffer from a "ramp-up" effect that I will
describe, that causes large pauses at the beginning of the benchmark
(up to 120ms latency), but in its steady state the longest pause are
around 22ms.

Please keep in mind that the original benchmark is designed to
exercise a very specific workflow that exercises worst-case behavior
for GHC's garbage collector. This does not mean that GHC's latencies
are bad in general, or that the other tested languages have smaller
latencies in general.

The implementations I use, with a Makefile encapsulating the logic for
running and analyzing them, are available in a Gitlab repository:

- git: <https://gitlab.com/gasche/gc-latency-experiment.git>
- files: <https://gitlab.com/gasche/gc-latency-experiment/tree/master>

<!-- more -->

## The Haskell benchmark

James Fisher's Haskell benchmark is very simple: it creates an
association table in which medium-size strings are inserted
repeatedly -- a million times. When the channel reaches 200_000
messages, a string is deleted each time a string is created, to keep
the total working size constant.

```haskell
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map

data Msg = Msg !Int !ByteString.ByteString

type Chan = Map.Map Int ByteString.ByteString

message :: Int -> Msg
message n = Msg n (ByteString.replicate 1024 (fromIntegral n))

pushMsg :: Chan -> Msg -> IO Chan
pushMsg chan (Msg msgId msgContent) =
  Exception.evaluate $
    let inserted = Map.insert msgId msgContent chan in
      if 200000 < Map.size inserted
      then Map.deleteMin inserted
      else inserted

main :: IO ()
main = Monad.foldM_ pushMsg Map.empty (map message [1..1000000])
```

To compile and run the program (`make run-haskell` also works in my repository):

```
ghc -O2 -optc-O3 Main.hs  # compile the program
./Main +RTS -s            # run the program (with GC instrumentation enabled)
```

On my machine, running the program takes around 1.5s. We are not
interested in the total running time (the *throughput* of the
algorithm), but in the pause times induced by the GC: the worst pause
time is 51ms (milliseconds), which is the same as the one reported by
the blog post -- and there it is considered excessive, with an
expected worst-case latency of at most "a few milliseconds".

(I did my testing with GHC 7.8, Fischer reports results with 7.10,
they are essentially the same.)

This Haskell code makes two assumption about the `Map` data structure
(immutable associative maps) that make the benchmark more cumbersome
to port to other languages. It assumes that the element count is
pre-cached in the data structure and thus `Map.size` is
constant-time -- for both OCaml and Racket it is linear. It also uses
a key ordering that makes it easy to remove the smallest key -- OCaml
does this as well, but Racket uses hashes instead.

I initially worked around this by storing count and minimum-key
information in the ported versions, but in fact it's much nicer to
write a variant of the benchmark, with the same behavior, that does
not require these specific features:

```haskell
type Msg = ByteString.ByteString
type Chan = Map.Map Int Msg

windowSize = 200000
msgCount = 1000000

message :: Int -> Msg
message n = ByteString.replicate 1024 (fromIntegral n)

pushMsg :: Chan -> Int -> IO Chan
pushMsg chan highId =
  Exception.evaluate $
    let lowId = highId - windowSize in
    let inserted = Map.insert highId (message highId) chan in
    if lowId < 0 then inserted
    else Map.delete lowId inserted

main :: IO ()
main = Monad.foldM_ pushMsg Map.empty [0..msgCount]
```

This variant has the same running times and worst-case pause, 50ms, as
the original program.

### Explaining Haskell results

James Fischer explains that the reason why the latencies are this high
(50ms is considered high) is that while GHC's garbage collector is
generational, its older generation still uses a stop-and-copy
scheme. This means that when it contains lots of large objects, a lot
of time is spent copying them.

The [original blog post][ghc-latency-post] contains a more detailed
description of the problem and of various optimizations that may be
attempted. Unfortunately, it seems that it is currently impossible to
optimize that kind of workloads by tuning the code or GC parameters:
the copying behavior of the old heap cannot really be worked-around
currently.

As a meta-comment, one possible explanation for why this design choice
was made might be that a lot of effort was invested in the Haskell's
GC to support concurrent mutators (a multi-core runtime). The
additional complexity imposed by this extremely challenging and useful
requirement may have encouraged runtime authors to keep the general GC
architecture as simple as reasonably possible, which could explain
this choice of using the same collection strategy in all generational
spaces.

## OCaml version

The code can easily be ported into OCaml, for example as follows:

```ocaml
open Batteries
module IMap = Map.Make(Int)

let message n = String.make 1024 (Char.chr (n mod 256))

let window_size = 200_000
let msg_count = 1_000_000

let push_msg chan high_id =
  let low_id = high_id - window_size in
  let inserted = IMap.add high_id (message high_id) chan in
  if low_id < 0 then inserted
  else IMap.remove low_id inserted

let () =
  Seq.init msg_count (fun i -> i)
  |> Seq.fold_left push_msg IMap.empty |> ignore
```

Evaluating throughput is not the point, and the balanced maps used by
the Haskell and OCaml are certainly implemented in slightly different
ways that would explain any performance difference, but I was still
amused to see the total runtime be essentially the same: 1.5s.

To measure the maximal pause time, there are two options:

- use the new instrumented runtime contributed by Damien Doligez in
  OCaml 4.03; this works but, being a relatively new feature with not
  much usability effort put into it, it's far from being as convenient
  as GHC's `+RTS -s` parameter.

- Simply measure the time spend in each iteration (pushing a message),
  and using this as an upper bound on the pause time: clearly any GC
  pause cannot pause for more time than the iteration takes. (With my Makefile,
  `make run-ocaml`)

To use the new instrumented runtime, you need to have an OCaml
compiler, version 4.03.0, compiled with the
`--with-instrumented-runtime` configure-time switch. Then, you can use
the `i`-variant (`i` for "instrumented") of the runtime that is
compiled with instrumentation enabled. (My makefile rule `make
run-ocaml-instrumented` does this for you, but you still need a switch
compiled with the instrumented runtime.)

```
ocamlbuild -tag "runtime_variant(i)" main.native
OCAML_INSTR_LOG=ocaml.log ./main.native
```

The log file `ocaml.log` will then contain a low-level log of all
GC-related runtime calls, with nanosecond time, in a format made for
machine rather than human consumption. The tools `ocaml-instr-report`
and `ocaml-instr-graph` of the OCaml source distribution (not
installed by default, you need a source checkout), will parse them and
display tables or graph. The entry point of interest for worst-case
latency is `dispatch`, which contains the time spent in all GC
activity. The relevant section of `ocaml-instr-report`'s output shows:

```
==== dispatch: 2506
470ns..1.0us:  1     (768ns)                       0.04%
1.0us..2.2us: # 2                                  0.12%
2.2us..4.7us: ### 8                                0.44%
4.7us..10us : #### 10                              0.84%
 10us..22us :  1     (14us)                        0.88%
 22us..47us :                                      0.88%
 47us..100us:                                      0.88%
100us..220us: ## 3                                 1.00%
220us..470us: ########## 668                      27.65%
470us..1.0ms: ########### 1795                    99.28%
1.0ms..2.2ms: ##### 17                            99.96%
2.2ms..4.7ms:  1     (2.7ms)                     100.00%
```

As you can see, most pauses are between 220µs and 1ms, with the
longest pause being 2.7ms.

The other approach to measure latency for this program, which works on
older OCaml versions without an instrumented runtime, is just to
insert explicit timing calls and compute the worst-case time of an
iteration -- as an over-approximation over the max pause time,
assuming that the actual insertion/deletion time is small.

```ocaml
let worst = ref 0.
let time f =
  let before = Unix.gettimeofday () in
  let result = f () in
  let after = Unix.gettimeofday () in
  worst := max !worst (after -. before);
  result

let push_msg chan high_id = time @@ fun () ->
  let low_id = high_id - window_size in
  let inserted = IMap.add high_id (message high_id) chan in
  if low_id < 0 then inserted
  else IMap.remove low_id inserted

(* ..main loop.. *)
let () = Printf.printf "Worst pause: %.2E\n" !worst
```

Running this version reports a worst-case latency of 2ms seconds on my
machine (I use the `%E` formatter for scientific notation, so it gets
printed as `2.03E-03`), which is in line with the instrumented
runtime -- actually slightly lower, as the instrumentation may add
some overhead.

A downside of this poor man worst-latency computation approach is that
we only get the worst time, not any kind of timing distribution.

### Explaining OCaml results

The OCaml GC has had reliable incremental phases implemented by
default for a long time, and does not use a copying strategy for its
old generation. It is mark&sweep, executed well, so it was
predictable from the start that this specific benchmark would not be
a worst-case for OCaml.

The latest released OCaml version, OCaml 4.03.0, has seen work by
Damien Doligez to improve the worst-case latency in some situations,
motivated by the industrial use-cases of Jane Street. In particular,
the latency *instrumentation* tools that I'm using above were
developed by Damien on this occasion. I checked with the second
measurement strategy that the latency is just as good on previous
OCaml versions: this particular use-case was not in need of
improvement before 4.03.

## Racket version

Max New wrote a first version of Racket port of this benchmark -- he
had to explicitly keep track of the map count and minimum key to match
the original GHC version. I adapted his code to my simplified variant,
and it looks rather similar to the other implementations.

```scheme
#lang racket/base
(require racket/match)

(define window-size 200000)
(define msg-count  2000000)

(define (message n) (make-bytes 1024 (modulo n 256)))

(define (push-msg chan id-high)
  (define id-low (id-high . - . window-size))
  (define inserted (hash-set chan id-high (message id-high)))
  (if (id-low . < . 0) inserted
      (hash-remove inserted id-low)))

(define _
  (for/fold
     ([chan (make-immutable-hash)])
     ([i (in-range msg-count)])
     (push-msg chan i)))
```

I initially used the poor man approach of explicit timing calls to
measure latency, but then switched to two better methods:

- Sam Tobin-Hochstadt's [gcstats](https://github.com/samth/gcstats)
  package makes Racket programs produce a summary of their runtime
  behavior in the same format as GHC's `+RTS -s` output, with in
  particular the worst-case pause time. It is also very easy to use:

        racket -l gcstats -t main.rkt

- By setting the environment variable `PLTSTDERR=debug@GC`, the racket
  runtime will log GC events on the standard error output. One can
  then grep for minor or major collections, or produce a histogram of
  running times through the following scripting soup I cooked myself:

        cat racket.log | grep -v total | cut -d' ' -f7 | sort -n | uniq --count

Racket has an incremental GC that is currently experimental (it is
not enabled by default as it can degrade throughput) and is enabled by
setting the environment variable `PLT_INCREMENTAL_GC=1`. I compared
with and without the incremental GC, and generally it shifts the
latency histogram towards smaller latencies, but it turns out not to
help so much for the worst-case latency without further tuning, for
a reason I will explain. All results reported below use the
incremental GC.

On my machine, using the latest release Racket 6.5, the maximal pause
time reported by `gcstats` is around 150ms, which is rather bad -- the
excessive pause of GHC was 50ms.

### Investigating the Racket results

I sent [an
email](https://groups.google.com/forum/#!topic/racket-dev/AH6c-HGgzJ0)
to the racket-dev mailing list, hoping to get explanations and advice
on how to improve the code to decrease GC latencies. (Remember that
one problematic aspect of the GHC benchmark is that there is no real
way for users to tweak the code to get better latencies for the same
workflow. So we are evaluating default latencies but also
tweakability.) It worked out quite well.

First, Matthew Flatt immediately sent a few commits on the Racket
codebase to improve some behaviors that were problematic on the
benchmark. Using the development version of Racket instead of 6.5, the
worst-case latency drops from 150ms to 120ms on my machine. All
remaining times are reported using the development version.

Matthew Flatt also analyzed the result and noticed that the worst-case
latency systematically happens at the beginning of the benchmark, just
after the channel reaches its maximal side of 200,000 messages. This
is hard to see with the default benchmark parameters, where the
"ramp-up" period of filling the channel takes one fifth of the total
iterations. To see this clearly, I increased the iteration count from
1,000,000 to 10,000,000, then ran `make
run-racket-instrumented`. I can look at the pause time of major
collections by doing `grep MAJ racket.log`, and on my machine I have:

```
GC: 0:MAJ @ 50,634K(+37,221K)[+1,560K]; free 5,075K(-5,075K) 12ms @ 373
GC: 0:MAJ @ 101,983K(+35,024K)[+1,560K]; free 10,880K(-5,168K) 38ms @ 521
GC: 0:MAJ @ 192,491K(+38,404K)[+1,560K]; free 8,174K(-24,030K) 56ms @ 810
GC: 0:MAJ @ 377,716K(+49,259K)[+1,560K]; free 10,832K(-9,536K) 92ms @ 1571
GC: 0:MAJ @ 742,630K(+59,881K)[+1,560K]; free 140,354K(-156,738K) 138ms @ 3321
GC: 0:MAJ @ 1,214,486K(+112,313K)[+1,560K]; free 361,371K(-377,755K) 60ms @ 6046
GC: 0:MAJ @ 1,417,749K(+138,410K)[+1,560K]; free 600,291K(-600,291K) 23ms @ 8553
GC: 0:MAJ @ 1,400,780K(+155,379K)[+1,560K]; free 564,923K(-564,923K) 21ms @ 11048
GC: 0:MAJ @ 1,408,812K(+147,347K)[+1,560K]; free 583,454K(-583,454K) 21ms @ 13506
GC: 0:MAJ @ 1,404,757K(+151,402K)[+1,560K]; free 572,350K(-572,350K) 20ms @ 15983
GC: 0:MAJ @ 1,407,842K(+148,317K)[+1,560K]; free 579,079K(-579,079K) 22ms @ 18438
GC: 0:MAJ @ 1,405,641K(+150,518K)[+1,560K]; free 575,624K(-575,624K) 21ms @ 20907
GC: 0:MAJ @ 1,405,833K(+150,326K)[+1,560K]; free 577,191K(-577,191K) 21ms @ 23362
GC: 0:MAJ @ 1,405,763K(+150,396K)[+1,560K]; free 575,779K(-575,779K) 20ms @ 25897
GC: 0:MAJ @ 1,406,444K(+149,715K)[+1,560K]; free 577,553K(-577,553K) 20ms @ 28348
GC: 0:MAJ @ 1,406,409K(+149,750K)[+1,560K]; free 576,323K(-576,323K) 21ms @ 30827
GC: 0:MAJ @ 1,407,054K(+149,105K)[+1,560K]; free 577,961K(-577,961K) 21ms @ 33290
GC: 0:MAJ @ 1,404,903K(+151,256K)[+1,560K]; free 576,241K(-576,241K) 20ms @ 35774
GC: 0:MAJ @ 1,406,551K(+149,608K)[+1,560K]; free 575,352K(-575,352K) 22ms @ 38251
GC: 0:MAJ @ 1,405,775K(+150,384K)[+1,560K]; free 577,401K(-577,401K) 21ms @ 40730
GC: 0:MAJ @ 1,406,015K(+150,144K)[+1,560K]; free 575,563K(-575,563K) 20ms @ 43254
GC: 0:MAJ @ 1,406,129K(+150,030K)[+1,560K]; free 577,760K(-577,760K) 21ms @ 45730
GC: 0:MAJ @ 1,406,157K(+150,002K)[+1,560K]; free 575,394K(-575,394K) 22ms @ 48220
GC: 0:MAJ @ 1,406,514K(+149,645K)[+1,560K]; free 577,765K(-577,765K) 21ms @ 50697
```

Look at the evolution of major collection pause times: there is an
early peek at `140ms`, but then pause times decrease and the steady
state has sensibly shorter pauses of around `22ms`. By looking at the
amount of memory freed during each collection, one can see that the
peak corresponds to the first major collection that frees a lot of
memory; it is the first major collection after the channel has reached
its maximal size, and starts removing a lot of messages.

My understanding of this behavior is that the incremental GC keeps
some runtime parameter that observe the memory allocation patterns of
the program, and try to predict when the next collection should be or
how much work it should do. Matthew Flatt explains that this
monitoring logic currently fails to adapt gracefully to the change of
regime in our program, and incurs a large peak pause at this point.

This is good news for our benchmark: sure, there is a very bad pause
at the beginning of the program, but it's a one-time thing. It does
not really affect the last decile of latency that is discussed in
James Fischer's post, and would not be a problem during the steady
state of an actual message-passing application.

### Tuning the Racket version

Matthew Flatt also remarked that by inserting explicit calls to the
GC, one can get collection performed more often than Racket's
heuristics demand and partly avoid the large peak pause. However,
too frequent explicit collections hurt the program throughput.

I experimented a bit and found that the peak pause issue could be
partly mitigated by inserting explicit GC calls around the change of
regime -- around the iteration count that corresponds to the maximal
channel size. I defined a function doing just that

```scheme
(define (maybe-gc i)
  (when (and gc-during-rampup
             (i . > . (window-size . / . 2))
             (i . < . (window-size . * . 2))
             (zero? (modulo i 50)))
        (collect-garbage 'incremental)
        (collect-garbage 'minor)))
```

which is controlled by a `gc-during-rampup` parameter that you can
explicitly set to `#t` to experiment -- explicit GC calls are disabled
by default in my benchmark code. Then I just inserted a `(maybe-gc i)`
call in the main loop.

Because the extra GC calls happen only during rampup, the performance
of the steady state are unchanged and the global cost on throughput is
moderate (20% in my experiment with iteration count 2,000,000). This
seems effective at mitigating the peak pause issue: the worst-case
time on my machine is now only 38ms -- the pauses during the steady
state are unchanged, around 22ms.

This is, of course, a hack; the long-term solution is to wait for
Racket developers to devise better dynamic control strategies to avoid
the ramp-up problem. Apparently, the incremental GC was previously
tested on games that had simpler allocation profiles, such as
short-lived memory allocations during each game tick, with no such
a long ramp-up phase. But I was still interested in the fact that
expert users can tweak the code to noticeably decrease the worst-case
pause time.

To summarize, Racket's incremental GC exhibits
a decent-but-not-excellent steady state behavior, with maximal
latencies of around 22ms, but currently suffers from a GC control
issues that cause much larger pauses during the benchmark ramp-up
period. Explicit GC calls can partly mitigate them.
