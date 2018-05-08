    Title: Sampling Gradual Typing Performance
    Date: 2018-05-08T15:37:37
    Tags: gradual typing, migratory typing, performance, statistics, by Ben Greenman, by Zeina Migeed

This post explains the sampling method introduced in the paper [_On the Cost of Type-Tag Soundness_](http://www.ccs.neu.edu/home/types/publications/publications.html#gm-pepm-2018)

<!-- more -->

## Quick Reference: How to apply the method

0. Find an untyped program, measure its running time.
1. Define a _granularity_ for type annotations (by-function, by-module, by-program, ....).
2. Define a sample size **s** and number of samples **r**.
3. Randomly select **s** _configurations_ uniformly at random, measure their running time.
4. Repeat the previous step **r** times.
5. Pick a positive real number **D**.
6. Count the proportion of configurations in each sample with running time less-than-or-equal-to **D**
7. Build a 95% confidence interval for the **r** proportions computed in the previous step
8. Conclusion: there is a good chance that your interval contains the true proportion of configurations with running time less-than-or-equal-to **D**


## Background: what to measure

A migratory typing system adds static typing to a dynamically-typed (or, untyped) language.
The recipe for "adding static typing" has a few steps:

- add a syntax for type annotations
- add a static type checker
- add a semantics for statically-typed parts of the program

If the semantics for statically-typed parts of the program is **not** the same
 as the semantics for dynamically-typed parts, then it is important to measure
 performance.

The key question is: how does adding type annotations affect the
 running time of a working program?
We do not know how to answer this question directly.

An easier question, that we can answer, is: for a few programs each with
 one full set of type annotations, how does adding or removing the chosen type
 annotations affect the running time of these programs?

The next two sections give two methods for answering this question.


## Exhaustive Method

One way to answer our easier question is to remove type annotations one
 "unit" at a time and measure the running time of all these partially-typed
 programs.
We call the "unit" the _granularity_ of the performance evaluation.
For example, some choices for granularity are to remove types one module
 at a time, to remove types one function at a time, or to remove types
 one variable at a time.
We call the "partially-typed programs" the _configurations_ of the original
 dynamically-typed program.
Note that the number of configurations depends on the choice of granularity
 --- I can't just use the word _configurations_ without telling you the
 granularity I have in mind.

After measuring the running time of all configurations, we can summarize the
 results.
One way to summarize is to pick a number **D** and count the number of configurations
 that run at most **D** times slower than the original dynamically-typed program.
If this number is large, then the takeaway is:
 if _you_ are willing to accept at most a **D**x slowdown, and you add your
 own type annotations to your own program, then there's some hope that your
 configuration runs at most **D** times slower than your original program.

> Credit for the exhaustive method: [_Is Sound Gradual Typing Dead?_](https://www2.ccs.neu.edu/racket/pubs/popl16-tfgnvf.pdf) and [_Toward Practical Gradual Typing_](https://www2.ccs.neu.edu/racket/pubs/ecoop2015-takikawa-et-al.pdf)


## Simple Random Approximation Method

The method above does not scale to large programs or fine granularities
 because it asks for an exponential number of measurements.
E.g., if there are 20 units to add or remove types from, then there are 1 million
 configurations to measure.
Exponentials are bad.

[_On the Cost of Type-Tag Soundness_](http://www.ccs.neu.edu/home/types/publications/publications.html#gm-pepm-2018),
 suggests a method based on simple random sampling that answers a similar question.
Instead of measuring the true proportion of configurations that run at most
 **D** times slower than the original dynamically-typed program, we:

- pick a sample size **s** (in the paper, we used **s = 10M** where **M** is the number of units),
- pick a number of samples **r** (in the paper, we used **r = 10**),
- and build a 95% confidence interval for the true proportion of configurations
  that run at most **D** times slower than the original program (from the
  **r** proportions of configurations that run at most **D** times slower than the
  original program in each of the **r** samples).

The method is outlined above, described in the paper, and validated in that paper's appendix.
Please let us know if you have more questions.

> Maybe you're wondering, "gee why do they keep writing out 'configurations that
>  run at most ....' instead of something shorter?".
> Well, the short version is _**D**-deliverable_ and it was introduced in the
> [_Is Sound Gradual Typing Dead?_](https://www2.ccs.neu.edu/racket/pubs/popl16-tfgnvf.pdf) paper.
> Unfortunately, (1) that paper instantiated **D** to **3**-deliverable in order to
>  explain a few graphs and (2) at least two published papers ([paper 1](https://dl.acm.org/citation.cfm?id=3009849), [paper 2](https://dl.acm.org/citation.cfm?id=3133878))
>  now cite us as saying **3**x overhead is the cutoff between a good migratory
>  typing system and a bad one.
>
> ...
>
> If we can't trust scientists to understand, then we _definitely_ can't trust
>  you folks on the internet.



## FAQ

### Q. What is the sampling method useful for?

- Making a confidence interval for the true proportion of configurations that
  run at most **D** times slower than some baseline, for your favorite value of **D**.


### Q. What is the sampling method **not** useful for?

- Finding the slowest configuration.
- Finding the average running time of all configurations.
- Evaluations where "removing types" might involve changing **List[Int]** to **List[Dyn]**, etc.
- Situations where its wrong to assume that a programmer will start from untyed and pick a configuration uniformly at random
- .... many more ....


### Q. How does migratory typing relate to gradual typing?

Gradual typing is not just about adding a type system to an existing programming
 language.
See [_Refined Criteria for Gradual Typing_](http://drops.dagstuhl.de/opus/volltexte/2015/5031/)
 and [_Migratory Typing: 10 Years Later_](http://drops.dagstuhl.de/opus/volltexte/2017/7120/)
 for details.


### Q. Do you have code I can use to plot sampling data?

Yes, start here:

- <http://docs.racket-lang.org/gtp-plot/index.html#%28def._%28%28lib._gtp-plot%2Fplot..rkt%29._samples-plot%29%29>

Please ask questions and open issues if you have trouble.
The source is here:

- <https://github.com/bennn/gtp-plot>


### Q. Where is code for the sampling paper?

Start here:

- <https://pkgd.racket-lang.org/pkgn/package/gm-pepm-2018>

Source is here:

- <https://github.com/nuprl/retic_performance>


## Closing Thoughts

Statistics is easy to do wrong.
Please let us know if you think our method is doing bad statistics.
