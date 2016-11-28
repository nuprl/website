    Title: SRC-submissions
    Date: 2016-11-17T13:52:52
    Tags: by Gabriel Scherer

Max New, Daniel Patterson and Benjamin Greenman recently wrote three
two-page abstracts on what they are working on right now. Come have
a look -- and any feedback is welcome!

<!-- more -->

## Gradual Type Precision as Retraction

[Gradual Type Precision as Retraction](http://maxsnew.github.io/docs/precision-as-retraction.pdf)  
Max New  
2016

> Gradually typed programming languages allow for a mix of precision
> of static type information, allowing advanced type features to be
> added to existing languages, while still supporting interoperability
> with legacy code. The advantages of gradual typing are enticing to
> researchers and practitioners alike, but a general theory of
> gradually typed languages is only beginning to emerge after a decade
> of research.
>
> It has long been noted that there is much similarity between work on
> contracts and gradual typing, and the use of retracts in domain
> theory which were used to relate models of untyped and typed lambda
> calculus in [Scott(1976)](https://pdfs.semanticscholar.org/359e/ca57fe42d97cbb67f0b5591869abe5eb5421.pdf) and [Scott(1980)](http://andrewkish-name.s3.amazonaws.com/scott80.pdf). Here we take this
> connection seriously and consider how judgments in modern gradually
> typed languages can be framed in terms of retractions. While
> retractions in programming languages were originally studied in
> terms of denotational semantics in domains, our presentation will
> use only the most basic elements of category theory: composition,
> identity and equality of terms, so our formulation is equally
> applicable to axiomatic or operational semantics.
>
> In particular we propose a semantic criterion for the notion of
> precision of gradual types, a common judgment in gradually typed
> languages (sometimes called naive subtyping for
> historical reasons). We relate it to a previous definition from
> [Wadler and
> Findler(2009)](https://www.eecs.northwestern.edu/%7Erobby/pubs/papers/esop2009-wf.pdf)
> that defines type precision in terms of blame. We show that our
> definition decomposes in a similar way into “positive” and
> “negative” type precision, but without depending on a specific
> notion of blame in the language.

## Linking Types: Specifying Safe Interoperability and Equivalences

[Linking Types: Specifying Safe Interoperability and Equivalences](https://dbp.io/pubs/2016/linking-types-poplsrc2017-proposal.pdf)  
Daniel Patterson  
2016

> All programs written in high-level languages link with libraries
> written in lower-level languages, often to expose constructs, like
> threads, random numbers, or automatic serialization, that aren’t
> possible in the high-level language. This linking usually takes
> place after compiling both languages to a common language, possibly
> assembly. In this sense, reasoning about crosslanguage linking means
> reasoning about compilation.
>
> While most languages include cross-language linking (FFI)
> mechanisms, they are ad-hoc and can easily break the semantic
> equivalences of the source language, making it hard for source
> programmers to reason about correctness of their programs and hard
> for compiler writers to reason about correctness of their
> optimizations.
>
> In this work, I design and motivate linking types, a language-based
> mechanism for formally specifying safe linking with libraries
> utilizing features inexpressible in the source. Linking types allows
> programmers to reason about their programs in the presence of
> behavior inexpressible in their language, without dealing with the
> intricacies of either the compiler or the particular language they
> are linking with.

## Pruning Contracts with Rosette

[Pruning Contracts with Rosette](http://www.ccs.neu.edu/home/types/resources/popl2017-src.pdf)
Benjamin Greenman
2016

> [Contracts](http://www.ccs.neu.edu/racket/pubs/icfp16-dnff.pdf) are
> a pragmatic tool for managing software systems, but programs using
> contracts suffer runtime overhead. If this overhead becomes
> a performance bottleneck, programmers must manually edit or remove
> their contracts. This is no good. Rather, the contracts should
> identify their own inefficiencies and remove unnecessary dynamic
> checks. Implementing contracts with
> [Rosette](https://emina.github.io/rosette/) is a promising way to
> build such self-aware contracts.

## While we're at it let's rant on SRCs

These abstracts are submitted at POPL's "Student Research
Competition". You submit an abstract, and if you get accepted to that
thing, you get a bit of travel support money, and you have to prepare
a poster and present it at the conference.

I have a firm dislike for the *Competition* part of that concept:
I think that people think of research too competitively already, and
that we should have less of that, not more. (Having some is
unfortunately unavoidable in scarce-resource situations.) I think that
the process of awarding prizes to students with the "best poster" is
dumb -- and borderline ridiculous.

On the other hand, my experience seeing them writing these extended
abstracts is that it's a useful exercise for them, and produces nice
result -- short, readable introductions to their ideas. And Jennifer
Paykin [convincingly
argues](https://github.com/gasche/icfp2016-blog/blob/master/SVs/jennifer_paykin.md)
that although writing a poster is rather painful, actually presenting
it during the conference is interesting and useful. In her words,
"it's worth it to get the experience of authentic and fruitful
discussions". Plus having posters in the corridors of one's lab is very
nice.

I think we could have "Student Research Sessions" or "Student Poster
Sessions", where students are encouraged to present their work, would
write those nice extended abstracts and posters, interact with
researchers at the conference, and get travel money, without the
ranking and prize stuff. (I would still encourage students to
participate to SRC today, it seems to be worth it.)