    Title: Programming Language Conference in Russia
    Date: 2017-05-24T12:25:17
    Tags: by Artem Pelenitsyn

In April 3--5 I took part into a Russian conference exclusively devoted to programming languages: [Programming Languages and Compilers][PLC] ([Google.Translated version of the site][PLCGT]). I was a member of organizing committee and had a paper there. 

This is the first conference in Russia highly focused on our area of PL. At least for the last several decades (I believe, there were conferences of the kind back in USSR). The conference was devoted to the memory of prominent Soviet PL-researcher from Rostov-on-Don, Adolf Fuksman who worked on ideas quite similar to what we know as the aspect-oriented programming back in the 70-s.

<!-- more -->

We designed and implemented the conference with my colleagues from I.I.Vorovich institute of Mathematics, Mechanics and Computer Science, Southern Federal University ([Rostov-on-Don][RND], Russia). We aimed at gathering as much PL-researchers and enthusiasts from academia in Russia as we could. One of the consequences of the aim was a decision to run the conference in Russian. Though we missed expertise from our non-Russian speaking colleagues, we got thorough participation from all over Russia:

> Saint-Petersburg, Moscow, Novosibirsk, Krasnoyarsk, Ekaterinburg, Kazan, etc.

I only mention here the cities with more than 1 mil. population in decreasing in number of conference participants order (and excluding Rostov itself, of course).

I particularly liked talks by invited speakers. When searching for ones, we targeted Russians who work at prominent universities and/or have some visibility at the international level. We ended up with two researchers: [Ilya Sergey][Ilya] (University College of London) and [Ekaterina Komendantskaya][Katya] (Heriot-Watt U., Edinburg, UK). Interestingly, their talks were quite close to each other: 

* I. Sergey, Dependent Types for verification of real-world programs,
* E. Komendantskaya, Automated Theorem Proving for Type Inference, Constructively.

Both of them talked about types as a logic tool to ensure program correctness.

Biggest opening in this conference for me was a team from Languages Tools Laboratory of [JetBrains][JB]. Surely, you heard about JB: either about their famous IDE,  IntelliJ IDEA, or the Kotlin programming language (which, by the way, [is endorsed][AndroKotlin] for Android development these days). You may also have noticed that JB become sponsors of ECOOP and OPLSS this year. So we had a whole team of researchers from Saint-Petersburg office of JB. Among their topics: `OCaml` embedding of `miniKanren` (some results was presented on ML Workshop 2016), parser combinator libraries for `OCaml` and constrained graph querying (this is not specifically a PL problem, see [arXiv:1502.02242][arXiv150202242] for details).

Otherwise the spectrum of topics presented on the conference was quite broad, here are some: 

* Static analysis with PVS-studio (a sponsor talk), 
* supercompilation (a talk by researchers from Pereslavl-Zalesskiy, where the topic is actively developed for decades), 
* C++ and beyond (by a member of the ISO C++ committee), 
* architecture-agnostic parallel programming languages and compilation techniques for parallel architectures, 
* game semantics and ontologies for PL semantics, 
* program analysis, 
* compiler architectures.

Full program with links to slides in Russian is available [here][plcprog].

Let me mention my submission: that was a joint work with a student of mine on exploring design space for parser combinator libraries using programming language with direct support of effect system, namely [Frank][Frank]. [Julia Belyakova][Julia] also participated in the conference with her work on Coq-certified interpreter for an extension of lambda-calculus with concept-parameters (module-like kind of thing). The follow-up of that work is accepted for FTfJP workshop this year. You can also listen to her on the topic at the [NEPLS][NEPLS] next week.

I hope that we will find sources, time, and, most important, high quality submissions for PLC-2018.

[PLC]: http://plc.sfedu.ru/
[PLCGT]: https://translate.google.com/translate?sl=auto&tl=en&js=y&prev=_t&hl=en&ie=UTF-8&u=http%3A%2F%2Fplc.sfedu.ru%2F&edit-text=&act=url
[RND]: https://www.google.com/maps/place/Rostov-on-Don,+Rostov+Oblast,+Russia/@49.8345629,18.9321123,4.5z/data=!4m5!3m4!1s0x40e3c777c3b4b6ef:0x8248b451e48b4d04!8m2!3d47.2357137!4d39.701505
[Ilya]: http://ilyasergey.net/
[Katya]: http://www.macs.hw.ac.uk/~ek19/
[JB]: https://en.wikipedia.org/wiki/JetBrains
[AndroKotlin]: https://blog.jetbrains.com/kotlin/2017/05/kotlin-on-android-now-official/
[arXiv150202242]: https://arxiv.org/abs/1502.02242
[plcprog]: https://docs.google.com/spreadsheets/d/11QiFUqJG_NiBHVUfji_6-FiqP3aQWmdDBN13abM32nY/edit?usp=sharing
[Frank]: http://popl17.sigplan.org/event/popl-2017-papers-do-be-do-be-do
[Julia]: http://staff.mmcs.sfedu.ru/%7Ejuliet/index.en.html
[NEPLS]: http://www.nepls.org/Events/30/

