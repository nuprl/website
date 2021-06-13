    Title: Deep and Shallow Types
    Date: 2020-12-23T18:21:55
    Tags: dissertation, migratory typing, by Ben Greenman

I successfully defended my Ph.D. dissertation.
You can find the document, a talk recording, and much more here:

- <http://ccs.neu.edu/home/types/publications/publications.html#g-dissertation-2020>

To the PRL: thanks for a wonderful 6.5 years.

<!-- more -->

### Abstract

> The design space of mixed-typed languages is lively but disorganized.
> On one hand, researchers across academia and industry have contributed language
>  designs that allow typed code to interoperate with untyped code.
> These design efforts explore a range of goals;
>  some improve the expressiveness of a typed language, and
>  others strengthen untyped code with a tailor-made type system.
> On the other hand, experience with type-sound designs has revealed major challenges.
> We do not know how to measure the performance costs of sound interaction.
> Nor do we have criteria that distinguish ``truly sound'' mixed-typed languages
>  from others that enforce type obligations locally rather than globally.
>
> In this dissertation, I introduce methods for assessing
>  mixed-typed languages and bring order to the design space.
> My first contribution is a performance-analysis method that allows language
>  implementors to systematically measure the cost of mixed-typed interaction.
>
> My second contribution is a design-analysis method that allows language designers
>  to understand implications of the type system.
> The method addresses two central questions: whether typed code can cope with
>  untyped values, and whether untyped code can trust static types.
> Further distinctions arise by asking whether error outputs can
>  direct a programmer to potentially-faulty interactions.
>
> I apply the methods to several designs and discover limitations that motivate
>  a synthesis of two ideas from the literature:
>  deep types and shallow types.
> Deep types offer strong guarantees but impose a high interaction cost.
> Shallow types offer weak guarantees and better worst-case costs.
> This dissertation proves that deep and shallow types can interoperate
>  and measures the benefits of a three-way mix.

Next year, I'll be a [CI Fellow](https://cifellows2020.org) at Brown.

