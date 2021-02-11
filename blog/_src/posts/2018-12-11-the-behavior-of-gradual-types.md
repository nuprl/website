    Title: The Behavior of Gradual Types: A User Study
    Date: 2018-12-11T19:50:33
    Tags: migratory typing, gradual typing, extended abstract, by Ben Greenman

<!-- more -->

> Note: this post is an extended abstract for the paper _The Behavior of
> Gradual Types: A User Study_ by Preston Tunnell--Wilson, Ben Greenman,
> Justin Pombrio, and Shriram Krishnamurthi. For the full paper, datasets,
> and slides, [click here](http://www.ccs.neu.edu/home/types/publications/publications.html#tgpk-dls-2018).

The long-term goal of gradual typing is to build languages that offer the
 "best" of both static and dynamic typing.
Researchers disagree, however, on what the semantics of a mixed-typed language
 should be; there are [at least three competing proposals](/blog/2018/10/06/a-spectrum-of-type-soundness-and-performance/)
 for combining a dynamically-typed language with a similar statically-typed language.

> It's an interesting situation.
> There are dozens of papers on the semantics of gradual types---and
> [many claim](http://www.ccs.neu.edu/home/types/resources/talks/tgpk-dls-2018.pdf)
> to have developers in mind---but zero papers that ask developers what they think.

To help inform the discussion, we recently designed a [survey][data]
 to see what programmers think of three mixed-typed semantics.
The survey is based on 8 example programs; we selected these 8 programs because the set as a whole tells the three mixed-typed semantics apart.
For each program, the survey presents a few possible outcomes of running the
 program and asks participants for their opinion on each outcome.

The image below shows one program from the survey:

  ![Figure 1: example program](/img/gtsurvey-example-program.png)

This program creates an array, passes it between typed and untyped variables,
 and performs write & read operations.
What should happen when we run this program?
One option is to ignore the type annotations and return the second element
 of the array (`"bye"`).
A second option is to reject the write operation (on line 4) because it attempts
 to write a number to a variable of type `Array(String)`.
A third option is to reject the assignment after the read operation (on line 5)
 because it attempts to assign a string to a variable of type `Number`.
These are the three behaviors in the survey:

  ![Figure 2: behaviors for the example question](/img/gtsurvey-example-behaviors.png)

> A fourth option is to reject the assignment of an `Array(String)` to a
> variable of type `Array(Number)`. A few participants left comments asking
> for this behavior. See the [anonymized responses][data]
> for their comments,
> and see [the paper][paper]
> for why we left that behavior out.

For each behavior, we asked for respondents' preference along two independent dimensions:

- Do you _like_ or _dislike_ this behavior?
- Does it match your _expectation_ as a programmer?

Combined, the dimensions lead to four possible _attitudes_: Like and Expected,
 Like and Unexpected, Dislike and Expected, Dislike and Unexpected.
The full example question, with attitudes and space for comments, is below.

  ![Figure 3: complete question](/img/gtsurvey-example-question.png)

We administered the survey to three populations --- software engineers,
 students, and Mechanical Turk workers --- and thereby collected three sets of
 attitudes for each question.
The results for the running example are below:

  ![Figure 4: results for Question 7](/img/gtsurvey-example-data.png)

The figure is a matrix of three columns (one for each population)
 and three rows (one for each behavior).
Each cell of the matrix contains a bar chart showing the attitudes
 that we collected.

> Unlike the survey question, the behaviors in the results are labeled as
> **Deep**, **Erasure**, and **Shallow**. These names describe the three
> mixed-typed semantics.

For this question, the software engineers (left column, green bars)
 mostly picked the "Dislike and Unexpected" attitude for every behavior.
The students (mid column, blue bars) also show consensus on "Dislike and
 Unexpected" for the **Deep** and **Erasure** behaviors; however, they are split
 for the **Shallow** behavior.
The Mechanical Turk workers are divided on every behavior.

See [the paper][paper] for the other questions and responses.

Overall, our main finding is that respondents preferred behaviors that enforced
 full types and reported runtime mismatches as early as possible.
The takeaway is thus:

<p style="margin-left: 40px; margin-right: 40px">if you are designing a
mixed-typed language and choose <strong>not</strong> to enforce full types, then make sure
to explain this behavior to users!</p>

Put lots of example programs in the language's documentation.
The programs in the survey can be adapted to explain how your chosen
 behavior differs from alternatives.



## Questions

Here are some good questions we've gotten that are not clearly answered in the paper.

#### Q. Did any respondents "expect" more than one behavior?

Yes, 59% <!-- 20/34 --> of the software engineers
and 82% <!-- 14/17 --> of the students selected "Liked and Expected" and/or
"Dislike and Expected" for different behaviors on the same program.

<!-- They probably interpreted "Expected" as -->
<!--  "the program does something that makes sense", rather than -->
<!--  "the program does the one thing that I believe it should do". -->

<!-- ids for "double-expect" S.Es : R_24bz47lgcAOkCux R_2R4dZ1l0t3yx6fW R_b7yMVe7VtmmsrHb R_31MXSUfCyDE8FdG R_6LGXyOirYNtYWd3 R_2qyMZBAs74PrsSz R_2ASFRBh2jfuRgP1 R_1PUc0AUEzdXKGt8 R_2dL60N9oPIkbvWY R_1BXXqYyxH7R4r9l R_1ON2sxGalcODyAd R_1oyZasBudU5gKPS R_1FIHgkQbWGaxuHd R_b1s2YMBWCrCRvxf R_29t0zWxkQsfb9FT R_2fevZOrFGzS6JLf R_8Dn6NMjDyigT59n R_2pRG370z3cBUaKv R_2qDXTFI53ntWMu4 R_ZI8AwATueqyWwOR -->
<!-- ids for "double-expect" students : R_9B6WHWEX5l0DskN R_22VAu37cGWQPQx1 R_3hgYSaGy2tbyY3G R_3rTbAqgn1rhQK4d R_r3HqAP1yGRXHaZX R_1l05qvQ1sYOCcCF R_3qaMT9xR7CRYg2Y R_1Li0sGHkxk1VfcA R_24ITtgvBzg9RpE3 R_3HzshHbDWkayp4t R_5mtEFLtSX0iPVOp R_1IR6vdpmVw4OCqV R_2XpWlkKjH9LQqln R_DoQrROe0dcb1YJz -->


#### Q. Did the respondents have a prior preference for static or dynamic typing?

Near the end of the survey we asked: "Which do you prefer, typed or untyped programming?".
See table 2 of [the paper][paper] for coded responses to this question,
 or the [anonymized responses][data] for the ground truth.
Most preferred typed programming.


[paper]: http://cs.brown.edu/~sk/Publications/Papers/Published/tgpk-beh-grad-types-user-study
[data]: http://cs.brown.edu/research/plt/dl/dls2018
