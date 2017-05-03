    Title: No Good Answers, Gradually Typed Object-Oriented Languages
    Date: 2017-05-09T14:04:31
    Tags: HOPL, Gradual Typing, by Ben Chung

<!-- more -->

Untyped code remains a real problem in practice, as a result of reduced
performance and hindered readability. One approach to solve this problem
is gradual typing.

Gradual typing puts the onus on the developer to add type annotations,
statically checks whatever type annotations have been written, and dynamically
ensures that untyped code does not violate those annotations. A number of
approaches have been put forward to try to achieve these objectives while
retaining efficiency, semantic meaning, and the ability to actually type
untyped code.

I discussed three systems, all of which achieve the objective of typing untyped
code in different ways, and all of which have different tradeoffs.

Unofficial Notes:

- <https://github.com/nuprl/hopl-s2017/blob/master/lecture_notes/2017-04-18.md>

Code Examples:

- <https://github.com/BenChung/GradualComparison/tree/master/examples/HOPL>
