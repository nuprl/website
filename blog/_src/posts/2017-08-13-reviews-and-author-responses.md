    Title: Reviews and author responses: we should stop asking for 500-word responses
    Date: 2017-08-13T14:29:41
    Tags: by Gabriel Scherer

This year I reviewed many ICFP submissions, and got to be on the
receiving end of equally many author responses (also sometimes called,
somewhat combatively, rebuttals). I found that there was a large
difference between the official written advice on author responses and
what I, as a reviewer reading the responses, found effective. In
particular, I now believe that limiting yourself to 500 words should
strongly be avoided -- we should even stop giving that advice.

<!-- more -->

This year, I had the honor (and accompanying load of work) of being
a Program Committee (PC) member at the ICFP conference. It was my
first time being a PC member at a conference, and I found it extremely
pleasant and interesting, thanks to the authors who sent us articles
to review, my fellow PC members, and the ever-smiling careful
balancing work of our PC chair, Mark Jones. It was also a lot of work,
starting with 18 reviews to do over a month and a half, an intense PC
meeting, and the new "second phase" process with the opportunity for
authors and reviewers to exchange feedback on changes requested by the
program committee.

There is little guidance on how to write author responses, although
this [blog
post](http://www.pl-enthusiast.net/2014/09/17/advice-writing-author-response/)
by Michael Hicks on pl-enthusiast is quite good. One thing that is
obvious as a reviewer and is only slightly brushed in this post,
however, is that author responses should *not* aim to fit a 500 words
limit, and in fact I believe that it is a  bad idea to do so.

As for most conference, the ICFP review system recommends (in writing)
to authors to keep their response to 500 words (some systems also
highlight words after those in red to make the point clear). Don't do
this! The least convincing responses I have seen are those that
followed this recommendation.

(I have also seen at least 18*2 other reviewers read the same
responses I read, most of them well over 500 words, and *none* of them
made any comment on the length of the author responses.)

We have a frustrating situation where the explicit rule is different
from the thing people do in practice. This is bad for newcomers that
do not know the norms and cannot tell if ignoring the rule may hurt
them. This is the point of this blog post:

- If you are an author, please know that disrespecting the 500-words
  limit is the *right thing* to do. You should also know, of course,
  that people have limited time, so keep the main body of your
  response reasonably long. (I spent a day and a half on your paper
  already, I am willing to spend 10 additional minutes reading the
  response.)

- If you are a program committee chair or a Magical HotCRP Wizard,
  please remove this silly recommendation to keep responses to 500
  words. (See an alternative proposal at the end of this post.)

## My personal response format

My author responses start with general comments that elaborate on the
main point I want to tell all reviewers. Then, a second part
contains per-reviewer comments (one section per reviewer);
it is clearly marked as skippable. Here is the skeleton of the last
response I wrote:

> We thank the reviewers for their work on our article and their
> detailed feedback.  We start with a general discussion that responds
> to the salient points raised by reviewers.  In a second part, we
> provide detailed responses to the questions/remarks of each
> reviewer.
>
> ### General discussion
>
> [..]
>
> ### Specific questions/comments: review #A
>
> [..]
>
> ### Specific questions/comments: review #B
>
> [..]
>
> ### Specific questions/comments: review #C
>
> [...]

For this particular response, the "General discussion" section used
1296 words according to `wc -w` (M-x shell-command-on-region). In the
following sections, I quote the reviews to answer specific points,
email-style (following the Markdown syntax that HotCRP
renders properly).

## Suggested wording for PC chairs

If you are a PC chair, you should remove the suggestion of respecting
a 500 words limit for your conference. Here would be a suggested
alternative wording:

> Please remember, in writing your author response, that reviewers may
> stop reading the response at any point. We suggest having
> a reasonably-sized main section where you make your most important
> high-level comments, and clearly marked sections where you answer
> individual reviewer's questions. It is not useful nor productive to
> answer every point of each review, you should focus on the comments
> that you believe the reviewers are most interested in.
