# nuprl.github.io

Overview
========

This is the **source code** for the website for the
Northeastern University Programming Research Lab,
hosted at prl.ccs.neu.edu.

To make changes to `prl.ccs.neu.edu` from here:
1. Commit/push to the `src` branch.
   Be sure to create a merge commit.
2. Checkout/pull the `master` branch
3. Run `make sync` on master. This will:
   - `cherry-pick` the last merge commit to the `src` branch
   - rebuild the site
   - commit/push all changes to `master`

If you did not make a merge commit, replace (3) with:
  - `git cherry-pick <COMMIT-SHA>` to pull individual commits to master
  - Run `make`
  - Commit/push to the `master` branch


Building
========

Build Prerequisites: [Racket](http://racket-lang.org/)

You can regenerate the templated pages by running `make`. Eventually we will
have a script that does this automatically on commit.
