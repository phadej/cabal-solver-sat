# cabal-solver-sat

Recently I wrote [a blog post](https://oleg.fi/gists/posts/2023-08-30-using-cabal-install-solver-as-sat-solver.html) about using `cabal-install` solver as SAT solver.
Natural question, which I had in the back of my mind for quite long time, is whether we can use off-the-shelf SAT solver as `cabal-install` dependency solver?
I always assumed that we can, but I never bothered to try out myself.
So here we go.

