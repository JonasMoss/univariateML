# TASK-113 closure checks

`check.py` is a deterministic mathematical reproducer, independent of the
library fitter. It writes `checks.csv` and `summary.json` beside itself.
It needs the already installed mpmath and SymPy; it installs nothing.

From the worktree root:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-03-bbinom-closure-data/check.py
```

The checks derive the joint Poisson expansion symbolically, compare explicit
NB/Poisson/defective paths with high-precision beta-binomial probabilities,
check both sides of the NB seam, and verify second- and fourth-order
coefficients. The numerical tolerances are assertions in the script; they
are convergence checks at stated points, not bounds on untested remainders.

The equidispersed counterexample search enumerates ordered multisets by
sample length, then lexicographically, with entries in `0,...,7`. It stops
at the first strict local-Poisson case for which one of 36 explicit shape
pairs at `N=M` beats Poisson. The search limit is a discovery budget; it
produces a feasible witness and never reports a capped estimate. Its final
witness is verified independently with exact rational arithmetic.

Exact certificates use Python `Fraction`: finite Taylor lower bounds for
`exp(S)`, and the geometric remainder for the positive atanh series for
`log(q)`. The latter brackets the unique NB face root for `(1,1,5)` between
`4.51` and `4.52`, bounds its likelihood, and proves that the uniform
`BB(5,1,1)` beats it despite the negative NB seam slope. The finer
80-digit bracket in `summary.json` is a numerical refinement, separate
from that exact certificate. No stochastic seed or optimizer is involved.

The [report](../2026-11-03-bbinom-closure-report.md) supplies the proofs,
interpretation, exact parameter conventions, and repository-suite results.

Run render.py through the same single-slot wrapper to check the new local
links and generate preview.html with the installed pandoc. The preview
uses MathML and contains no scripts; the Markdown report remains its source.
