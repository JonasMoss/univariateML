# CMB free-size holds: partial analysis and oracle

The three free-size subsets with a free canonical coordinate remain
`not-implemented`. This investigation does **not** establish a complete
classification of their limiting laws, existence conditions, or a reliable
global integer-profile stopping criterion. The numerical sweep is evidence,
not a certificate. No free-size optimizer or new `limits:` entry was added.
The `holds:` reasons and wrapper diagnostic now identify the blockers.

## Proved limiting paths

Write `N=size`, `eta=logit(prob)`, and the unnormalized weight as
`choose(N,k)^nu exp(k eta)`.

* With `nu>0` fixed and `exp(eta)=lambda/N^nu`, each fixed-k weight tends to
  `lambda^k/(k!)^nu`. Since `choose(N,k)<=N^k/k!`, these weights are
  dominated by the summable CMP weights. Thus the normalized limit is CMP,
  including Poisson at `nu=1`. This path is available when `prob` is free;
  in the jointly free subset, `nu` may also tend to any positive constant.
* At `nu=0`, fixed `prob<1/2` gives a geometric limit with ratio
  `r=prob/(1-prob)`. If instead `nu=0` and `prob` is free, any geometric
  ratio in `(0,1)` is available by holding the odds equal to that ratio.
* A geometric limit is also reachable with a **fixed** `prob<1/2` and free
  dispersion: choose `r` strictly between its odds and 1, and set
  `nu_N=log(r/odds)/log(N)>0`. Then the fixed-k weights converge to `r^k`
  and are bounded by `r^k`; dominated convergence proves the normalized law.
  This establishes a family of paths, not an exhaustive classification.
* Escape of mass to infinity also matters. For example, take `prob=1/2`
  and fixed `nu=-2`. The two endpoint weights are 1, and the total of
  interior weights is at most `(N-1)/N^2`, since every interior binomial
  coefficient is at least `N`. Half the probability tends to zero and half
  escapes to infinity. Thus a catalogue restricted to proper probability
  laws on the nonnegative integers would miss relevant likelihood limits.
* At a fixed size, the known exponential-family polygon gives point-mass,
  adjacent-two-point and endpoint-two-point dispersion faces, with the
  availability of each face constrained by held coordinates. They must be
  compared across sizes as well as against size-to-infinity faces. At
  `nu=1`, finite-size CMB is exactly binomial.

## Existence and search by held subset

* `{}`: constant data attain likelihood one at `N=max(x)` and `prob=1`
  (at zero, `prob=0` also works). Data supported on `{0,1}` have an attained
  Bernoulli fit at `N=1`. These are sufficient conditions only. In general,
  fixed-size dispersion faces, CMP (including Poisson), geometric, and
  other joint escape paths must be compared. No all-size upper bound or
  reliable numerical stopping rule was established; the subset remains held.
* `{prob}`: for `prob=0`, compatible all-zero data attain likelihood one;
  for `prob=1`, compatible constant data attain it at their value of `N`.
  Interior held probabilities permit the geometric path above for
  `prob<1/2` and fixed-size dispersion faces. A complete classification,
  including mass lost at large counts and a comparison across sizes, remains
  open; the subset remains held.
* `{nu}`: any constant sample has an attained likelihood-one fit at
  `N=max(x)` and an endpoint probability. For positive `nu`, the CMP face
  has the same held dispersion; `nu=1` includes Poisson. At `nu=0` the
  free-probability geometric face is available. Negative `nu` can lose
  endpoint mass as above; its complete profile comparison is open. No
  global tail bound or validated numerical margin was found; the subset
  remains held.

For contrast, the previously implemented `{prob,nu}` search enumerates sizes
and stops only when its analytic decreasing upper bound rules out the entire
tail; the existing work budget reports failure instead of a capped estimate.
The allocation ceiling is 1e7. The present work does not change that search.
None of the three remaining holds is marked `no-mle`: attained examples above
prove that such a label would be wrong.

## Numerical oracle and tests

`2026-10-30-holds-cmb/oracle.R` reproduces P0b's random-number consumption
and samples, then profiles the package's `dcmb` independently in R with
multiple BFGS starts, using `eta=a-nu*log(N)` for free probability. It
records 756 profiles at 21 sizes per sample/subset, including the original
`default_100`, n=20/100 samples, ties, adjacent and endpoint data,
negative-dispersion samples, and Poisson samples. Held `nu` values include
1.5 and -0.6. The accompanying CSV records CMP (`dcmp`), geometric (`dgeom`),
Poisson (`dpois`), and finite binomial (`dbinom`) likelihood comparisons.
The CMP face optimizations and all profiles are numerical; five selected
optimizers reported nonconvergence, all on adjacent data. No output is a
proof of a global maximum, and bounded-size grids cannot certify a tail.

For P0b `default_100`, the fully free numerical profile at `N=2000` is
approximately -164.0930, still the best on this grid, versus -164.0919 for
the numerically fitted CMP face. This is consistent with a slow approach to
CMP, but does not prove escape or exclude a later finite maximum. For fixed
`prob=.3`, the best sampled size is 10; for fixed `nu=1.5`, it is 11.
Those are grid observations only. A regression test checks the new held-prob
geometric path at three increasing sizes, in addition to the existing CMP
and fixed-coordinate geometric path tests.

Final simbox verification: `just cpp-test` 6/6 CTest targets; `just test`
`[ FAIL 0 | WARN 0 | SKIP 0 | PASS 30745 ]`; `just quality cmb` 1/1 passed.
The first R run exposed two incorrect test assertions; both were corrected
and the complete suite rerun. Logs are in `build/scratch/`.

## Copyable corrections to P0b and outstanding work

P0b's claim that both held coordinates always make the likelihood decay
geometrically is false: when `nu=0` and `prob<1/2`, the size limit is
geometric with ratio `prob/(1-prob)`. Nevertheless, the smallest feasible
size maximizes likelihood in that case because its normalizer increases.
Positive held `nu` with odds `lambda/N^nu` has CMP limit weights
`lambda^k/(k!)^nu`; it is not generally a two-point limit. Geometric laws
also arise with fixed interior `prob<1/2` and a varying `nu` as above.
The P0b `default_100` ambiguity is narrowed but remains unresolved.

Complete the joint-limit and mass-escape classification, compare every
reachable fixed-size face, and prove a global size-profile bound or validate
a defensible numerical criterion with an explicit margin before enabling
these subsets. Add escape statuses, `limits:` declarations, C++ search tests,
and oracle comparisons when an algorithm is supported. If sizes beyond 1e7
must be evaluated, TASK-78 owns `cmb/dpqr.hpp` and `special/cmp.hpp`; their
allocation and normalizer interfaces would need separate review. No
TASK-78-owned files were changed here.
