# GB2 implementation report

Status: implemented and validation passed; no commit made.

## Files

New `univariateML-cpp/include/cppunivariate/distributions/gb2/`:
`README.md`, `mle.hpp`, `dpqr.hpp` (stable density), `adapter.cpp`,
`test.cpp`, `test.R`, `golden.json`.
Shared changes only:
- `univariateML-cpp/include/cppunivariate/cppunivariate.hpp`: one include.
- `bindings/r/CANONICAL_NAMES.md`: GB2 coefficient/reference row.
- `bindings/r/tests/testthat/test_input_checks.R`: GB2 fixed-p/q arguments
  for the successful-fit contract (the free ten-point sample escapes).
No reference fixes, existing goldens, protected documentation, board, or git
index/history were changed. Generated package/build files are ignored.

## Mathematics: derived / proved algebraically

Actuar order is (shape1,shape2,shape3,scale)=(q,a,p,b).
The standardized-log objective uses softplus for both log-logistic tails.
For fixed p,q the objective is strictly concave in (A,c); holding a fixes A,
and holding b restricts c to a line. The same inner implementation handles
all 16 fixed subsets on nonconstant data. Holding a and b leaves the beta-shape likelihood.
Analytic envelope scores and the Schur-complement profile Hessian are used.
The beta normalizer uses exact shape-shift recurrences followed by a
Stirling remainder, avoiding subtraction of large log-gamma terms.
Double Pareto is evaluated at every sorted log-data kink, using
n log n - 2n log(sqrt(L)+sqrt(R)) - n and the log-data Jacobian.
The endpoint suprema use `pareto_mle` and `power_mle(x,0)` (no epsilon).
GG/inverse-GG comparisons reuse `trgamma_fit_log` on logs/negative logs;
its nested face statuses are propagated. Lognormal uses `lognormal_mle`.
Fixed-scale fits compare the available lognormal, fixed-kink double-Pareto
and one-sided fixed-endpoint faces. A held opposite shape incurs its
shape*log(2) penalty for observations exactly at a held endpoint.

## Mathematics: numerical evidence / assumptions

Outer log-p/log-q search uses a grid, Burr/inverse-Burr/beta-prime starts,
near-face starts, multiple refinements, and expanding boxes. Each seed gets
a short exploration; up to eight distinct promising basins get full refinement. Newton uses
the analytic envelope Hessian in concave basins; BFGS supplies ascent elsewhere.
A face wins likelihood ties within 1e-8*n after stationary/outward-edge
search evidence. A budget-exhausted search without such evidence is unresolved.
Interior results require a small
profile score and must be away from the searched box edge; failures have a
separate SearchUnresolved status. Double Pareto is named honestly, without
inventing a package fitter.
The existing truncated Stirling series is used numerically, without a
rigorous rounding/error enclosure. Derivative and oracle tests check its use.
This is a numerical multibasin search, not a global optimality proof.
In particular neither closure exhaustiveness nor a global certificate
excluding every unvisited outer basin has been proved by this implementation.

## Validation

Final C++: 4/4 ctest targets passed, 291 cases / 504878 assertions total
(BFGS 6/21, sampling 1/10204, distributions 281/494597, no-throw 3/56).
GB2 contributes 3 cases / 44 assertions, including the tiny-budget guard.
Final `just test`: 4384 passed, 0 failures, 0 warnings, 0 skips.
GB2's R file contributes 126 expectations (125 also passed standalone before
adding the budget guard).
`just quality gb2`: 1/1 passed; symbolic logLik differs by 1.82e-12,
and the independent eight-start oracle by 1.59e-12.
Tests cover independent multistart log-density oracles, all fixed subsets
and df/fixed metadata, reciprocal symmetry, scale equivariance, member fits,
all six face models, stable extreme densities, the exact double-Pareto
profile, numerical envelope gradient/Hessian checks, and exhausted budgets.
Only the new GB2 golden was generated (`UMVL_GOLDEN_IDS=gb2 just goldens`),
then refreshed after refinement changed its coefficients by about 2e-8;
its log likelihood was unchanged to rounding. Existing goldens are untouched.

Timings (slot-locked, nice 10, one thread, g++ C++17 -O2, installed BH):
n=10000: 3.39864 seconds, 752 profile evaluations, status Ok.
n=1000000: 308.724 seconds, 496 profile evaluations, status Ok.
Both use gamma-ratio draws with (a,b,p,q)=(2,1,.4,.3), mt19937_64 seed 12345.
Times include all face/member fits and the outer search, excluding data generation and compilation.
The final budget guard changes only status eligibility, not the timed search.
Initial BFGS-only n=10000: 18.4155 s / 10912 profile evaluations.

## Actuar defects (no runtime repairs installed)

Reproduced against installed actuar: the following return 0, -Inf, Inf, Inf.

```r
actuar::ptrbeta(.4,.003,1000,.002) # true about .09600094
actuar::ptrbeta(1e300,2,5,3,lower.tail=FALSE,log.p=TRUE) # about -6905.964
actuar::qtrbeta(.5,.01,1,1) # exact expm1(log(2)/.01), about 1.26765e30
actuar::qtrbeta(.001,.06,50,.04,lower.tail=FALSE) # finite, about 7.4
```

Ordinary pbeta on a rounded logistic does not repair these defects; log.p
controls the output, not the input. No reference replacement was justified.
Consequently R pml/qml inherit actuar's tail failures.

## Open questions

Global outer-search certification remains open, as in the design document.
