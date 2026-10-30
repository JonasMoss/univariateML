# Numerical routine inventory — preliminary review

Revision `5f11fb9e878219ac47a9a372e985eb69da6198e3`. This is **not an
exhaustive routine-by-routine audit**. Entries below identify major shared
paths, their immediate uses and evidence, and the remaining work before a
step-7 replacement review. Family-local routines in `mle.hpp` and `dpqr.hpp`
are not yet completely enumerated; their domains, users and error bounds
must be audited before claiming coverage.

| Path | Users and contract | Evidence and limits / review candidate |
|---|---|---|
| `core/lambert_w.hpp` | ZIP fitter, real W0 and W−1 branches; iterative refinement | Branch-point and extreme range/error behavior need explicit comparison with Boost and tests before replacement. |
| `core/inverse_digamma.hpp` | Kumaraswamy fitting; inverse positive-domain digamma | Initialization and fixed iteration behavior need range and residual validation; compare a safeguarded solver. |
| `core/newton.hpp` | gamma, fatigue, and other scalar fits; relative/hybrid and absolute step variants | Step convergence alone is not a global certificate; family safeguards and statuses matter. Gamma's absolute rule preserves historical fits. |
| `core/bfgs.hpp`, `finite_diff.hpp`, `reparam.hpp` | Optimizers and transformed parameters, including std/sged/sstd and others | [Multistart report](2026-10-30-switchfix-multistart-report.md), `cppunivariate/tests/test_bfgs.cpp`; finite differences and local convergence are numerical, not global guarantees. |
| `core/multistart.hpp` | BFGS and simplex candidate selection, with score acceptance for failed searches | R6 score gate and best objective do not prove globality; see multistart report and `roadmap/decisions.md`. |
| `core/reductions.hpp`, `tie_collapse.hpp` | sample reductions and collapse checks | Floating-point accumulation and tie definition are part of caller contracts; audit cancellation and overflow per use. |
| `special/tails.hpp` | shared log-tail arithmetic, gamma and normal tails, discrete inversion | [tails report](2026-10-29-tails-report.md), [truth table design](2026-10-29-dpqr-truth/README.md). Smaller-tail log arithmetic protects underflow; no universal floating-point enclosure. |
| `gb2/beta_tails.hpp` | GB2, genbeta, beta-prime and location-scale t tail paths | Logit-coordinate beta tail and quantile bridge; check extreme parameters, rounding, fallback and no-throw behavior before relocation or consolidation. |
| `special/cmp.hpp` | CMP normalizer, moments and fitter | [CMP report](2026-10-29-cmp-normalizer-report.md): bounded direct tails and Euler–Maclaurin remainder in real arithmetic; rounding not enclosed, work limits can stop. |
| `special/skewed_densities.hpp` | GED, skew GED, skew normal and skew t fits and densities | Parameter transformations and stable standardized log densities; family tests and quality checks are numerical evidence, not error bounds. |
| `special/special.hpp` | no-throw Boost.Math seam plus standard-library and custom tail paths | Includes beta, gamma, normal, logistic, Student t, binomial, Poisson, negative-binomial, inverse-Gaussian, noncentral chi-square, zeta, polygamma and Owen T. Several `fs_*` tail paths are custom; do not equate these with direct Boost calls. Full per-function path/range/fallback audit is outstanding. |
| family-local `profile.hpp`, `tails.hpp`, `dpqr_core.hpp`, `fixed_scale.hpp`, `hold_search.hpp` | truncated, zero-modified, beta-prime, Burr, GED, t and skew-family callers | These files and inline helpers inside fitters/densities require individual inventory; common names do not establish interchangeable contracts. |

The simplification candidates are Lambert W, repeated scalar solvers,
overlapping distribution/tail helpers, beta-tail relocation and any other
custom/Boost overlap found in the completed inventory. Comparison must cover
range, endpoint and log-tail behavior, convergence/failure reporting,
no-throw semantics, accuracy, compatibility and performance; overlap alone
is not a replacement justification. The concurrent TASK-78 owns `special/`
and d/p/q/r sources: do not modify them as part of this inventory. A full
map of all Boost wrapper calls, their users, and the family-local custom
routines remains to be produced.
