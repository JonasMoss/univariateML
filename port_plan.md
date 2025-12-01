## C++ Port Plan

- Current C++ (via `src/ml_exports.cpp` + `inst/include/ml_core.hpp`):
  - Burr (mlburr_rcpp, burr_lmom)
  - Cauchy (mlcauchy_rcpp)
  - Gumbel (ml_gumbel_rcpp)
  - Logistic (mllogis_fast)
  - inverse_digamma helper

- Priority ports (start with easy):
  - **beta / betapr**: Newton on digamma/trigamma; invert-digamma already available. Easy.
  - **gamma / lgamma**: Newton on shape (digamma/trigamma), rate/shape-log via mean/log-mean. Easy.
  - **closed-form/easy**: d-unif, exp, geom, pois, binom? (binom medium), norm/lognormal (lwnorm), pareto, rayleigh, dunif/unif, laplace, mllaplace, mlnorm, mlnbinom, mlzip, mlzipf (medium), mllogitnorm, mllnorm.

- Medium:
  - gompertz (Newton with exp sums)
  - kumar (Newton on a with polygamma)
  - invgamma, invweibull, invgauss (Newton / transforms)
  - weibull (Newton on shape)
  - lomax, naka, nbino
  - llogis/nlm-based logis; logis uses external nlm

- Hard (external optimizers/other packages):
  - ged, sged, std/sstd, paralogis (stats4::mle), gser (Lambert W)

- Naming/layout:
  - Keep pure cores in `inst/include/ml_core.hpp` (per-model helpers).
  - Export only ML-facing Rcpp in `src/ml_exports.cpp` with literal defaults (tol=1.220703e-4, maxit=100).

- Next steps:
  1) Port beta/betapr and gamma/lgamma to C++ core + exports.
  2) Add a sweep of easy closed-form ports (exp, geom, unif, pareto, rayleigh, laplace, norm/lognorm, pois, etc.).
  3) Migrate remaining medium items incrementally; hard ones last or revisit with alternative approaches.
