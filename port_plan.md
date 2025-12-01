## C++ Port Plan

- Current C++ (via `src/ml_exports.cpp` + `inst/include/ml_core.hpp`):
  - Burr (mlburr_rcpp, burr_lmom)
  - Cauchy (mlcauchy_rcpp)
  - Gumbel (ml_gumbel_rcpp)
  - Logistic (mllogis_fast)
  - Beta/Betapr (mlbeta_rcpp, mlbetapr_rcpp)
  - Gamma/Lgamma (mlgamma_rcpp)
  - Binomial (mlbinom_rcpp)
  - Lognormal/logit-normal (mllnorm_rcpp, mllogitnorm_rcpp)
  - inverse_digamma helper
  - Closed-form: exp, unif, pois, norm, laplace, geom, pareto, rayleigh

- Priority ports (start with easy):
  - **Done**: beta/betapr, gamma/lgamma, binom, lognormal/logit-normal, closed-form exp/unif/pois/norm/laplace/geom/pareto/rayleigh.
  - **Next easy/medium**: mlnorm/mllaplace (R wrappers already using C++ core but revisit if needed), mlzip/mlzipf, mlnbinom, gompertz, kumar, weibull, lomax, naka, invgamma/invweibull/invgauss, llogis/logis nlm variant.

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
  1) Tackle the remaining easy/medium ports (mlzip/mlzipf, mlnbinom, gompertz, kumar, weibull, lomax, naka, invgamma/invweibull/invgauss, llogis/logis variant) and wire to `ml_exports.cpp`.
  2) Add benchmarks/validations for the new ports (iterations, convergence flags, edge-case parameters).
  3) Leave hard/external-optimizer models for a later pass or alternative implementations.
