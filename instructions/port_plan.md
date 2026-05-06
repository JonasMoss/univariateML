## C++ Port Plan

- Current C++ coverage (via `src/ml_exports.cpp` + `inst/include/ml_core.hpp`):
  - Core ports: burr, cauchy, gumbel, logistic, beta/betapr, gamma, binomial, lognormal/logit-normal, weibull/invweibull, invgamma/invgauss, lomax, negbinom, zip/zipf, gompertz, kumar, nakagami, invburr, lgser, plus closed-form exp/unif/pois/norm/laplace/geom/pareto/rayleigh, inverse_digamma helper.

- Remaining (hard/external optimizer or not yet tackled):
  - ged, sged, std/sstd, paralogis (stats4::mle)

- R-source cleanup to align with C++:
  - Replace legacy R implementations in `r-src/ml` for ported models with thin wrappers calling the corresponding `*_rcpp` functions (preserve logLik/iterations where needed).
  - Remove any now-redundant R metadata/comments from those files.

- Next steps:
  1) Sweep `r-src/ml` and convert ported models to delegate to the C++ wrappers; leave only hard models with R implementations.
  2) Add benchmarks/validations for the remaining hard models or consider alternative implementations to avoid external optimizers.
