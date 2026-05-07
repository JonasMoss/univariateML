# Vision

univariateML is a portable C++17 + Boost.Math library for univariate
density estimation: maximum-likelihood fits and `d`/`p`/`q`/`r`
evaluation. R and (eventually) Python are thin shells around the C++
core.

## Three layers

1. **C++ core (primary).** `src/ml/` — header-only, no Rcpp, no
   Armadillo, no R headers. `mlcore::span` as the input boundary;
   Boost.Math (via the `BH` CRAN package or system Boost) for special
   functions and base distributions. Owns its own tests in
   `tests/cpp/` (doctest + ctest).

2. **R shell (testing scaffold and current published artifact).**
   Auto-generated R wrappers under `R/` call thin Rcpp adapters in
   `src/ml_exports.cpp` and `src/distribution_exports.cpp`. testthat
   acts as the trusted reference: every C++ change is sanity-checked
   against R-side fits and the golden JSONs in `tests/golden/`.
   Eventually the R shell becomes a pure adapter — no algorithm
   bodies in `R/`.

3. **Python shell (planned).** Same shape: pybind11 (or nanobind)
   adapters around mlcore. Vendor Boost.Math via `bcp` or use the
   system install. The same `tests/golden/` JSONs cross-validate
   across shells.

## Status (May 2026)

- 41 estimators in `src/ml/estimators/` (header-only, span-based).
- 30 distributions in `src/ml/distributions/` with `d`/`p`/`q`/`r`
  as templates.
- Hand-rolled BFGS + finite-difference gradient + reparam helpers in
  `src/ml/core/`, used by the six fGarch-family estimators
  (paralogis, std, snorm, sstd, ged, sged).
- Boost.Math is the math backend; see `math_backend.md`.
- R shell: 730 testthat assertions including 84 golden-regression
  assertions at 1e-12.
- C++ shell: doctest case per estimator (`mlcore_tests`), Rosenbrock
  smoke test for the optimiser (`bfgs_tests`), R-seeded sample
  reproducibility (`sample_tests`).
- `LinkingTo` is `Rcpp` + `BH`. No `RcppArmadillo`.

## What's left

- WS6 docs catch-up: paralogis 2-param upgrade note; vignette section
  on boundary-regime behaviour for {Lomax, paralogis, ged/sged/std,
  snorm/sstd, binom/nbinom}.
- Newton-with-explicit-Hessian variant of the optimiser. The BFGS
  `Cost` concept already supports it; an `mlcore::newton_md_minimize`
  reading the same interface plus `hessian()` is the natural
  follow-up.
- Python shell scaffolding.

## Out of vision (for now)

- Any dependency outside `Rcpp` + `BH` on the R side, `<cmath>` +
  Boost.Math on the C++ side.
- Bayesian inference, MCMC, regression. Univariate density
  estimation only.
- Bit-for-bit reproducibility across shells. Goldens use the C++
  core; R and Python results agree to that core's precision, not to
  scipy or fGarch.
