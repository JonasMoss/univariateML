# univariateML — engineering notes

## Vision

univariateML is a portable C++17 + Boost.Math library for univariate
density estimation: maximum-likelihood fits and `d`/`p`/`q`/`r`
evaluation. R and (eventually) Python are thin shells around the C++
core.

### Three layers

1. **C++ core (primary).** `cppunivariate/include/cppunivariate/` — header-only, no Rcpp, no
   Armadillo, no R headers. `cppunivariate::span` as the input boundary;
   Boost.Math (via the `BH` CRAN package or system Boost) for special
   functions and base distributions. Owns its own tests in
   `cppunivariate/tests/` (doctest + ctest).

2. **R shell (testing scaffold and current published artifact).**
   Auto-generated R wrappers under `r/R/` call thin Rcpp adapters in
   `r/src/ml_exports.cpp` and `r/src/distribution_exports.cpp`. testthat
   acts as the trusted reference: every C++ change is sanity-checked
   against R-side fits and the golden JSONs in `quality/golden/`.

3. **Python shell (planned).** Same shape: pybind11 / nanobind
   adapters around cppunivariate. The same `quality/golden/` JSONs cross-
   validate across shells.

### Status (May 2026)

- 41 estimators in `cppunivariate/include/cppunivariate/estimators/` (header-only, span-based).
- 30 distributions in `cppunivariate/include/cppunivariate/distributions/` with `d`/`p`/`q`/`r`
  as templates.
- Hand-rolled BFGS + finite-difference gradient + reparam helpers in
  `cppunivariate/include/cppunivariate/core/`; the six fGarch-family estimators (paralogis, std,
  snorm, sstd, ged, sged) use this path.
- `LinkingTo: Rcpp, BH` — no `RcppArmadillo`.
- 730 testthat assertions including 84 golden-regression checks at
  1e-12; 5 ctest targets (`estimator_tests`, `bfgs_tests`,
  `newton_md_tests`, `hessian_tests`, `sample_tests`).

### Out of vision (for now)

- Any dependency outside `Rcpp` + `BH` on the R side, `<cmath>` +
  Boost.Math on the C++ side.
- Bayesian inference, MCMC, regression. Univariate density
  estimation only.
- Bit-for-bit reproducibility across shells. Goldens use the C++
  core; R and Python results agree to that core's precision, not to
  scipy or fGarch.

---

## C++ style guide (cppunivariate)

`cppunivariate/include/cppunivariate/` is the C++ artifact. Header-only, **C++17**, depends only on
`<cmath>` / `<algorithm>` / `<vector>` / `<cstddef>` and Boost.Math.

### Hard rules

- **No exceptions in cppunivariate.** Surface diagnostics via per-estimator
  `*Status` enums in the result struct. `Rcpp::stop` is fine in the
  R-shell adapter layer (`r/src/ml_exports.cpp`,
  `r/src/distribution_exports.cpp`) but never in `cppunivariate/include/cppunivariate/`.
- **No `using` declarations.** Always be explicit
  (`std::vector<double>`, `cppunivariate::span`, etc.).
- **No raw pointers.** cppunivariate takes `cppunivariate::span`; R-shell adapters
  use Rcpp types.
- **No printing or logging in cppunivariate.** Status enums only.
- **No Armadillo anywhere.** R-shell adapters use
  `Rcpp::NumericVector` / `Rcpp::IntegerVector`.
- **C++17 features OK; avoid C++20 syntax.** `cppunivariate::span` is the
  C++17 substitute for `std::span<const double>`.
- **Uniform parameter names**: `tol` for tolerance, `maxit` for
  iteration limits, across every estimator.

### Layout

```
cppunivariate/                        # the C++ artifact
  CMakeLists.txt                      # produces INTERFACE target `cppunivariate`
  include/cppunivariate/
    core/                             # span, optimisers (newton, bfgs), reductions,
                                      # finite_diff, reparam, metadata.hpp (shared
                                      # SupportDescriptor / ParamDescriptor types)
    distributions/<id>/               # one folder per distribution (id matches r/metadata)
      mle.hpp                         # *_mle(span x, ...) -> *Result + Cost struct
      dpqr.hpp                        # d/p/q/r templates (when present; some
                                      # distributions delegate to base R via the
                                      # R-side wrapper -- see r/metadata/<id>.json)
      metadata.hpp                    # constexpr id, name, support, params[]
      dossier.md                      # human-readable docs (stub today; filled later)
    distributions/rng_std.hpp         # shared sampling infrastructure (not per-dist)
    special/                          # special.hpp (Boost.Math seam), skewed_densities.hpp
    cppunivariate.hpp                 # aggregator (includes every <id>/mle.hpp)
  tests/                              # doctest + ctest, R-free
```

R-shell adapters live outside `cppunivariate/include/cppunivariate/`:

- `r/src/ml_exports.cpp` — Rcpp wrappers calling cppunivariate estimators.
- `r/src/distribution_exports.cpp` + `r/src/distributions/<dist>.cpp` —
  Rcpp wrappers calling cppunivariate distributions.

### Estimator template

```cpp
namespace cppunivariate {

enum class FooStatus { Ok, IterationFailure, BadInit };

struct FooResult {
  double param1, param2;
  double logLik;
  int iterations;
  FooStatus status;
};

inline FooResult foo_mle(span x, double tol, int maxit) {
  // cppunivariate::sum / mean / sum_log etc. from reductions.hpp;
  // cppunivariate::special::digamma / lgamma / etc. from special/special.hpp;
  // cppunivariate::newton_raphson_1d for 1D Newton or
  // cppunivariate::bfgs_minimize for multi-D.
}

}  // namespace cppunivariate
```

Match the existing files in `estimators/` for naming. The 1D Newton
helpers in `core/newton.hpp` come in `_rel` and `_abs` step variants
— pick what the original R algorithm used so goldens stay
reproducible.

### Multi-D optimiser

Hand-rolled BFGS in `cppunivariate/include/cppunivariate/core/bfgs.hpp`: header-only quasi-Newton
with backtracking Armijo line search and the standard two-rank update
on the inverse-Hessian approximation.

Cost concept (compile-time):

```cpp
int    dim() const;
double operator()(const double *theta) const;
void   gradient(const double *theta, double *g) const;
```

If you only have an objective, wrap in `cppunivariate::finite_diff_cost<Cost>`
from `core/finite_diff.hpp`; that adapter supplies a central-difference
gradient. The six fGarch-family estimators use this path.

Reparametrisation lives in `core/reparam.hpp`: `from_log` / `to_log`
and `from_log_shift` / `to_log_shift` (e.g. `nu > 2` for Student-t).

`BfgsControl` exposes three convergence rules; **any one** fires:

- `||g||_2 < grad_tol` (default `1e-6`)
- `||theta_new − theta||_inf < step_tol * (||theta||_inf + step_tol)`
  (default `1e-8`)
- `(f_old − f_new) < reltol * (|f_old| + reltol)` (default `1e-12`)

The reltol default is intentionally tight: at `kFitTol = 1.22e-4` the
f-relative criterion fires on the asymptotic flat approach to an MLE
long before the gradient is small. Estimators that need a score
gate map their user-facing `tol` onto `grad_tol`, not `reltol`.

#### Score-residual gate

Each multi-D estimator's doctest pairs the golden check with a
`finite_diff_gradient` evaluation at the recovered MLE:

```cpp
CHECK( ||g||_2 < 1e-3 )
```

This catches the "fake-fit" class of bug — a parameter implicitly
fixed during fitting will leave a non-zero score component there,
regardless of what optimiser produced the result. Independent of
optimiser trajectory; survives golden regeneration.

#### Wrapper failure handling

Match `nlminb`'s legacy behaviour: only `Rcpp::stop` on truly invalid
output (NaN in any returned parameter) or `InvalidStart` (data isn't
fittable). On plain BFGS iteration-limit hits, return whatever
`theta_hat` the optimiser landed on — downstream consumers running on
the legacy fGarch path expect a usable result back, not an error.

### Adding a new distribution

Every distribution lives in a dossier bundle at
`cppunivariate/include/cppunivariate/distributions/<id>/`. The legacy
flat-file layout (separate `estimators/` and `distributions/<id>.hpp`)
was retired in the dossier sweep; legacy `metadata/models.json` and
`metadata/docs.json` are intentionally empty.

1. Create the dossier folder `cppunivariate/include/cppunivariate/distributions/<id>/`.
   The folder name **is** the distribution id.
   - `mle.hpp` — `cppunivariate::*_mle` and the `Cost` struct.
     Relative includes are two levels up: `../../core/...`,
     `../../special/...`.
   - `dpqr.hpp` — d/p/q/r templates. Always present, even when the
     R-side wrapper uses base R's dpqr (see exp); the C++ artifact
     stays self-contained for downstream consumers.
   - `metadata.hpp` — `constexpr` distribution metadata using the
     shared types from `core/metadata.hpp`. Format is constrained
     (one entry per ParamDescriptor row, `kInf` alias for infinity)
     so the regex parser in `r/scripts/parse_cpp_metadata.R` can
     read it; see `metadata/SCHEMA.md` for the full schema and
     existing dossiers (`distributions/lomax`, `distributions/exp`)
     for examples.
   - `dossier.md` — human-readable docs. Today a bare stub
     (`# <Name>` + empty section headers); filled later by a
     dedicated docs pass. Don't pre-populate from existing comments
     or anywhere else.
2. Add one include line to `cppunivariate/include/cppunivariate/cppunivariate.hpp`:
   `#include "distributions/<id>/mle.hpp"`. The aggregator only pulls
   estimators; `dpqr.hpp` and `metadata.hpp` are included on demand
   by their consumers.
3. Add `r/metadata/<id>.json` with the R-side refs:
   `{"sampler": "...", "density": "..."}`. Use `pkg::fn` for base R
   or other packages (e.g. `stats::rexp`); use a bare name for an
   Rcpp wrapper exported by univariateML itself (e.g. `rlomax`).
4. Wire an Rcpp wrapper in `r/src/ml_exports.cpp`. If the C++ dpqr
   is exposed to R, add `r/src/distributions/<id>.cpp` with
   `#include "cppunivariate/distributions/<id>/dpqr.hpp"`. If R is
   happy with base R's dpqr (as for exp), no wrapper file is
   needed; goldens use the `stats::*` reference from the JSON.
5. Regenerate goldens via `just goldens`.
6. Add a doctest case in `cppunivariate/tests/test_estimators.cpp`.
   For multi-D estimators add the score-residual `||g||_2 < 1e-3`
   check too.
7. `just test` and `just cpp-test` must both pass.

The codegen scripts read two sources and merge them, sorted
alphabetically by id:

- per-distribution `metadata.hpp` files parsed via
  `r/scripts/parse_cpp_metadata.R` (the C++ canonical form),
- per-distribution `r/metadata/<id>.json` sidecars (R-side refs only).

`metadata/models.json` and `metadata/docs.json` are kept as empty
stubs — the scripts still read them, but every entry should be a
dossier today. A given id must appear in exactly one source.

---

## Math backend

Boost.Math via the `BH` CRAN package; falls back to system Boost for
the standalone CMake build. The seam is `cppunivariate/include/cppunivariate/special/special.hpp`
— estimators never include `<boost/...>` directly. Swapping backend
(Rmath, Cephes, hand-roll) is a single-file change at that seam,
followed by a goldens regen + visual diff.

What's in Boost.Math: every common univariate distribution
(normal, gamma, beta, t, Weibull, …) plus the special functions
(digamma, trigamma, lgamma, lbeta). What we layer on top in
`cppunivariate/include/cppunivariate/distributions/` and `cppunivariate/include/cppunivariate/special/skewed_densities.hpp`:
the Fernandez–Steel skew-normal/t/GED family, fatigue, log-logistic,
nakagami, logit-normal, zipf, zip, paralogis — closed-form
compositions over Boost primitives.

Boost.Math licence (BSL-1.0) is permissive; no attribution-in-binary
requirement.
