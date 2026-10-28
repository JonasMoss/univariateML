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
   Auto-generated R wrappers under `univariateML/R/` call thin Rcpp adapters in
   `univariateML/src/ml_exports.cpp` and `univariateML/src/distribution_exports.cpp`. testthat
   acts as the trusted reference: every C++ change is sanity-checked
   against R-side fits and the golden JSONs in `quality/golden/`.

3. **Python shell (planned).** Same shape: pybind11 / nanobind
   adapters around cppunivariate. The same `quality/golden/` JSONs cross-
   validate across shells.

### Status (May 2026)

- 42 distribution dossiers in `cppunivariate/include/cppunivariate/distributions/<id>/`,
  each bundling `mle.hpp` (estimator + Cost), `dpqr.hpp` (d/p/q/r templates),
  `metadata.hpp` (constexpr ParamDescriptor rows), and `dossier.md`. Header-only,
  span-based. 41 of them carry their own `mle.hpp`; `lgamma` reuses `gamma_mle`
  on log-transformed data via the R-side wrapper.
- Hand-rolled BFGS + finite-difference gradient + reparam helpers in
  `cppunivariate/include/cppunivariate/core/`; the six fGarch-family estimators (paralogis, std,
  snorm, sstd, ged, sged) use this path.
- `LinkingTo: Rcpp, BH` — no `RcppArmadillo`.
- testthat suite includes ~84 golden-regression checks at 1e-12; 7
  ctest targets (`estimator_tests`, `bfgs_tests`, `newton_md_tests`,
  `hessian_tests`, `sample_tests`, `metadata_tests`, `validation_tests`).
- AI-first validation harness (`quality/`): per-distribution sympy
  spec at `quality/math/<id>.py`; harness symbolically derives
  gradient/Hessian and asserts the C++ `Cost` matches at 1e-12 on a
  Sobol theta grid. **41 of 42 distributions validated** — every
  distribution with its own `mle.hpp` claims
  `quality_report.md::claimed_level: validated` (lgamma is the
  exception — wrapper-only on `gamma_mle(log(x))`, no Cost of its
  own). dpqr round-trip + simulation recovery deferred to dedicated
  roadmap entries.

### Out of vision (for now)

- Any dependency outside `Rcpp` + `BH` on the R side, `<cmath>` +
  Boost.Math on the C++ side.
- Bayesian inference, MCMC, regression. Univariate density
  estimation only.
- Bit-for-bit reproducibility across shells. Goldens use the C++
  core; R and Python results agree to that core's precision, not to
  scipy or fGarch.

---

## Repo policy

**Do not push to any remote — including `origin/master` — unless the
user explicitly asks.** Local commits, merges, branch creation, and
`just test` / `just cpp-test` runs are all fine and do not need
confirmation. Pushing is the gated step.

Rationale: this working tree is philosophically a separate repo from
the published `univariateML` CRAN package. The two histories share
ancestry but are diverging — what lives here (C++ core, dossiers,
monorepo layout, future Python shell) is not yet what CRAN expects.
Decisions about *when* and *what* to publish upstream are
human-judgement calls and stay with the user.

This applies to `git push`, `gh pr create`, `gh pr merge`, and any
other command that publishes local state to a remote. If you think a
push is warranted, surface the suggestion and wait for an explicit
go-ahead.

---

## C++ style guide (cppunivariate)

`cppunivariate/include/cppunivariate/` is the C++ artifact. Header-only, **C++17**, depends only on
`<cmath>` / `<algorithm>` / `<vector>` / `<cstddef>` and Boost.Math.

### Hard rules

- **No exceptions in cppunivariate.** Surface diagnostics via per-estimator
  `*Status` enums in the result struct. `Rcpp::stop` is fine in the
  R-shell adapter layer (`univariateML/src/ml_exports.cpp`,
  `univariateML/src/distribution_exports.cpp`) but never in `cppunivariate/include/cppunivariate/`.
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
    distributions/<id>/               # one folder per distribution (id matches univariateML/metadata)
      mle.hpp                         # *_mle(span x, ...) -> *Result + Cost struct
      dpqr.hpp                        # d/p/q/r templates (when present; some
                                      # distributions delegate to base R via the
                                      # R-side wrapper -- see univariateML/metadata/<id>.json)
      metadata.hpp                    # constexpr id, name, support, params[]
      dossier.md                      # human-readable docs (stub today; filled later)
    distributions/rng_std.hpp         # shared sampling infrastructure (not per-dist)
    special/                          # special.hpp (Boost.Math seam), skewed_densities.hpp
    cppunivariate.hpp                 # aggregator (includes every <id>/mle.hpp)
  tests/                              # doctest + ctest, R-free
```

R-shell adapters live outside `cppunivariate/include/cppunivariate/`:

- `univariateML/src/ml_exports.cpp` — Rcpp wrappers calling cppunivariate estimators.
- `univariateML/src/distribution_exports.cpp` + `univariateML/src/distributions/<dist>.cpp` —
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

Match the existing dossiers under `distributions/<id>/mle.hpp` for
naming. The 1D Newton helpers in `core/newton.hpp` come in `_rel` and
`_abs` step variants — pick what the original R algorithm used so
goldens stay reproducible.

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

**One folder per distribution** (`roadmap/2026-10-28-per-distribution-layout.md`).
Everything about distribution `<id>` lives in
`cppunivariate/include/cppunivariate/distributions/<id>/`; nothing shared
lists distributions by hand. Migration status: lomax and paralogis use this
layout; the others still use the legacy files (`metadata.hpp`,
`univariateML/metadata/<id>.json`, adapters in `univariateML/src/ml_exports.cpp`,
cases in `cppunivariate/tests/test_estimators.cpp`, `quality/golden/<id>.json`)
until they are migrated. Follow `lomax/` as the worked example.

| file | what |
|---|---|
| `README.md` | YAML front matter (id, name, support, params, R refs, control-argument docs, log-density, limits, references, examples) + prose. Schema: `distributions/README_SCHEMA.md`. Replaces `metadata.hpp`, the JSON sidecar and `dossier.md`. |
| `mle.hpp` | `*_mle(span x, tol, maxit, ...)`, `*Result`, `*Status`; a `Cost` struct (validation harness) |
| `dpqr.hpp` | d/p/q/r templates |
| `adapter.cpp` | the Rcpp adapter `ml<id>_`; copied to the package's `src/dist_<id>.cpp` by `assemble_package.R`. Argument names follow `roadmap/2026-10-28-cran-argument-names.md` (`reltol`, `abstol`, `gradtol`, `iterlim`, `<param>0`) and must match `README.md`'s `r.control` (codegen checks). |
| `test.cpp` | doctest cases; globbed into the `distribution_tests` target; `#include "test_support.hpp"` for `load_distribution_golden(id)` and tolerances |
| `test.R` | testthat file; copied to `tests/testthat/test-dist-<id>.R` |
| `golden.json` | golden data + estimates, written by `just goldens` |

Steps for a new distribution:

1. Create the folder with the files above (copy `lomax/` and adapt).
2. Add `quality/math/<id>.py` and register the Cost in
   `cppunivariate/tests/test_validation.cpp` (the harness still uses these;
   its rework to read `README.md` is pending).
3. `just goldens`, then `just cpp-test` and `just test` must pass.

R-side metadata, wrappers and roxygen docs are generated from `README.md`
by `univariateML/scripts/load_distributions.R`, `codegen_metadata.R` and
`codegen_wrappers.R` (`just gen`). Distributions that are not Hellinger-closed
declare their `limits`; see `notes/ml-behaviour.tex`.

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
