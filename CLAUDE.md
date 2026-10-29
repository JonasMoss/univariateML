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
   R wrappers and their docs are generated (`just gen`) from each
   distribution's `README.md`; they call thin Rcpp adapters
   (`distributions/<id>/adapter.cpp`, copied into the gitignored
   `univariateML/package/src/` by `just assemble`). testthat acts as the
   trusted reference: every C++ change is sanity-checked against R-side
   fits, slow oracle fits, and the goldens in `distributions/<id>/golden.json`.

3. **Python shell (planned).** Same shape: pybind11 / nanobind
   adapters around cppunivariate, reading the same `README.md` front matter;
   the same goldens cross-validate across shells.

### Status (October 2026)

- 98 distributions, **one folder each** in
  `cppunivariate/include/cppunivariate/distributions/<id>/`: `README.md`
  (front matter + prose), `mle.hpp`, `dpqr.hpp`, `adapter.cpp`, `test.cpp`,
  `test.R`, `golden.json`. See "Adding a new distribution" below. `lgamma`
  has no `mle.hpp`; its adapter fits gamma on `log(x)`.
- Fitter behaviour at the edges (escapes to limit models, boundaries,
  non-existence) is studied in `notes/ml-behaviour.tex`, with
  `notes/ml-behaviour-sweep.R` (all fitters vs a slow oracle, ~6 s) and two
  research reports. Escapes that a fitter detects stop with "use `mlX`"
  (decision D1) and are declared under `limits:` in its `README.md`.
- The R package is ML-first. By default each model's
  `dml`/`pml`/`qml`/`rml` call an external package's functions
  (`r.density` in `README.md`), with an optional `r.args` map when their
  argument names differ from the coefficients (hnorm uses
  `extraDistr::dtnorm` with `a = mu`). Where references are broken, heavy
  or missing (decided 2026-10-29: Zipf, the Conway–Maxwell families, the
  former fGarch, logitnorm and nakagami models, and families that only
  VGAM had), the package defines its own d/p/q/r: `distributions/<id>/dpqr.R`
  (exported, documented) over Rcpp exports of `dpqr.hpp`, shared helpers in
  `univariateML/R/dpqr_helpers.R` and `univariateML/src/dpqr.h`, and
  `r.density` without a package prefix (`README_SCHEMA.md`). Known defects in those packages,
  and the runtime-checked workarounds (`univariateML_reference_fixes`),
  are listed in `KNOWN_ISSUES.md`. A reference without a quantile
  function (extraDistr's beta-binomial) gets one by integer inversion of its
  own CDF (`univariateML_reference_quantiles`).
- Boost.Math only through `special/special.hpp`, with a no-throw policy.
- Hand-rolled BFGS + finite-difference gradient + reparam helpers in
  `core/`; paralogis, std, sstd, ged, sged use this path (snorm is an
  exact one-dimensional profile fit).
- `LinkingTo: Rcpp, BH` — no `RcppArmadillo`.
- Tests: C++ ctest targets `distribution_tests`, `bfgs_tests`,
  `sample_tests`, `no_throw_tests`; R testthat includes golden regression.
- Validation harness (`just quality [id ...]`): reads README symbolic
  densities and real fitter goldens; checks logLik, regular score and
  observed information, and an eight-start scipy oracle. Outputs are
  ignored under `build/quality/`. Limits are listed, not executed;
  per-distribution tests check their statuses.

Open items and current test status: `STATUS.md`.

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
ancestry but are diverging — what lives here (C++ core, per-distribution folders,
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
  `*Status` enums in the result struct. Boost.Math is called with the
  no-throw policy in `special/special.hpp`. `Rcpp::stop` is fine in the
  R-shell adapters (`distributions/<id>/adapter.cpp`: they sit in the
  folder but are R code, never part of the C++ library; header sync and
  install copy `*.hpp` only), never in a header.
- **No `using` declarations.** Always be explicit
  (`std::vector<double>`, `cppunivariate::span`, etc.).
- **No raw pointers.** cppunivariate takes `cppunivariate::span`; R-shell adapters
  use Rcpp types.
- **No printing or logging in cppunivariate.** Status enums only.
- **No Armadillo anywhere.** R-shell adapters use
  `Rcpp::NumericVector` / `Rcpp::IntegerVector`.
- **C++17 features OK; avoid C++20 syntax.** `cppunivariate::span` is the
  C++17 substitute for `std::span<const double>`.
- **Uniform parameter names** in C++: `tol` for tolerance, `maxit` for
  iteration limits, across every estimator. R-facing adapter arguments use
  the CRAN-compatible names in `roadmap/2026-10-28-cran-argument-names.md`
  (`reltol`, `abstol`, `gradtol`, `iterlim`, `<param>0`).

### Layout

```
cppunivariate/                        # the C++ artifact
  CMakeLists.txt                      # produces INTERFACE target `cppunivariate`
  include/cppunivariate/
    core/                             # span, optimisers (newton, bfgs), reductions,
                                      # finite_diff, reparam
    distributions/<id>/               # everything about one distribution:
      README.md                       #   front matter + prose (schema: ../README_SCHEMA.md)
      mle.hpp                         #   *_mle(span x, ...) -> *Result; Cost only if used by fitter
      dpqr.hpp                        #   d/p/q/r templates
      adapter.cpp                     #   Rcpp adapter ml<id>_ (R shell code)
      test.cpp                        #   doctest cases -> distribution_tests
      test.R                          #   testthat file
      golden.json                     #   golden data + estimates
    distributions/rng_std.hpp         # shared sampling infrastructure (not per-dist)
    special/                          # special.hpp (Boost.Math seam), skewed_densities.hpp
    cppunivariate.hpp                 # aggregator (includes every <id>/mle.hpp)
  tests/                              # shared doctest targets + helpers, R-free
univariateML/                         # R package sources (R/, src/as_span.h, tests/,
                                      # scripts/ for assembly, codegen, goldens);
                                      # package/ is generated and gitignored
quality/                              # validation harness (experimental)
notes/                                # LaTeX research notes (just note-build)
```

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

Match the existing `distributions/<id>/mle.hpp` files for naming. The 1D Newton helpers in `core/newton.hpp` come in `_rel` and
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

Iterative failure is an error (rule R6,
`roadmap/2026-10-29-parameter-rules.md`).
- Preserve failure statuses through transformations and delegates, and
  stop in the adapter with the status and a clear diagnostic.
- Keep a failed solve only when its finite free-coordinate score has
  Euclidean norm below 1e-3 in fitter coordinates, or passes the
  family's documented score gate. Use the total likelihood score, with
  any objective normalization undone.
- Invalid starts and nonfinite results stop.
- A rescue solve must meet its own convergence or score criterion; never
  return an unchecked last iterate.

This replaces the nlminb-style rule that returned whatever BFGS landed
on, which let `mlstd` return its start values (TASK-77).

### Adding a new distribution

**One folder per distribution** (`roadmap/2026-10-28-per-distribution-layout.md`).
Everything about distribution `<id>` lives in
`cppunivariate/include/cppunivariate/distributions/<id>/`; nothing shared
lists distributions by hand except the `cppunivariate.hpp` aggregator.
Follow `lomax/` as the worked example.

| file | what |
|---|---|
| `README.md` | YAML front matter (id, name, support, params, R refs, control-argument docs, log-density, limits, references, examples) + prose. Schema: `distributions/README_SCHEMA.md`. |
| `mle.hpp` | `*_mle(span x, tol, maxit, ...)`, `*Result`, `*Status`; a `Cost` struct only when the fitter uses it |
| `dpqr.hpp` | d/p/q/r templates |
| `adapter.cpp` | the Rcpp adapter `ml<id>_`; copied to the package's `src/dist_<id>.cpp` by `assemble_package.R`. Argument names follow `roadmap/2026-10-28-cran-argument-names.md` (`reltol`, `abstol`, `gradtol`, `iterlim`, `<param>0`) and must match `README.md`'s `r.control` (codegen checks). |
| `test.cpp` | doctest cases; globbed into the `distribution_tests` target; `#include "test_support.hpp"` for `load_distribution_golden(id)` and tolerances |
| `test.R` | testthat file; copied to `tests/testthat/test-dist-<id>.R` |
| `golden.json` | golden data + estimates, written by `just goldens` |

Steps for a new distribution:

1. Create the folder with the files above (copy `lomax/` and adapt), and
   add one line `#include "distributions/<id>/mle.hpp"` to
   `cppunivariate.hpp`.
2. Run `just quality <id>`.
3. `just goldens`, then `just cpp-test` and `just test` must pass.
4. `just readme-table` updates the distribution tables in the package README.

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
