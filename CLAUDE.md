# univariateML — engineering notes

## Vision

univariateML is a portable C++17 + Boost.Math library for univariate
density estimation: maximum-likelihood fits and `d`/`p`/`q`/`r`
evaluation. R and (eventually) Python are thin shells around the C++
core.

### Three layers

1. **C++ core (primary).** `univariateML-cpp/include/cppunivariate/` — header-only, no Rcpp, no
   Armadillo, no R headers. `cppunivariate::span` as the input boundary;
   Boost.Math (via the `BH` CRAN package or system Boost) for special
   functions and base distributions. Owns its own tests in
   `univariateML-cpp/tests/` (doctest + ctest).

2. **R shell (testing scaffold and current published artifact).**
   R wrappers and their docs are generated (`just gen`) from each
   distribution's `README.md`; they call thin Rcpp adapters
   (`bindings/r/distributions/<id>/adapter.cpp`, copied into the gitignored
   `univariateML-r/src/` by `just assemble`). testthat acts as the
   trusted reference: every C++ change is sanity-checked against R-side
   fits, slow oracle fits, and the goldens in `distributions/<id>/golden.json`.

3. **Python shell (planned).** Same shape: pybind11 / nanobind
   adapters around cppunivariate, reading the same `README.md` front matter;
   the same goldens cross-validate across shells.

### Status (October 2026)

- 98 distributions, **one folder each** in
  `univariateML-cpp/include/cppunivariate/distributions/<id>/`: `README.md`
  (front matter + prose), `mle.hpp`, `dpqr.hpp`, `test.cpp`, `golden.json`, `truth.py`, `truth.json`. See "Adding a new distribution" below. `lgamma`
  fits gamma on `log(x)` in its core `mle.hpp`, including the Jacobian.
- Fitter behaviour at the edges (escapes to limit models, boundaries,
  non-existence) is studied in `dev/research/ml-behaviour.tex`, with
  `dev/research/ml-behaviour-sweep.R` (fitters vs a slow oracle) and
  research reports. Escapes that a fitter detects stop with "use `mlX`"
  (decision D1) and are declared under `limits:` in its `README.md`.
- The R package is ML-first: it exports fitters, `dml`/`pml`/`qml`/`rml`,
  model selection, bootstrap and plots, and **no d/p/q/r** (decided
  2026-10-30). `dml` and friends call the function named by `r.density` in
  `README.md`. The 14 stats families use `stats` (kept for now, not banked:
  `dev/roadmap/2026-10-28-r-package-scope-revisited.md`); every other family
  uses the package's own internal d/p/q/r, generated at assembly by
  `bindings/r/scripts/codegen_dpqr.R` from `dpqr.hpp` (README
  coefficients in order, no defaults; entrypoints appended to the family's
  `src/dist_<id>.cpp`, library headers first because `Rmath.h` #defines
  names such as `pnorm`). `r.shift` and `r.args` remain resolver features.
  extraDistr and actuar are no longer dependencies (TASK-78); defects found
  in them are recorded in `dev/KNOWN_ISSUES.md` and `dev/research/upstream/`.
- Every `dpqr.hpp` is tail-accurate (`special/tails.hpp` states the rules)
  and checked against an mpmath truth table, `distributions/<id>/truth.json`
  (`just dpqr-truth <id>`; route in the family's `truth.py`), from C++
  (`mltest::check_dpqr_truth`) and R (`test-generated-dpqr.R`). Truth tables
  cover interior parameters; boundary values a fitter can return need
  closed-form cases in `test.cpp`.
- Boost.Math only through `special/special.hpp`, with a no-throw policy.
- Hand-rolled BFGS + finite-difference gradient + reparam helpers in
  `core/`; paralogis, std, sstd, ged, sged use this path (snorm is an
  exact one-dimensional profile fit).
- `LinkingTo: Rcpp, BH` — no `RcppArmadillo`.
- Tests: C++ ctest targets `distribution_tests`, `bfgs_tests`,
  `sample_tests`, `no_throw_tests`, `cmp_tests`, `tails_tests`; R testthat includes golden regression.
- Validation harness (`just quality [id ...]`): reads README symbolic
  densities and real fitter goldens; checks logLik, regular score and
  observed information, and an eight-start scipy oracle. Outputs are
  ignored under `build/quality/`. Limits are listed, not executed;
  per-distribution tests check their statuses.

Open items and current test status: `dev/STATUS.md`.
Validation commands, evidence and gaps: `dev/validation/README.md`.

### Out of vision (for now)

- Any dependency outside `Rcpp` + `BH` on the R side, `<cmath>` +
  Boost.Math on the C++ side.
- Bayesian inference, MCMC, regression. Univariate density
  estimation only.
- Families whose parameter vector has a user-chosen length (categorical,
  finite mixtures such as `dmixpois`, `dmixnorm`); dropped 2026-10-30.
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

`univariateML-cpp/include/cppunivariate/` is the C++ artifact. Header-only, **C++17**, depends only on
`<cmath>` / `<algorithm>` / `<vector>` / `<cstddef>` and Boost.Math.

### Hard rules

- **No exceptions in cppunivariate.** Surface diagnostics via per-estimator
  `*Status` enums in the result struct. Boost.Math is called with the
  no-throw policy in `special/special.hpp`. `Rcpp::stop` is fine in the
  R-shell adapters (`bindings/r/distributions/<id>/adapter.cpp`: they sit in the
  binding tree and are R code, never part of the C++ library; header sync and
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
  the CRAN-compatible names in `dev/roadmap/2026-10-28-cran-argument-names.md`
  (`reltol`, `abstol`, `gradtol`, `iterlim`, `<param>0`).

### Layout

```
README.md                            # landing page; LICENSE.md at root
univariateML-cpp/                    # C++17 artifact; namespace unchanged
  CMakeLists.txt                     # INTERFACE target cppunivariate
  include/cppunivariate/
    core/                            # span, optimizers and reductions
    distributions/<id>/              # canonical mathematical dossier
      README.md, mle.hpp, dpqr.hpp, test.cpp, golden.json, truth.py, truth.json
    special/                         # Boost.Math seam and numerical helpers
    cppunivariate.hpp                # public aggregator
  tests/                             # shared doctest targets and helpers
bindings/r/                          # authored R package inputs and scripts
  distributions/<id>/                # adapter.cpp, test.R and other R files
bindings/python/README.md            # planned binding
univariateML-r/                      # generated installable R package; ignored
dev/                                # development records
  STATUS.md, KNOWN_ISSUES.md
  roadmap/, research/, tools/, design/
  validation/quality/                # Python module remains named quality
```

`just regen` assembles the R package from `bindings/r/`, copies adapters and
R tests from `bindings/r/distributions/`, and copies C++ headers only from
`univariateML-cpp/include/`. Dossiers, fixtures and C++ tests stay in the core.
The `quality` and `dpqr-truth` recipes set `PYTHONPATH=dev/validation`; for
manual module use run `PYTHONPATH=dev/validation python3 -m quality ...`.
`python3 dev/tools/check_links.py` checks local Markdown link destinations.

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

Hand-rolled BFGS in `univariateML-cpp/include/cppunivariate/core/bfgs.hpp`: header-only quasi-Newton
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

Some multi-D estimators' doctests pair the golden check with a
`finite_diff_gradient` evaluation at the recovered fit:

```cpp
CHECK( ||g||_2 < 1e-3 )
```

This can catch a "fake-fit" where a parameter implicitly fixed during
fitting leaves a non-zero score component. It checks local stationarity,
not existence or global optimality, and survives golden regeneration.

#### Wrapper failure handling

Iterative failure is an error (rule R6,
`dev/roadmap/2026-10-29-parameter-rules.md`).
- Preserve failure statuses through transformations and delegates, and
  stop in the adapter with the status and a clear diagnostic.
- Keep a failed solve only when its finite free-coordinate score has
  Euclidean norm below 1e-3 in fitter coordinates, or passes the
  family's documented score gate. Use the total likelihood score, with
  any objective normalization undone.
- Invalid starts and nonfinite results stop.
- A rescue solve must meet its own convergence or score criterion; never
  return an unchecked last iterate.
- Every loop cap is a multiple of `min(iterlim, 100000)` by work unit;
  certificate and enumeration caps stop as `budget`
  (`dev/design/work-budgets.md`).

This replaces the nlminb-style rule that returned whatever BFGS landed
on, which let `mlstd` return its start values (TASK-77).

### Adding a new distribution

**One folder per distribution** (`dev/roadmap/2026-10-28-per-distribution-layout.md`).
Core definitions and evidence for distribution `<id>` live in
`univariateML-cpp/include/cppunivariate/distributions/<id>/`; R adapters and
R tests live in `bindings/r/distributions/<id>/`; nothing shared
lists distributions by hand except the `cppunivariate.hpp` aggregator.
Follow `lomax/` as the worked example.

| file | what |
|---|---|
| `README.md` | YAML front matter (id, name, support, params, R refs, control-argument docs, log-density, limits, references, examples) + prose. Schema: `distributions/README_SCHEMA.md`. |
| `mle.hpp` | `*_mle(span x, tol, maxit, ...)`, `*Result`, `*Status`; a `Cost` struct only when the fitter uses it |
| `dpqr.hpp` | d/p/q/r templates |
| `bindings/r/distributions/<id>/adapter.cpp` | the Rcpp adapter `ml<id>_`; copied to the package's `src/dist_<id>.cpp` by `assemble_package.R`. Argument names follow `dev/roadmap/2026-10-28-cran-argument-names.md` (`reltol`, `abstol`, `gradtol`, `iterlim`, `<param>0`) and must match `README.md`'s `r.control` (codegen checks). |
| `test.cpp` | doctest cases; globbed into the `distribution_tests` target; `#include "test_support.hpp"` for `load_distribution_golden(id)` and tolerances |
| `bindings/r/distributions/<id>/test.R` | testthat file; copied to `tests/testthat/test-dist-<id>.R` |
| `golden.json` | golden data + estimates, written by `just goldens` |
| `truth.py`, `truth.json` | high-precision d/p/q route and its table, written by `just dpqr-truth <id>`; checked by the truth `TEST_CASE` in `test.cpp` |

Steps for a new distribution:

1. Create the folder with the files above (copy `lomax/` and adapt), and
   add one line `#include "distributions/<id>/mle.hpp"` to
   `cppunivariate.hpp`.
2. Run `just quality <id>` and `just dpqr-truth <id>`.
3. `just goldens`, then `just cpp-test` and `just test` must pass.
4. `just readme-table` updates the distribution tables in the package README.

R-side metadata, wrappers and roxygen docs are generated from `README.md`
by `bindings/r/scripts/load_distributions.R`, `codegen_metadata.R` and
`codegen_wrappers.R` (`just gen`). Distributions that are not Hellinger-closed
declare their `limits`, including `fixed` for conditional limits; see
`dev/research/ml-behaviour.tex`. Declare coefficient `kind` and numeric `simulate`,
required-argument `advice`, structural `holds` exceptions, and R5
`fit_domain` restrictions with their mechanisms and held-set exemptions.
Use support expressions and endpoint log limits where bounds depend on
coefficients, and `r.shift` or `r.args` for reference mappings. The schema
details validation and defaults. `test_conformance.R` checks exact adapter
arguments, required diagnostics, and every holdable subset on golden data;
it is not a proof for arbitrary samples.

---

## Math backend

Boost.Math uses plain Boost headers in standalone CMake: explicit
`Boost_INCLUDE_DIR`, or CONFIG then MODULE discovery (`BOOST_ROOT` supported).
CMake never invokes R. The monorepo `just cpp-test` recipe alone can resolve
BH with Rscript as a convenience fallback and passes its plain include path. The seam is `univariateML-cpp/include/cppunivariate/special/special.hpp`
— estimators never include `<boost/...>` directly. Swapping backend
(Rmath, Cephes, hand-roll) is a single-file change at that seam,
followed by a goldens regen + visual diff.

What's in Boost.Math: every common univariate distribution
(normal, gamma, beta, t, Weibull, …) plus the special functions
(digamma, trigamma, lgamma, lbeta). What we layer on top in
`univariateML-cpp/include/cppunivariate/distributions/` and `univariateML-cpp/include/cppunivariate/special/skewed_densities.hpp`:
the Fernandez–Steel skew-normal/t/GED family, fatigue, log-logistic,
nakagami, logit-normal, zipf, zip, paralogis — closed-form
compositions over Boost primitives.

Boost.Math licence (BSL-1.0) is permissive; no attribution-in-binary
requirement.
