# C++ Port Plan

## Goals
- Implement estimator cores in portable C++17 with Armadillo (no OpenMP; avoid vendor-specific flags).
- Keep a clean separation: pure C++ core usable standalone; thin Rcpp wrappers for R integration.
- Enforce style/quality: clang-format, no raw `void*`, prefer explicit types, no OpenMP pragmas.

## Layout Proposal
- `inst/cpp/` (or `src/core/` if preferred):
  - `include/univariate_ml/`: headers for shared types, options, helpers, and per-model fit APIs.
  - `src/`: C++ implementations per model (Armaddilo-based), shared optim/check utilities.
  - Optional standalone build files (CMake/meson) kept outside R build path for future.
- `src/`: Rcpp bindings only (auto-generated stubs calling core). No core logic here.
- `scripts/`: generators for bindings/wrappers/headers if we template them; clang-format config.

## Naming & APIs
- Core API: functions like `fit_<dist>(const arma::vec& x, const options& opt = {}) -> result`.
  - `result`: struct with `arma::vec params`, `double loglik`, `int iters`, `bool converged`, `std::string warn`.
  - `options`: reltol, iterlim, etc.
- Rcpp layer: wrappers named `ml<dist>_rcpp` returning `List(estimates, logLik, iterations, warn)`.
- R layer: auto-generated decorators/wrappers (already in place) call the Rcpp entry points once ported.

## Style Guide
- C++17, Armadillo containers; avoid raw pointers; prefer `auto` judiciously.
- Rcpp bindings should take `arma::vec` (or other Armadillo types) directly; pure cores also operate on Armadillo containers, no `NumericVector`.
- No OpenMP; guard any future parallel code with `_OPENMP` and keep defaults off.
- Prefer range-based loops and Armadillo algorithms over explicit indexing when readability/performance permit; avoid manual indexing unless necessary for speed.
- Always name iteration/count arguments `maxit`; always name tolerance argument `tol` (use for both absolute/relative checks).
- Use `std::variant` (or similar tagged types) for fallible operations; avoid naked `throw` in core code where possible.
- clang-format enforced (add `.clang-format`).
- Error handling: use Rust-like error handling with enums.
- **Never** use `using`. Always be explicit.
- Logging/prints: none in core; return warnings in `result.warn`.

## Specifics
- Use `maxit` for maximal iterations.
- Use `tol` for both absolute and relative tolerance

## Multi-D optimiser

Estimators that need a multi-parameter fit use the hand-rolled BFGS in
`src/ml/core/bfgs.hpp`. It's a header-only quasi-Newton minimiser with
backtracking Armijo line search and the standard two-rank update applied
directly to the inverse-Hessian approximation.

Cost concept (compile-time):

  int    dim() const
  double operator()(const double *theta) const
  void   gradient(const double *theta, double *g) const

If you only have an objective and not an analytic gradient, wrap in
`mlcore::finite_diff_cost<Cost>` from `src/ml/core/finite_diff.hpp`; that
adapter supplies a central-difference gradient. All six fGarch-family
estimators (paralogis, std, ged, snorm, sstd, sged) use this path.

For unconstrained reparametrisation, see `src/ml/core/reparam.hpp`:
`from_log` / `to_log` and `from_log_shift` / `to_log_shift` (e.g. nu > 2
for Student-t / sstd).

### Tolerances

`BfgsControl` exposes three convergence rules; **any one** fires:

  - `||g||_2          < grad_tol`              (default 1e-6)
  - `||theta_new - theta||_inf < step_tol * (||theta||_inf + step_tol)` (default 1e-8)
  - `(f_old - f_new)  < reltol * (|f_old| + reltol)` (default **1e-12**)

The reltol default is intentionally tight: at the standard `kFitTol = 1.22e-4`
the f-relative criterion fires on the asymptotic flat approach to an MLE long
before the gradient is actually small. Estimators that need to satisfy a
score-residual gate should map their user-facing `tol` onto `grad_tol`, not
`reltol`.

### Score-residual gate

Each multi-D estimator's doctest pairs the golden check with a
`finite_diff_gradient` evaluation at the recovered MLE:

  CHECK( ||g||_2 < 1e-3 )

This catches the "fake-fit" class of bug — a parameter implicitly fixed
during fitting will leave a non-zero score component there, regardless of
what optimiser produced the result. Independent of optimiser trajectory;
survives golden regeneration.

### Wrapper failure handling

Match `nlminb`'s behaviour: only `Rcpp::stop` on truly invalid output (NaN
in any returned parameter) or `InvalidStart` (data isn't fittable). On
plain BFGS iteration-limit hits, return whatever theta_hat the optimiser
landed on — downstream consumers that ran with the legacy fGarch path
expect to get a usable result back, not an error.

## Porting Steps
1) Set up directory structure under `inst/cpp/` (or `src/core/`) with `include/` and `src/`, add `.clang-format`.
2) Add a minimal core scaffold: `types.hpp` (result/options), `checks.hpp` (support/NA checks), `optim.hpp` (Newton-Raphson helper).
3) Add generator for Rcpp bindings (or hand-write a template) that maps each model to a `cpp` entry point.
4) Port a first model end-to-end (e.g., Gumbel or Weibull) using Armadillo core + Rcpp binding; wire R wrapper to use the Rcpp path.
5) Regenerate wrappers/R metadata as needed; ensure tests still pass.
6) Incrementally port remaining models; delete R implementations when each is validated.
7) Add Justfile targets: `gen-bindings`, `format-cpp`, `build-core` (future), and include `rcpp-attrs` when bindings change.

## Testing & Checks
- Keep existing R tests; add A/B comparisons R vs C++ outputs during migration.
- Ensure `R CMD check` stays clean (portable flags only).
- Optionally add a small C++ test harness in `inst/cpp/tests` for standalone builds later.
