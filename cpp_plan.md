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
