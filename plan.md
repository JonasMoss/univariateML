# Plan

## Goals
- Clean the repo into a CRAN-friendly state (no stray plots, caches, compiled binaries, or scratch notebooks).
- Get back to an R CMD check-compliant package baseline.
- Set up a clear path to move estimators to Rcpp/RcppArmadillo with scripted R code generation.

## Immediate steps
1) Snapshot current state (done on branch `cleanup/initial-pass` with commit `chore: snapshot current state`).
2) Purge non-package artifacts: Rplot*.png and other plot exports, vignette cache files under `vignettes/copula_cache/`, scratch/workspace scripts, generated `out`/`show.sh`, and compiled objects like `src/*.o` and `src/univariateML.so`.
3) Tighten ignore rules for build outputs, vignette caches, workspace scratch, and generated plots.
4) Ensure sources are consistent: keep only source `.cpp`/`R` files that should ship; remove stale binaries; regenerate any required Rcpp exports cleanly.
5) Run devtools/rhub checks locally (as feasible) and fix check NOTES/WARNINGS/ERRORS to reach R CMD check compliance.

## Next-phase prep
- Add a Justfile and helper scripts to generate R wrappers from C++ (Rcpp/RcppArmadillo), so estimators share a consistent interface and can be built standalone.
- Incrementally port estimators to C++ (prioritizing the heavy ones), validating against existing R implementations and tests.
- Expand tests to cover numerical edge cases and match-stats across R/C++ implementations.

## Check/cleanup checklist (from latest R CMD check)
- Fix test failures (11): log-likelihood attribute name mismatches and missing warnings in mlgumbel, mlinvweibull, mllomax, mlweibull; determine if tests or code are at fault.
- R/utility.R has non-ASCII chars; replace with ASCII or escapes.
- Rd mismatch: `inverse_digamma.Rd` usage/doc arguments inconsistent (x vs y); align aliases/usage/docs.
- Makevars non-portable: remove `-march=native`, `-Ofast`, `-fopenmp` etc., and GNU make extensions; switch to portable defaults.
- Top-level NOTE: add `plan.md` to `.Rbuildignore` (or relocate) to silence.
- Avoid ::: self-namespace call to `newton_raphson_1d`; refactor to export/internal access cleanly.
- OpenMP macro NOTE: align SHLIB_OPENMP_* flags if keeping OpenMP, or drop OpenMP for CRAN portability.
- Compilation flags NOTE: ensure we rely on R’s defaults; avoid custom hardening flags that trigger notes.
- Future timestamp NOTE: likely harmless unless clock skew; monitor.
