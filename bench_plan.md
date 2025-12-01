Benchmarking plan for competing C++ estimators

- Goal: compare alternative Rcpp implementations named `ml*_rcpp[_X]` (suffix `_X` identifies variants). Defaults use `tol = eps^(1/4)` (~1.22e-4) and `max_iter = 100`.
- Harness (in R, e.g. `scripts/bench_cpp.R`):
  - Input: list of candidate function names, data generators (n grid, seeds, distributions), and a reference choice (e.g., the default implementation with tighter tol if needed).
  - Use `bench::mark` to time candidates; capture outputs (estimates, iterations, logLik if returned), and any warnings/errors.
  - Compare accuracy to the reference (param deltas, logLik delta); record failures.
  - Output tidy tibble plus CSV/JSON in `bench_results/`.
- Config: `metadata/benchmarks.json` describing per-family suites (data setups, candidate names, metrics to check).
- Reporting: optional `bench_results/<family>.md` summarizing timing, accuracy, and failure counts.
- Usage pattern: add a new variant `mlburr_rcpp_fast`, list it in config, run the bench script; results stay outside the package build (ignored via .Rbuildignore).
