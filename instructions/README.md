# instructions/

Working notes for univariateML's de-R-ification. The package's public
documentation lives in `man/`, `vignettes/`, and the repo-root
`README.md`; this directory is internal.

## Files

- `vision.md` — three-layer vision (C++ core, R shell, Python shell).
- `math_backend.md` — Boost.Math vs alternatives; current decision
  and the seam at `src/ml/special/special.hpp`.
- `cpp_instructions.md` — style guide for mlcore. Layout, estimator
  template, optimiser API, score-residual gate.
- `bench_plan.md` — design notes for a future Rcpp-variant
  benchmark harness. Forward-looking; not yet built.
- `stuff.md` — table of special functions used by `mlcore::special`
  and their roles. Reference for backend swaps.
- `vibe.md` — personal notes on AI-assisted coding workflow.

## Vendored porting references

Past iterations vendored upstream source (extraDistr, actuar, R
Mathlib) under this directory as **temporary porting reference**,
deleted in the cleanup commit once the port landed. The current tree
has no live vendored references. If a future port needs one, follow
the same pattern — see commits `db913bb` (vendor) and `cf10046` (drop)
for the extraDistr/actuar precedent, and `522f0d5` for the nmath one.
