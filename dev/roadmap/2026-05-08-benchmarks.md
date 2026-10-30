# Benchmarks (deferred)

**Status:** deferred — useful after correctness mechanisms are stronger.

## The idea

Add benchmarks after the quality structure is clearer. Benchmarks should
measure runtime and robustness without driving estimator design prematurely.

## Why it matters

Performance matters, especially because many estimators use custom numerical
methods. But benchmarks are less urgent than correctness, provenance, and
validation. A fast wrong estimator is not useful.

## Design when revisited

Benchmark:

- runtime by sample size
- iteration counts and convergence status
- failure rates across parameter regimes
- stability on extreme but valid samples
- comparison against generic optimizers
- comparison against trusted R or Python package implementations where
  available

Keep benchmark acceptance separate from correctness acceptance.

## Trigger to revisit

Take this off the shelf after the quality ladder exists, or when a concrete
estimator change is motivated by measured runtime or convergence problems.

## Verification when revisited

- Existing benchmark scripts are either integrated or replaced intentionally.
- Benchmark scenarios are associated with distributions or implementation IDs.
- Results can be compared across implementation versions.

## Updates

- 2026-05-10 — legacy `bindings/r/scripts/bench.R` and the
  `just bench` target removed. The harness was unrunnable
  (`load_model_meta` read the empty legacy `metadata/models.json`)
  and there were no `_rcpp` / `_cpp` variants in the namespace to
  bench. Rebuild fresh from this entry's design notes when the
  first A/B variant lands.
