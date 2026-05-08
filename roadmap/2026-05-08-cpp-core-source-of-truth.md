# C++ core as source of truth (deferred)

**Status:** deferred — strategic direction, not today's code change.

## The idea

Make the portable C++ library the canonical implementation of the project.
The R package remains the current published shell, and a future Python package
should follow the same thin-wrapper pattern.

The core should stay local and copyable: simple headers, plain result structs,
explicit status enums, and little framework machinery.

## Why it matters

The project has too many distributions for several language-specific
implementations to stay correct by hand. A single C++ core gives one place for
estimators, distribution functions, numerical policy, and validation.

## Design when revisited

- Treat `src/ml/` as the seed of the future C++ library.
- Keep estimator and distribution logic local to each distribution where
  possible.
- Keep Rcpp and package-specific behavior outside the C++ core.
- Generate R and future Python bindings from shared metadata.
- Prefer explicit functions and structs over class hierarchies or runtime
  registries.

## Trigger to revisit

Take this off the shelf when a new estimator is added, when Python bindings
start, or when the repo layout begins moving away from an R-package-first
shape.

## Verification when revisited

- C++ tests exercise the implementation without loading R.
- R wrappers continue to match C++ goldens.
- New code can be understood distribution-by-distribution without following a
  global object system.
