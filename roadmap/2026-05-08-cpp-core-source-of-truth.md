# C++ core as source of truth (closed 2026-05-10)

**Status:** closed — this is now the architecture; see CLAUDE.md "Vision".

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

## What landed

- The C++ core lives at `cppunivariate/include/cppunivariate/`,
  header-only, no R headers, no Armadillo. CLAUDE.md "Vision" enshrines
  the three-layer (C++ core → R shell → Python shell) architecture and
  the C++ style guide pins the hard rules (no exceptions in cppunivariate,
  no `using`, no raw pointers, span-based input boundary).
- 42 distribution dossiers under
  `cppunivariate/include/cppunivariate/distributions/<id>/`, each
  bundling `mle.hpp` / `dpqr.hpp` / `metadata.hpp` / `dossier.md`.
- C++ tests (`cppunivariate/tests/`, doctest + ctest, 6 targets) run
  without loading R; R wrappers in `univariateML/R/` are auto-generated
  from per-distribution metadata and verified against goldens at 1e-12.
- Rcpp adapters confined to `univariateML/src/ml_exports.cpp` and
  `univariateML/src/distribution_exports.cpp`; the C++ core has no
  knowledge of them.
- "De-R-flavour cppunivariate" sweep (commit `9ce49fb`) finished the
  cleanup. Python shell is still a placeholder (`python/README.md`)
  but the C++ core is verified ready for it.

Landmark commits: `0d9e480` (dossier sweep finishing 38 distributions),
`9ce49fb` (de-R-flavour), `e04e2ca` (workspace + generated package
split), the Phase 1–4 monorepo move (`c01be1f` → `5c3d187`).
