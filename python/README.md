# cppunivariate Python bindings (planned)

This directory is a placeholder. No bindings are built today. The
sibling `cppunivariate/` library is ready to be consumed by a Python
shell whenever someone is ready to write one; this README captures the
design constraints so the next person doesn't rediscover them.

## Status

Planned, not started. The C++ core at `../cppunivariate/include/`
already meets the requirements for a Python shell:

- header-only, no R dependency (verified by `git grep` on every commit)
- portable C++17, depends only on `<cmath>` and Boost.Math
- explicit status enums on every estimator result (no exceptions)
- input boundary is `cppunivariate::span<const double>`, easy to
  construct from any contiguous numeric buffer

## Recommended binding library

**nanobind** (preferred) over **pybind11**:

- header-only, smaller binary
- faster compile times (a real win for 40+ estimators × d/p/q/r)
- C++17, matches our core
- consumes `cppunivariate/include/` directly — no intermediate copy
  step needed (unlike the R shell, which mirrors headers into
  `r/inst/include/` to satisfy CRAN policy §1.6)

pybind11 works too if nanobind is unavailable; the binding code is
nearly identical for our shape of API.

## Layout (when built)

```
python/
  pyproject.toml        # scikit-build-core or meson-python
  src/cppunivariate/
    __init__.py
    _bindings.cpp       # nanobind module: all 41 ml_* + d/p/q/r
  tests/
    test_estimators.py
    test_distributions.py
    conftest.py
```

The binding file should be one source file that includes
`cppunivariate/cppunivariate.hpp` and exposes:

- `ml_<dist>(x, **kwargs)` — wrappers around `cppunivariate::*_mle()`
  that return a typed result (NamedTuple or dataclass) mirroring the
  C++ `*Result` struct (params, logLik, iterations, status enum).
- `d<dist>`, `p<dist>`, `q<dist>`, `r<dist>` — wrappers around the
  templated d/p/q/r in `cppunivariate/distributions/`. Vectorize over
  a leading axis with NumPy.

Status enums become Python `enum.IntEnum` per estimator.

## Cross-shell validation

Both shells fit the same data to the same algorithms in the same C++
core. The shared validation surface is `quality/golden/*.json` (42
JSON files, each storing the input dataset, RNG seed, fitted
parameters, and log-likelihood).

A Python equivalent of `r/tests/testthat/test_golden_regression.R`
should:

1. Read every JSON in `<repo_root>/quality/golden/`.
2. Load the recorded `data` array.
3. Fit it via the corresponding `cppunivariate.ml_<id>()`.
4. Assert agreement with the recorded estimates and logLik to the
   same 1e-12 tolerance the R-side asserts.

Goldens are language-agnostic; the JSON schema is documented at
`metadata/SCHEMA.md` and stays the canonical contract across shells.

## What to avoid

- **Do not duplicate** the metadata schema (`metadata/models.json`) on
  the Python side. Generate any wrapper boilerplate from the same
  JSON the R-side codegen reads. See `r/scripts/codegen_*.R` for
  the pattern.
- **Do not** ship goldens inside the Python package wheel; they're
  dev-tree artefacts at `quality/golden/`. Locate them via env var
  the way the R-side and the C++ ctest harness already do
  (`UMVL_GOLDEN_DIR`).
- **Do not** add a runtime dependency on Boost. Boost.Math is a
  build-time-only header dep; the compiled bindings ship no Boost
  symbols.

## Build dependencies

- C++17 compiler
- Boost.Math headers (system Boost or vendored)
- nanobind ≥ 2.0 (or pybind11 ≥ 2.10)
- scikit-build-core or meson-python for `pyproject.toml`-driven builds

## Triggers to start

- Someone wants to fit one of these distributions from Python.
- The R shell stabilises enough that "fix on R, propagate to Python"
  is a routine workflow rather than a constant catch-up.
- The dossier / quality-ladder / provenance roadmap entries land —
  having a quality status per distribution makes a new shell's
  rollout safer.
