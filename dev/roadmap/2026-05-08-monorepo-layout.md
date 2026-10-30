# Monorepo layout (mostly landed)

**Status:** mostly landed — the C++-primary layout is in place; a few
subdirs (`python/package/`, `dev/validation/quality/{simulations,references}/`,
`docs/{generated,notes}/`) remain placeholders awaiting the work that
populates them.

## The idea

Move toward a repo layout where the C++ library is primary and language
packages are generated shells.

## Why it matters

The current repo still looks like an R package, even though the C++ core is
becoming the real source artifact. A future layout should make that explicit
without disrupting current development too early.

## What landed

The current top-level layout (May 2026):

```text
cppunivariate/                        ✓ C++-primary, header-only
  include/cppunivariate/{core,distributions,special}/
  tests/                              (doctest + ctest)
bindings/r/                         ✓ R shell as workspace
  package/                            (generated, ready-to-build artifact)
  R/  src/  tests/  vignettes/        (workspace sources)
  scripts/  metadata/                 (codegen + per-dist sidecars)
python/                               ⚠ placeholder README only
dev/validation/quality/                              ✓ goldens + AI-first harness
  golden/                             (per-dist JSON references at 1e-12)
  math/<id>.py                        (sympy spec per distribution)
  harness/                            (python harness — derive / validate)
  derived/<id>/                       (eval_request, symbolic_grid, cpp_eval,
                                       gradient_report, hessian_report,
                                       quality_report)
  pyproject.toml                      (sympy / scipy / mpmath deps)
docs/                                 ⚠ empty
dev/roadmap/                              ✓
```

Landmark commits: `5c3d187` (Phase 4 physical move), `e04e2ca`
(workspace + generated package split), `e7f883c` (rename `r/` →
`bindings/r/`), `747fdfa` (housekeeping finish), `d448e11`…`11c618a`
(AI-first validation harness — see
[2026-05-10-validation-harness.md](2026-05-10-validation-harness.md)).

## Still deferred

- `python/package/` — pending the actual nanobind/pybind11 binding
  work (see `python/README.md` for the design constraints captured for
  whoever picks it up).
- `dev/validation/quality/simulations/` — pending the
  [simulation-recovery roadmap entry](2026-05-10-sim-recovery.md).
- `dev/validation/quality/references/` — pending the
  [dpqr-validation roadmap entry](2026-05-10-dpqr-validation.md).
- `docs/generated/` and `docs/dev/research/` — pending the
  documentation-from-metadata work.

## Trigger to fully close

When the three deferred subdirs above have actual content driven by
their respective roadmap entries — at that point delete this entry and
the layout is fully realised.

## Verification when fully closed

- The R package remains buildable throughout (still true today).
- Tests pass after each move (still true today).
- New metadata and validation assets survive the layout change without another
  redesign.

## Updates

- 2026-05-10 — top-level `metadata/` folder removed. The empty
  `models.json` / `docs.json` stubs and `SCHEMA.md` were replaced by
  the per-distribution form alone (`metadata.hpp` +
  `bindings/r/metadata/<id>.json`). `SCHEMA.md` lives at
  `bindings/r/metadata/SCHEMA.md` now; the `UMVL_METADATA_DIR` env
  var is gone from the Justfile and the four reader scripts.
