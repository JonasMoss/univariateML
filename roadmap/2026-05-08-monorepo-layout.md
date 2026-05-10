# Monorepo layout (mostly landed)

**Status:** mostly landed — the C++-primary layout is in place; a few
subdirs (`python/package/`, `quality/{simulations,references}/`,
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
univariateML/                         ✓ R shell as workspace
  package/                            (generated, ready-to-build artifact)
  R/  src/  tests/  vignettes/        (workspace sources)
  scripts/  metadata/                 (codegen + per-dist sidecars)
python/                               ⚠ placeholder README only
quality/                              ⚠ only golden/ exists
  golden/                             (per-dist JSON references at 1e-12)
metadata/                             (legacy stubs — models.json/docs.json
                                      empty since the dossier sweep)
docs/                                 ⚠ empty
roadmap/                              ✓
```

Landmark commits: `5c3d187` (Phase 4 physical move), `e04e2ca`
(workspace + generated package split), `e7f883c` (rename `r/` →
`univariateML/`), `747fdfa` (housekeeping finish).

## Still deferred

- `python/package/` — pending the actual nanobind/pybind11 binding
  work (see `python/README.md` for the design constraints captured for
  whoever picks it up).
- `quality/simulations/` and `quality/references/` — pending the
  validation-and-goldens work; goldens live alone today.
- `docs/generated/` and `docs/notes/` — pending the
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
