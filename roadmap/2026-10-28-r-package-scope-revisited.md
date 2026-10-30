# R package scope revisited: ML-only, or one package with d/p/q/r

**Status:** partly decided 2026-10-29, see "Decision 2026-10-29" at the
end; the rest below is the 2026-10-28 state. Deferred (2026-10-28). The R package stays ML-only, as decided
in `2026-05-08-r-package-scope.md`: it exports fitters, `dml`/`pml`/`qml`/
`rml`, model selection, bootstrap and plots, and every model's d/p/q/r
comes from an external package (stats, extraDistr, actuar, fGarch,
logitnorm, nakagami, sads). The author strongly prefers to keep it that
way; the only alternative he would accept is a single merged package later.

## Why this came up

`mlhnorm` (translated half-normal, decision D7) has no external density
with a location argument of that name. It is `extraDistr::dtnorm` with
`a = mean`, so it is covered by mapping the coefficients onto the external
function's arguments (`r.args` in `README.md`) rather than by defining
d/p/q/r in this package. Any future model in the same situation should use
an argument map too.

## The open question

| Option | What | Costs | Gains |
|---|---|---|---|
| A. ML-only (now) | d/p/q/r from other packages; argument maps where names differ | depends on upstreams; the extraDistr maintainer is currently unresponsive, so fixes and new families cannot go upstream and an archival would break univariateML | small API and docs; no parameterisation debates |
| B. One package | expose cppunivariate's `dpqr.hpp` (exists for every distribution) as R d/p/q/r, drop the upstream Imports | ~4 functions and man pages per family; must match the parameterisations users know; larger CRAN surface | no upstream risk; one consistent parameterisation; the C++ d/p/q/r get exercised from R |
| C. Companion package | d/p/q/r from the same C++ core in a second R package | two packages to release | keeps univariateML ML-only while removing the upstream risk |

## Trigger to revisit

- extraDistr (or another upstream) is archived or breaks univariateML;
- a model needs a density that no package covers even with an argument map;
- preparing a CRAN release, or starting the Python shell (which will
  expose the C++ d/p/q/r anyway).

## Verification when decided

The chosen option is written into CLAUDE.md ("Vision", R shell) and the
README schema; no package-defined d/p/q/r exist unless option B or C is
chosen.

## Decision 2026-10-29

- Every family whose reference is extraDistr or actuar gets the package's
  own d/p/q/r from the C++ core (`dpqr.hpp`), checked against independent
  high-precision truth tables; extraDistr and actuar leave Imports
  (board TASK-78 and subtasks).
- The new R d/p/q/r are **internal**: `dml`/`pml`/`qml`/`rml` reach them,
  the namespace does not grow. This is not option B or C above.
- **2026-10-30: no densities are exported** (Jonas: "Dont export any
  densities"), including the 18 families that had exported d/p/q/r before
  any release. The "extraDistr++" option is closed for this package; a
  companion package (option C) remains possible later.
- The 14 families on `stats` stay on `stats` for now. **Not banked**: this
  can change with the project's direction (an exported density layer, the
  Python shell, or one parametrization source for every family).
- Test oracle: truth tables computed with mpmath from each README's
  `log_density`, not the former reference packages.
