# Monorepo layout (deferred)

**Status:** deferred — future layout, not a near-term file move.

## The idea

Move toward a repo layout where the C++ library is primary and language
packages are generated shells.

## Why it matters

The current repo still looks like an R package, even though the C++ core is
becoming the real source artifact. A future layout should make that explicit
without disrupting current development too early.

## Design when revisited

Possible layout:

```text
cppunivariate/
  include/
  tests/
  distributions/
r/
  package/
  scripts/
python/
  package/
quality/
  golden/
  simulations/
  references/
docs/
  generated/
  notes/
roadmap/
```

The exact names can change. The important principle is that C++ and
distribution dossiers are central, while R and Python are downstream products.

## Trigger to revisit

Take this off the shelf after metadata, dossiers, quality status, and
generation rules are stable enough that a file move will not hide design
uncertainty.

## Verification when revisited

- The R package remains buildable throughout the transition.
- Tests pass after each move.
- New metadata and validation assets survive the layout change without another
  redesign.
