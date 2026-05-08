# R package scope: ML-only, Imports for distributions (deferred)

**Status:** deferred — direction, not today's code change.

## The idea

Make the R package focused: maximum likelihood estimation only, with
its `Imports:` carrying `extraDistr` / `actuar` / friends so users get
those distributions through the canonical packages instead of through
re-exports.

The C++ artifact (`cppunivariate/`) keeps the full d/p/q/r surface for
every distribution it has an MLE for — that's what downstream Python /
C++ consumers need. But on the R side, we don't have to expose it all.
Re-exporting `dlomax`, `plomax`, `qlomax`, `rlomax` etc. duplicates
what extraDistr already provides, doubles the documentation surface
(R's man-page hierarchy is flat and non-modular — every export is its
own visible help page), and dilutes the package's identity.

## Why it matters

- **Identity**: `univariateML` is an MLE package. Users coming for ML
  shouldn't have to scroll past 100+ d/p/q/r entries to find
  `mlweibull`. Other packages do dpqr; we should let them.
- **Doc surface**: every `@export`ed function gets a help page. Today
  that's ~40 ML wrappers + ~80 dpqr functions. Halving it would make
  the user-facing docs much easier to navigate.
- **Import cost**: extraDistr + actuar are mature, widely available,
  and small. Adding them to `Imports:` is cheaper than maintaining
  exported equivalents.

## Design when revisited

- Drop `@export` on the per-distribution dpqr Rcpp wrappers
  (`univariateML/src/distributions/<id>.cpp` → `univariateML/R/distributions_cpp_exports.R`).
  The C++ implementations stay (they're consumed by the C++ /
  future Python side); R-side they become an internal implementation
  detail or are removed entirely.
- Re-add `extraDistr` and `actuar` (and any other relevant package)
  to `univariateML/DESCRIPTION` `Imports:`.
- `univariateML/metadata/<id>.json` gains the freedom to point `density` /
  `sampler` at `extraDistr::dlomax` / `actuar::dburr` etc. without
  needing a univariateML-side wrapper.
- Examples in the auto-generated roxygen reference the upstream
  package (`extraDistr::rlomax(...)` etc.) instead of our own
  exports.

We don't HAVE to export everything in cppunivariate. Focus over
self-containment on the R side.

## Trigger to revisit

- A doc cleanup pass, or
- A user complaining about the man-page firehose, or
- Before the next CRAN submission.

## Verification when revisited

- The R-package NAMESPACE shrinks to ML wrappers + selected helpers
  only (no per-dist dpqr re-exports).
- DESCRIPTION's Imports lists `extraDistr` and any other dpqr-source
  packages that examples reference.
- C++ ctest still runs full d/p/q/r exercise on the `cppunivariate`
  side.
- Tarball size goes down (fewer .Rd files, fewer NAMESPACE lines).
