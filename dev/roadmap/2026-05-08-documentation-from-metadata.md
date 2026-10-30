# Documentation from metadata (deferred)

**Status:** folded into `2026-10-30-package-and-documentation.md` (catalog, generated docs). Earlier: deferred — documentation architecture for future generated docs.

## The idea

Generate user-facing documentation from the same distribution metadata and
dossiers used by the implementation and tests.

## Why it matters

Documentation for unusual distributions needs to be close to the code. Details
like Lomax's exponential limit, negative binomial's Poisson limit, or
non-regular boundary parameters are implementation contracts, not decorative
help text.

## Design when revisited

Generated documentation should include:

- distribution name and aliases
- support and parameterization
- parameter constraints
- estimator existence conditions
- algorithm summary
- failure modes and recommended alternatives
- limiting cases
- regularity and inference notes
- references
- examples

The current `metadata/docs.json` and wrapper generation scripts are the seed.
The next step is making the source richer and closer to per-distribution
dossiers.

## Trigger to revisit

Take this off the shelf when distribution dossiers are piloted, when R docs are
regenerated in a larger pass, or before adding Python documentation.

## Verification when revisited

- R documentation for pilot distributions is generated from dossier fields.
- Known non-standard behavior is present in generated docs.
- Long math notes are linked or cross-referenced without bloating help pages.
