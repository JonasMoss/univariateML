# Distribution dossiers (in progress — pilot landed)

**Status (2026-05-08):** Lomax + Exponential pilots landed; the
remaining 39 distributions are still legacy. Codegen merges
`metadata/models.json` and any per-distribution `metadata.json`
inside `cppunivariate/include/cppunivariate/distributions/<id>/`,
so the two pilots and the 39 legacy distributions co-exist
transparently. The remaining sweep is the trigger to retire this
entry.

**Original status:** deferred — foundation idea for future quality work.

## The idea

Create one source-of-truth dossier per distribution. A dossier collects the
math notes, implementation notes, metadata, tests, references, and generated
documentation inputs needed to maintain that distribution.

## Why it matters

The hard part of this project is remembering what is true for many
distributions: parameterizations, limiting cases, existence conditions,
regularity, references, and implementation history.

A dossier makes each distribution locally understandable and easier for humans
or coding agents to modify safely.

## Design when revisited

Each dossier should eventually cover:

- support and parameterization
- parameter constraints and aliases
- density/CDF/quantile/RNG reference behavior
- likelihood, score, Hessian, and Fisher information where known
- MLE existence conditions and boundary behavior
- regular and non-regular parameters
- implementation algorithm and optimizer settings
- implementation provenance
- test scenarios, goldens, and validation status
- generated documentation text and references

Use Lomax as the first serious pilot. It has the right kind of details:
dispersion condition, exponential limiting case, Newton iteration behavior, and
user-facing failure behavior. Add one simple regular distribution, such as
exponential or normal, so the format does not overfit to unusual cases.

## Trigger to revisit

Take this off the shelf before adding a substantial new distribution, before
rewriting generated docs, or before building a quality dashboard.

## Verification when revisited

- Lomax and one simple distribution have complete pilot dossiers.
- Existing R docs can be generated or cross-linked from dossier fields.
- The pilot distinguishes structured metadata from longer prose notes.
