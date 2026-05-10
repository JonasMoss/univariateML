# Implementation provenance (deferred)

**Status:** deferred — needed before implementations start changing often.

## The idea

Track explicit implementation identities for estimators and distribution
functions. The identity should say which algorithm is currently used, where it
came from, when it changed, and which validation baseline supports it.

## Why it matters

For a project with many distributions, "the implementation" is too vague. Some
estimators are closed form, some are ports from prior R code, some use custom
Newton methods, and some depend on delicate limiting or boundary behavior.

Provenance makes behavior changes auditable and makes copied code less
ambiguous.

## Design when revisited

Use IDs with a date and counter, for example:

```text
lomax_mle_20250608_1
```

An implementation record should include:

- distribution id
- function type, such as `mle`, `density`, `cdf`, `quantile`, or `rng`
- implementation id
- date and sequence number
- source or prior implementation
- algorithm summary
- references
- expected numerical tolerance
- known behavior changes
- associated golden or validation baseline

## Trigger to revisit

Take this off the shelf before accepting intentional golden changes, replacing
an estimator algorithm, or documenting copied/adapted implementation code.

## Verification when revisited

- At least two pilot distributions record their active implementation IDs.
- Intentional behavior changes include provenance updates.
- Generated docs or developer metadata expose provenance in a consistent way.
