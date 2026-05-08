# Quality ladder (deferred)

**Status:** deferred — quality taxonomy for later validation work.

## The idea

Give every distribution an explicit quality status. The status should summarize
how much trust the implementation has earned and what work remains.

## Why it matters

The project is not large by code volume, but it is large by number of
distributions. Without a visible quality ladder, it is too easy to forget which
estimators have only been ported, which have been mathematically checked, and
which are safe to treat as stable.

## Design when revisited

Proposed statuses:

- `experimental`: implementation exists, but only basic behavior is checked.
- `ported`: implementation reproduces an earlier accepted behavior or golden
  baseline.
- `validated`: stronger numerical checks pass, such as score, Hessian,
  simulation recovery, and reference comparisons.
- `audited`: math notes, regularity, boundary behavior, references, and docs
  have been reviewed.
- `stable`: API and numerical behavior are intended to remain durable.

Each distribution should eventually track support validation, `d/p/q/r` tests,
golden regression, score checks, Hessian checks, simulation recovery, edge
cases, reference comparisons, and documentation review.

## Trigger to revisit

Take this off the shelf when distribution dossiers exist for a pilot set, or
when the number of estimator changes makes it hard to remember which ones are
trusted.

## Verification when revisited

- A generated dashboard lists every distribution and quality status.
- Current distributions are marked conservatively.
- Status changes require evidence from tests, docs, or review notes.
