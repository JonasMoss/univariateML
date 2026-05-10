# Implementation provenance (substantially addressed 2026-05-10)

**Status:** addressed by the v1 harness's `spec_hash`. The original
"centralised implementation record" idea is replaced by a distributed
approach: `Spec` carries identity + references + notes, git carries
dates and authorship, and `quality/derived/<id>/` carries the
validation baseline tied to a hash of the math.

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

## What landed

`Spec.spec_hash()` — SHA-256 (16 hex) of `sympy.srepr(log_density)` —
serves as the implementation provenance ID. Properties that fall out:

- **Stable across cosmetic edits.** Renaming a comment or adding a
  reference doesn't change the hash; only the math does.
- **Unstable across math equivalents.** `log(x*y)` vs `log(x) + log(y)`
  hash differently, forcing a deliberate reroll of the derived
  artifacts when the spec changes form. This is the desired behaviour:
  any apparent simplification is also a math edit.
- **Stamped into every artifact.** `eval_request.json`,
  `symbolic_grid.json`, `cpp_eval.json`, and `quality_report.md`
  carry the same hash; `validate.py` refuses to compare across
  hashes (catches stale-artifact bugs).

The original record's other fields map to existing infrastructure:

| Field (original)                    | Where it lives now                       |
|-------------------------------------|------------------------------------------|
| distribution id                     | `Spec.id` / dossier folder name          |
| function type (mle/dpqr/…)          | implicit in dossier file (`mle.hpp` / `dpqr.hpp`) |
| implementation id                   | `Spec.spec_hash()`                       |
| date / sequence number              | `git log -- cppunivariate/.../<id>/`     |
| source or prior implementation      | `Spec.notes` (free-form), git history    |
| algorithm summary                   | `mle.hpp` comments + `dossier.md`        |
| references                          | `Spec.references` (dict of pkg → callsite) |
| expected numerical tolerance        | `validate.py` constants (`*_TOL`)        |
| known behavior changes              | git log + commit messages                |
| associated validation baseline      | `quality/derived/<id>/`                  |

## What's still deferred

- A consolidated provenance dashboard (per-distribution table of
  spec hash, golden hash, recent commits touching the dossier). Lives
  in the same family as the quality-ladder dashboard — defer to that
  entry's verification step.
- "Function type" granularity for non-MLE artefacts (cdf/quantile/rng
  provenance). Today the harness focuses on MLE math; dpqr provenance
  arrives with the [dpqr-validation roadmap entry](2026-05-10-dpqr-validation.md).
