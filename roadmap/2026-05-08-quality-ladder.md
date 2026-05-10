# Quality ladder (substantially addressed 2026-05-10)

**Status:** mechanism in place via the v1 harness's
`quality_report.md`; v1 emits two of the five proposed levels.
Additional levels layer on as more validation checks land.

## The idea

Give every distribution an explicit quality status. The status should summarize
how much trust the implementation has earned and what work remains.

## Why it matters

The project is not large by code volume, but it is large by number of
distributions. Without a visible quality ladder, it is too easy to forget which
estimators have only been ported, which have been mathematically checked, and
which are safe to treat as stable.

## What landed

`quality/derived/<id>/quality_report.md` carries a YAML front-matter
`claimed_level`. v1 emits one of two values, computed deterministically
from the harness output (so the claim cannot drift from the evidence —
re-running `validate` always overwrites the report).

| Level         | Required harness checks                                                       | v1? |
|---------------|--------------------------------------------------------------------------------|-----|
| `experimental`| Distribution exists, doctest in `test_estimators.cpp` passes                   | ✓   |
| `ported`      | + golden regression ≤ 1e-12 (existing)                                         | implicit, not surfaced as a level yet |
| `validated`   | + symbolic `∇`/`H` match `Cost::gradient`/`hessian` at 1e-12                   | ✓   |
| `audited`     | + ∫f=1, reference dpqr cross-check, simulation recovery, dossier.md filled    | TODO (depends on dpqr + sim-recovery roadmap entries) |
| `stable`      | API freeze marker + audited                                                    | TODO |

Today's pilot trio (`exp`, `std`, `norm`) all claim `validated`.
Adding the remaining levels is straightforward once their underlying
checks land — the report writer just needs the predicate functions
to gate on.

## Trigger to revisit (for the audited / stable levels)

Take this off the shelf when:

- Dpqr round-trip and simulation-recovery harness layers land
  (their own roadmap entries) — `audited` then has predicates to check.
- We start sweeping the 39 remaining distributions and want a
  consolidated dashboard rather than per-distribution reports.

## Verification when fully closed

- A generated dashboard lists every distribution and its
  `claimed_level`, sourced mechanically from the union of
  `quality/derived/*/quality_report.md` front-matter.
- Distributions cannot claim a level whose required checks haven't run
  (already enforced for `validated`; extend to `audited`/`stable`).
- Status changes happen only via re-running the harness, never by
  hand-editing reports.
