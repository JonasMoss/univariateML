# Proof completion plan (2026-11-28)

Goal: every base existence-atlas claim (520 regimes × criterion, failure,
uniqueness) backed by a reviewed, full Lean proof. Modifier claims
(truncation, zero modifiers) are parked after the jobs already running.

## Where the base atlas stands

Every one of the 1,560 base claims has a reviewed written proof. Full Lean
coverage, from `atlas.csv` after the 2026-11-28 merges:

| Question | Full Lean | Written only |
| --- | ---: | ---: |
| Criterion | 437 | 83 |
| Failure | 416 | 104 |
| Uniqueness | 360 | 160 |

Of the 160 uniqueness claims, 118 are almost-everywhere statements ("unique
for almost every attaining sample with at least k levels"). Their Lean
versions so far are conditional implications between two putative winners and
were graded partial. They are one separate item (last below), because they need
generic-position machinery rather than per-family case work.

## Order of work

Each step is one Codex Lean job followed by one independent coverage review.
Targets exclude almost-everywhere uniqueness.

| Step | Families | Claims | Card |
| --- | --- | ---: | --- |
| 1 | hyper, nhyper, gpd, triang, pareto2, pareto3, lomax, invpareto, binom, dgamma, skellam | 31 | TASK-489 (first pass merged, about half complete); TASK-491 (second pass, running) |
| 2 | bbinom, cmb, bnbinom, cmp, pig, nbinom | 46 | TASK-490 (running) |
| 3 | positive-shape families: genbeta, gb2, betapr, fpareto, pgw, expweibull, fatigue, frechet, trgamma, invtrgamma, invburr, nsbeta, gev | 53 | TASK-493 (running) |
| 4 | location-scale and tails: tlambda, huber, bhatt, ged, snorm, sn, std, lst, st, emg | about 45 | — |
| 5 | skew families sstd, sged (criterion and failure) | about 52 | — |
| 6 | almost-everywhere uniqueness, all families | 118 | — |

Counts in steps 3 to 5 are from the 2026-11-28 atlas and will move as earlier
steps land.

## Modifier atlas (parked)

Parked on 2026-11-28 after lean-trunc5 (TASK-487), lean-zero6 (TASK-488) and
their review atlas-modcov4 (TASK-492). Full / partial-only Lean claims across the
seven modifiers: criterion 34 / 120, failure 9 / 62, uniqueness 34 / 49. Written
proofs cover about 417 of 520 criteria per truncation modifier. What remains
open, by modifier and family group, is the closing table of the
[atlas-modcov4 report](../research/evaluations/2026-11-28-atlas-modcov4-report.md#open-lean-coverage-by-modifier-and-family-group).
