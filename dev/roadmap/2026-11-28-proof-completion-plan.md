# Proof completion plan (2026-11-28)

Goal: every base existence-atlas claim (520 regimes × criterion, failure,
uniqueness) backed by a reviewed, full Lean proof. Modifier claims
(truncation, zero modifiers) are parked after the jobs already running.

## Where the base atlas stands

Every one of the 1,560 base claims has a reviewed written proof. Full Lean
coverage, from `atlas.csv` after the review atlas-coverage9 (TASK-505):

| Question | Full Lean | Written only | of which almost-everywhere |
| --- | ---: | ---: | ---: |
| Criterion | 468 | 52 | 0 |
| Failure | 466 | 54 | 0 |
| Uniqueness | 386 | 134 | 115 |

Steps 1 to 5 are closed: the 125 written-only claims outside almost-everywhere
uniqueness rest on their reviewed written proofs, and the missing Lean step for
each is recorded in the closing tables of the reviews
[atlas-coverage7](../research/evaluations/2026-11-28-atlas-coverage7-report.md#closing-record-for-families-with-both-passes),
[atlas-coverage8](../research/evaluations/2026-11-28-atlas-coverage8-report.md#closing-record-for-families-with-both-passes)
and [atlas-coverage9](../research/evaluations/2026-11-28-atlas-coverage9-report.md),
and for step-5 families in the earlier reviews of their rounds.

Step 6 (almost-everywhere uniqueness): lean-ae5 (TASK-504) proved the shared
fixed-count null-tie theorem (`Generic/AE5Weighted.lean`), with face and contact
competitors taken as off-null separation hypotheses, and three pilot families.
Its [classification](../research/evaluations/2026-11-28-lean-ae5-report.md#atlas-inventory)
sorts the 115 claims into seven proof routes with the family inputs each still
needs.

## Order of work

Each step is one Codex Lean job followed by one independent coverage review.
Targets exclude almost-everywhere uniqueness. Lean effort per family is bounded
(decision of 2026-11-28): one broad pass, then at most one deep pass. Claims still
open after that rest on their reviewed written proof; the job report records the
missing Lean step.

| Step | Families | Claims | Card |
| --- | --- | ---: | --- |
| 1 | hyper, nhyper, gpd, triang, pareto2, pareto3, lomax, invpareto, binom, dgamma, skellam | 31 | TASK-489 (first pass merged, about half complete); TASK-491 (second pass merged; done: 5 claims stay written-only) |
| 2 | bbinom, cmb, bnbinom, cmp, pig, nbinom | 46 | TASK-490 (first pass merged, 19 complete); TASK-494 (second pass merged; done: 12 claims stay written-only) |
| 3 | positive-shape families: genbeta, gb2, betapr, fpareto, pgw, expweibull, fatigue, frechet, trgamma, invtrgamma, invburr, nsbeta, gev | 53 | TASK-493 (broad pass merged, 4 complete); TASK-495 (gev, frechet, expweibull: all 7 proved); TASK-496 (genbeta, nsbeta: 5 of 15 closed); TASK-497 (gb2, fpareto, trgamma, invtrgamma: building blocks only, no claim closed); TASK-499 (betapr, invburr: all targets, invburr via certificate hypotheses); TASK-500 (pgw, fatigue: all 6 proved) |
| 4 | tlambda; huber, bhatt, emg (deep passes only) | 30 | TASK-501 (tlambda: 6 of 13 closed); TASK-502 (huber, bhatt, emg: all 17 targets addressed) |
| 5 | std, lst, st, sstd, sged, snorm, sn, ged | — | done: both passes already ran (lean-locscale, lean-skewls, lean-twopiece, lean-student, then lean-twopiece2, lean-student2, lean-ged); their written-only claims stand |
| 6 | almost-everywhere uniqueness, all families | 115 | TASK-504 (classification, shared theorem, pilot lomax/invpareto/pareto2; rows awaiting review) |

Steps 4 and 5 were reduced on 2026-11-28: the families in step 5 have already had a
broad round and a dedicated second round, so under the bounded-effort rule their
remaining claims rest on the written proofs. The step-4 families had only broad
rounds and get one narrow deep pass each.

Step 3 showed that broad passes stall on these families: the missing piece is
almost always escape exhaustion (upper envelopes or compact superlevel sets in the
joint parameter chart), a separate analytic proof per family. From step 3 on, jobs
take one to three related families at a time.

## Modifier atlas (parked)

Parked on 2026-11-28 after lean-trunc5 (TASK-487), lean-zero6 (TASK-488) and
their review atlas-modcov4 (TASK-492). Full / partial-only Lean claims across the
seven modifiers: criterion 34 / 120, failure 9 / 62, uniqueness 34 / 49. Written
proofs cover about 417 of 520 criteria per truncation modifier. What remains
open, by modifier and family group, is the closing table of the
[atlas-modcov4 report](../research/evaluations/2026-11-28-atlas-modcov4-report.md#open-lean-coverage-by-modifier-and-family-group).
