# Fit snapshot of master `c167a975`: baselines with outcomes (2026-11-13)

Recorded after the outcome mapping (all 85 fitted families and the 16 modifier
routes), the removal of the 16 retiring families and the quantile-tie change.
Tool and inputs: [validation guide](../../validation/README.md#fit-snapshot).
These records say what the fitters return; they are not evidence that a
result is right.

Files: [native](2026-11-13-fit-snapshot-data/native.jsonl.gz),
[double-only](2026-11-13-fit-snapshot-data/double.jsonl.gz),
[routes, native](2026-11-13-fit-snapshot-data/routes-native.jsonl.gz),
[routes, double-only](2026-11-13-fit-snapshot-data/routes-double.jsonl.gz),
each with its `provenance.json`. 11,820 family cases and 1,934 route cases per
mode.

## Against the earlier baselines

Ignoring the new fields `outcome`, `core_status` and `outcome_override`:

| comparison | native | double-only |
|---|---:|---:|
| 85 surviving families vs [baseline of `8a702cc2`](2026-11-12-fit-snapshot-report.md) | 0 of 11,820 differ | 1 of 11,820 |
| modifier routes vs the records taken before the removal ([retire-16](2026-11-13-retire-16-report.md)) | 0 of 1,934 | 0 of 1,934 |

The one difference is EMG archive case 1311, which reaches the 300 CPU-second
guard in this run and stopped numerically in the stored baseline; rebuilding
the starting commit gives the same timeout (retire-16 report, open finding 1).
No fit changed through the outcome mapping, the removal, or the Poisson and
hypergeometric tail repairs of the quantile job.

## Outcomes (native; double-only differs by a handful of cases)

Every family and route reports an outcome on every case, and no outcome
disagrees with the kind its adapter emits.

| outcome | families | routes | emitted today as |
|---|---:|---:|---|
| estimate, certified, unique | 7,695 | 1,659 | success |
| estimate, certified, not unique | 25 | 1 | success, with no flag |
| estimate, uncertified | 610 | 17 | success, flagged where the family has the flag |
| estimate, uncertified or not unique | 52 | 1 | `tie` stop |
| limit, proved | 1,142 | 127 | `limit` stop |
| limit, decided by the margin | 665 | 0 | `limit` stop, not distinguished from a proved one |
| nonexistence (unbounded / unattained) | 147 / 35 | 3 / 0 | `nomle` stop |
| budget | 30 | 1 | `budget` stop |
| failure, classed | 600 | 62 | `numerical`, `tolerance`, `conditioning`, `unresolved` |
| failure, plain error | 819 | 63 | unclassed error |

Four family cases are adapter overrides of a core success (gamma, gompertz,
gumbel, weibull: TASK-214.3).

The mapping reproduces what the adapters emit today. Where a status name says
more than today's rendering, the outcome follows the rendering, and step 5 of
the [consolidation plan](../../design/consolidation.md) corrects it with a
reviewed snapshot diff. Known instances, from the four job reports
(`2026-11-13-outcome-{a,b,c,d}-report.md`):

- boundary statuses that end in a plain error and are mapped to a failure:
  sstd (normal, half-normal, half-t and df-two boundaries), sged (scale
  boundary), fpareto and gb2 (lognormal and double-Pareto boundaries), the
  generic count-window boundaries;
- `Unbounded` statuses that end in a plain error: invexp, pareto3, kumar; and
  lst `NoMLE`;
- cauchy `NoMLE` does not distinguish the unbounded collapse from the
  unattained half-mass case, and its balanced two-point continuum returns a
  success with no uniqueness field;
- no reason exists for an identically zero likelihood: rayleigh
  `ZeroLikelihood` and hyper/nhyper `Infeasible` use `Unattained`;
- several adapters decide from the data what the core result does not carry
  (constant samples turned into `nomle` after an `InvalidInput` status in
  betapr, ged, gev, invtrgamma, logis, sged, sn, tlambda, trgamma, tnorm).

## Strict layer

`fit-judge.py` on these four files reports 21 failures, all on the known list
(`dev/validation/strict-known.json`): 12 precision-mode differences, 6 nesting
failures, 1 timeout, 2 invalid-input.
