# Fit snapshot baseline (2026-11-12)

The first full run of [fit-snapshot.py](../../validation/fit-snapshot.py)
([guide](../../validation/README.md#fit-snapshot)), taken as the reference for
the shared-outcome refactor ([consolidation plan](../../design/consolidation.md),
steps 3 and 4; TASK-214.2). A snapshot records what every fitter returns. It
does not say that any recorded result is right.

## What was run

Master `8a702cc2` (all six jobs of 2026-11-12 merged), workstation, GCC 15.2,
`-std=gnu++17 -O2 -DNDEBUG`, four slots, niced:

```sh
python3 dev/validation/fit-snapshot.py --output build/snapshots/base-8a702cc2-native --jobs 4
python3 dev/validation/fit-snapshot.py --output build/snapshots/base-8a702cc2-double --no-long-double --jobs 4
```

Each mode took under five minutes. Stored:
[native](2026-11-12-fit-snapshot-data/native.jsonl.gz),
[double-only](2026-11-12-fit-snapshot-data/double.jsonl.gz), with
[provenance](2026-11-12-fit-snapshot-data/native.provenance.json) for each.

Cases per mode: 13,754 over the 101 fitted families (11,185 archive, 101
golden, 1,660 class, 808 resample; 27 to 571 per family). expweibull and
gengamma have no archive cases, so their hold subsets are not covered.

## Repeatability

A second native run differed in 3 of 13,754 cases: genbeta archive cases 2495,
2497 and 2499, which need about 60 CPU seconds each and so fell on either side
of the 60-second audit guard. With the guard at 300 CPU seconds (now the
default) those cases finish, and the stored native baseline is identical to
the second run in all 13,754 cases. Under the same guard the double-only run
has no timeout either; emg archive case 1311 finishes as a numerical stop.
Only genbeta (both modes) and emg (double-only) were rerun under the 300-second
guard; no case of any other family had reached 60 seconds.

The double-only baseline was run once.

## Counts

| kind | native | double-only |
|---|---:|---:|
| ok | 10,008 | 10,003 |
| limit | 1,932 | 1,930 |
| plain error (no kind) | 882 | 882 |
| numerical | 550 | 556 |
| nomle | 185 | 185 |
| unresolved | 97 | 97 |
| tie | 53 | 52 |
| budget | 31 | 31 |
| conditioning | 16 | 18 |

Observations, none of them judged by the tool:

- Of the native `ok` results, 2,827 carry `certified = 1`, 228 `certified = 0`
  and 6,953 no flag.
- Every family has plain errors without a kind (882 cases in 101 families).
- The kind depends on the precision mode in
  [12 cases](2026-11-12-fit-snapshot-data/mode-kind-differences.txt), for
  example gompertz on huge data (`ok` natively, an exponential-limit stop in
  double-only mode) and three snorm archive cases (limit or tie natively,
  numerical in double-only mode). In all, 2,872 cases differ between the modes
  in some field, mostly in the last digits of estimates.
- No `ok` result has a non-finite log likelihood or free coefficient. The 186
  tnorm records with infinite values are held endpoints `a = -Inf` or
  `b = Inf`. (The gamma subnormal case of the audit is a plain error here: the
  adapter rejects the core's result.)

## Open

- The snapshot covers the binding-neutral adapters, not the R or JS layers.
- Hold subsets are exercised only by the archive; the magnitude classes use
  the default fit.
- A baseline is tied to its inputs. After the goldens, the archive or the
  class definitions change, a new baseline is needed.
