# Strict-layer speed-up D

2026-11-21 · TASK-214.47 · baseline `67ff1d731a35f37dad2e01bf8cc6d79d70b0681a`

On the workstation, full strict wall time fell from **2 h 46 min 39 s** on master to **1 h 11 min 13 s** with four workers and empty result caches, a **2.34×** speed-up. With warm caches it took **2 min 9 s**. All runs matched master on the failure sets, known-list verdicts, certificate comparisons, and final seam records.

## Baseline profile

Fresh snapshots from master contained 11,905 base cases and 1,950 modifier-route cases per precision mode. All four snapshots were built with GCC 15.2 and a four-slot reservation. Source and uncompressed record hashes are available for each snapshot. The strict commands ran on the same workstation. Wall time starts after the CPU reservation, and CPU time includes child processes. [Snapshot provenance](2026-11-21-strict-par-data/snapshot-inputs.csv)

| Master script | Wall s | CPU s |
| --- | ---: | ---: |
| `fit-judge.py` | 0.91 | 0.88 |
| `fit-density.py --quiet` | 40.42 | 40.31 |
| `fit-consistency.py` | 24.23 | 24.06 |
| `fit-refine.py --cache-only` | 26.29 | 26.22 |
| `cert-replay.py` | 8,717.55 | 8,716.02 |
| `strict-seam.py` | 1,189.13 | 1,190.65 |
| Full `just strict` | 9,998.59 | 9,998.20 |

The first four scripts together took 92 s, under 1% of baseline wall time. The seam breakdown was 728.30 CPU s in independent fit and density subprocesses and 446.85 CPU s compiling four probes, including the two recompiles for 300 s retries. [Native profile](2026-11-21-strict-par-data/native.seam-profile.json), [double profile](2026-11-21-strict-par-data/double.seam-profile.json), [native retries](2026-11-21-strict-par-data/native-retry.seam-profile.json), [double retries](2026-11-21-strict-par-data/double-retry.seam-profile.json)

The replay instrumentation timed each registered function across its input records. A row groups mechanisms that share a function and its numerical work. The generic `cover()` and `lomax_bnb()` kernels accounted for 6,018.24 CPU s inclusive of the work inside those calls. Their times overlap the grouped rows. [Certificate design](../../design/certificate-replay.md), [family-by-mechanism profile](2026-11-21-strict-par-data/old-mechanisms-by-family.csv), [cover kernels](2026-11-21-strict-par-data/old-cover-kernels.csv)

| Replay mechanisms on master | CPU s |
| --- | ---: |
| A · closed form | 0.00 |
| B/C · represented likelihood and scalar roots | 711.12 |
| C/D · nbinom prototype | 1.31 |
| C/E/F · Lomax prototype | 8.70 |
| D · face inequalities and exact predicates | 0.85 |
| E/F · face comparisons and global covers | 6,060.46 |
| G/H · integer searches and ties | 1,931.22 |

## Implementation

- Certificate replay and seam probes use process workers with BLAS and OpenMP pinned to one thread. The driver consumes results in the original order and applies the existing comparisons and ratchet. `just strict` defaults to four workers.
- Certificate cached results use the full snapshot record, precision mode, replay and input-reconstruction sources, compressed archive contents, `--dps`, `--budget`, and numerical-library versions. Input digests are validated on both misses and hits. The complete-result database replaces the older cover-only memoization, which could reuse cover work across different records during a run.
- Seam result keys include the probe input, archive, probe and reference sources, compiler and flags, and guard. Matching executables are reused across guard settings. Open fits and unresolved references are evaluated again. The results live in SQLite under `build/strict/`, with cache writes handled by the driver. The first four checks retain their serial execution.

## Equivalence and wall time

| Run | Workers | Replay cache hits | Certificate wall s | Seam wall s | Full wall s |
| --- | ---: | ---: | ---: | ---: | ---: |
| Master, empty cover cache | 1 | — | 8,717.55 | 1,189.13 | 9,998.59 |
| New, result caches bypassed | 1 | 0 / 13,054 | 9,484.73 | 1,413.40 | 10,984.98 |
| New, empty result caches | 4 | 0 / 13,054 | 3,715.81 | 468.94 | 4,273.14 |
| New, warm | 4 | 13,054 / 13,054 | 1.10 | 65.07 | 129.46 |
| New, warm | 1 | 13,054 / 13,054 | 1.11 | 176.67 | 241.06 |
| New, warm with Lomax payload marker | 4 | 12,846 / 13,054 | 6.21 | 34.42 | 115.50 |

The Lomax test added an ignored field to the result payload in 104 native and 104 double-only records. Exactly those 208 inputs missed. Inputs, input digests, checks, and fitted values were shared with the other runs. The whole-suite touched time was lower than the earlier warm time because the intervening serial warm run cached additional terminal seam results under the short guard. The replay column measures the effect of the payload change directly. [Timing table](2026-11-21-strict-par-data/timings.csv), [cache and guard counts](2026-11-21-strict-par-data/cache-and-guard.log)

Each full suite exited zero with the same 35 listed failures, zero new failures, and zero listed failures passing. The comparison covered all failure keys and details, all known-list classifications, 7,150 certificate output records containing 11,643 comparisons, and 19,388 seam records after guard retries. The certificate comparison included the full comparison payload, including `verdict`, `hp`, `double`, and `margin`. The recorded certificate results also matched in their original order between master and the uncached one-worker run. [Comparison hashes and record counts](2026-11-21-strict-par-data/comparison-hashes.csv), [equivalence checker](2026-11-21-strict-par-data/compare-strict.py)

Strict stdout matched apart from replay timing, cache counts, and guard-retry counts. For example, double-only archive case 2499 for `genbeta` crossed 30 s on the new uncached one-worker run. Its 300 s retry returned the same limit stop that master returned on the first attempt. Strict stderr was empty. The data directory contains the empty unified diffs. [Cold stdout diff](2026-11-21-strict-par-data/cold-stdout.diff), [warm replay diff](2026-11-21-strict-par-data/warm-replay.diff), [touched ratchet diff](2026-11-21-strict-par-data/touched-ratchet.diff)

The initial full one-worker timing predates executable reuse. After that change, the targeted `strict-seam.py native double --jobs 1 --no-cache` run completed in 639.33 wall s. Its seam records, normalized output, and ratchet matched master. The cold and warm four-worker measurements use the final code. The measurement shell encountered a syntax error while copying the initial two serial suites' seam outputs after both timed strict commands had exited zero. The original outputs were recovered from their build directories and used in the comparisons. [Final serial seam check](2026-11-21-strict-par-data/serial-seam-final.log), [final cache controls](2026-11-21-strict-par-data/cache-controls.log)

## Usage and reproduction

```sh
/tmp/umvl-cpu/run4 just strict
/tmp/umvl-cpu/run just strict NATIVE DOUBLE 1
/tmp/umvl-cpu/run4 python3 dev/validation/cert-replay.py NATIVE DOUBLE --jobs 4 --no-cache
/tmp/umvl-cpu/run4 python3 dev/validation/strict-seam.py native double --jobs 4 --no-cache
```

For the measurement, build each mode once with `/tmp/umvl-cpu/run4 python3 dev/validation/fit-snapshot.py --output build/snapshots/MODE --jobs 4`. Add `--no-long-double` for `double` and `routes-double`, and `--modifier-routes` for `routes-native` and `routes-double`. Set `TMPDIR`, `JUST_TEMPDIR`, and `XDG_RUNTIME_DIR` to the worktree's `build/tmp` and create that directory.

Copy the measurement scripts and `audit-hook/` from the data directory into `build/scratch/`. Run `bash build/scratch/prepare-master.sh` to reconstruct the baseline there with `git archive` and `git show`. With empty default result caches and a fresh baseline projection, run the `old` and `serial` variants of `measure-strict.sh` through `/tmp/umvl-cpu/run`. Run `bash build/scratch/measure-final.sh` for the four remaining variants and the equivalence checker. It handles CPU reservations itself. [Baseline setup](2026-11-21-strict-par-data/prepare-master.sh), [measurement runner](2026-11-21-strict-par-data/measure-strict.sh), [final runs](2026-11-21-strict-par-data/measure-final.sh), [validation guide](../../validation/README.md)

## Open

The ratchet can print `FIXED` for a listed certificate failure when that case produces an `OPEN` comparison. A synthetic known entry reproduced this because the ratchet receives coverage keyed by case while the list keys each comparison. Distinguishing a passing comparison from an open one requires comparison-level coverage. The tracked list contains zero certificate-replay entries. [Synthetic result](2026-11-21-strict-par-data/open-ratchet.log)

## Baseline replay CPU by family

The figures combine both precision modes and include replay calls that produced no comparison. Modifier routes retain their snapshot labels. [Family table as CSV](2026-11-21-strict-par-data/old-cpu-by-families.csv)

| Family | CPU s | Family | CPU s |
| --- | ---: | --- | ---: |
| `ht` | 2,895.194 | `kumar` | 0.902 |
| `nhyper` | 1,779.823 | `norm +truncate=tnorm` | 0.782 |
| `slash` | 1,574.978 | `invweibull` | 0.748 |
| `invparalogis` | 608.795 | `zeta` | 0.619 |
| `paralogis` | 572.188 | `dweibull` | 0.587 |
| `gpd` | 374.042 | `dnorm` | 0.560 |
| `nsbeta` | 332.449 | `binom` | 0.528 |
| `beta` | 317.369 | `pig` | 0.487 |
| `bbinom` | 77.036 | `cauchy` | 0.446 |
| `hyper` | 31.918 | `frechet` | 0.423 |
| `triang` | 22.747 | `weibull` | 0.366 |
| `cmb` | 19.180 | `gamma` | 0.336 |
| `genbeta` | 15.231 | `gumbel` | 0.289 |
| `llogis` | 15.203 | `hcauchy` | 0.159 |
| `pareto3` | 14.603 | `logitnorm` | 0.140 |
| `lomax` | 8.697 | `lnorm` | 0.093 |
| `invpareto` | 7.690 | `yulesimon` | 0.074 |
| `foldnorm` | 6.832 | `lgser` | 0.037 |
| `skellam` | 5.601 | `pois+zero=modified` | 0.032 |
| `fatigue` | 5.180 | `pois+zero=truncated` | 0.025 |
| `pareto2` | 4.694 | `gompertz` | 0.013 |
| `invgamma` | 3.407 | `lgser+zero=modified` | 0.009 |
| `naka` | 2.976 | `sgomp` | 0.008 |
| `lgamma` | 2.857 | `pois+zero=inflated` | 0.004 |
| `dgamma` | 2.766 | `dlaplace` | 0.001 |
| `burr` | 2.071 | `zeta +truncate=zipf` | 0.001 |
| `nbinom` | 1.308 | `cmp` | 0.001 |
| `logis` | 1.149 | `gev` | 0.000 |
