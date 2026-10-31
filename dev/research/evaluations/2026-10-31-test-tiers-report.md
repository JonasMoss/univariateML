# TASK-130: test tiers and speed (2026-10-31)

Source: `73a60c0064e937066f5be1c80b3e4f0cf488aa6e` plus this worktree's tier changes;
no fits, goldens, truth fixtures or settled d/p/q/r code changed. No commits.
Ubuntu 26.04.1, GCC 15.2, R 4.6.1, testthat 3.3.2, pkgbuild 1.4.8,
Rcpp 1.1.2, BH 1.90.0.1; same workstation throughout, no simbox.
Every build/test used `/tmp/umvl-cpu/run4`, `nice -n 10`, four reserved slots,
and one BLAS/OpenMP thread per process. No other slot-managed job could run;
process visibility did not establish absence of unrelated host activity.
Temporary files/logs are in this worktree's `build/tmp` and `build/scratch`.

## Paired wall times (seconds)

All recipe times include regeneration/build; shared ccache was enabled.
The paired test-only times separate build/cache costs; cold PCH trials disabled ccache.
Before tiers existed, dev/full selected the same complete serial R workload.
The serial control set `TESTTHAT_PARALLEL=false`; isolated checks set `UMVL_TEST_TIER=cran`.

| Workload | Before | After | Result |
|---|---:|---:|---|
| `just test` (dev) | 463.24 | 136.14 | 18,887 passes, 151 skips |
| `just test-full` | 463.24 (same old suite) | 247.56 | 39,040 passes, 1 skip |
| Optimized serial full R control | 463.24 | 426.20 | 39,040 passes, 1 skip |
| `just check-standalone` (CRAN) | 391.42 | 161.76 | Status: OK |
| Check alone, excluding regen/build | 356.40 | 128.69 | Status: OK |
| Test R process inside check | 317.54 | 77.78 | 13,463 passes, 213 skips |
| `just cpp-test` (dev; warm after build) | 239.83 | 42.03 | 104/104 shards |
| C++ full, whole recipe (warm after dev) | 239.83 | 109.81 | 125/125 shards |
| C++ test execution, dev/full | 152.77 | 41.87 / 109.59 | all green |
| C++ clean build, ccache disabled: PCH off/on | 79.00 | 56.76 | 28% faster |

Original R baseline: FAIL 0 / WARN 0 / SKIP 1 / PASS 39,040.
Final dev/full test-only times: 115.29/224.53 s. An earlier dev run took
106.94 s including regen; final timings above are not selected minima.
Original isolated check: `Status: OK`, 39,018 passes / 3 skips.
Final C++ full exactly matches 950 total cases / 1,292,464 assertions.
C++ cran also passed 104/104 shards (45.13 s); every final tier had zero failures/warnings.
Original C++: six targets; 928 distribution cases and 1,281,417 assertions
(the brief's 922-case count predates this actual source revision).
Optimized `just regen`: 19.28 s; compile logs show `ccache g++ ... -g -O2`,
without pkgbuild's trailing `-O0`. Assembly deletes the generated package first,
so a debug DLL cannot survive regeneration. The environment flag reaches workers.

## Coverage and classification

[Inventory](../../validation/test-tier-inventory.csv) records all 922 baseline
result-table rows, costs and purposes, plus legacy blocks whose cost was not separated.
`UMVL_TEST_TIER` overrides `cran|dev|full`; otherwise absent/false `NOT_CRAN`
means cran and true means dev. `just test` defaults explicitly to dev;
`just test-full` selects full. Invalid names stop. R CMD check stays serial
unless `TESTTHAT_PARALLEL=true` is requested; local recipes use four workers.
All tiers keep cheap API/error/face/identity/frozen-reference checks and one golden
fit per family. Shared scale sweeps and measured GB2/shifted-Pareto face and
CMB profile-grid sweeps start in dev. Oracle/multistart and replication stress tests
are gated inside test bodies, preserving cheap cases in the same family file.
CRAN omits the conformance fit sweep; dev holds none, each single coefficient,
and all regular coefficients; full enumerates every holdable subset.
Truth checks select 1/3/all parameter points in cran/dev/full, deterministically;
value/quantile rows per group are 2/1 in cran and baseline 5/3 in dev/full.
Full retains every original expectation and tolerance, including baseline truth sampling.
Edition 3 enables testthat scheduling; the setup retains edition 2 expectation
semantics, avoiding an unrelated comparison/condition migration.
C++ shards partition by family source and complementary slow-name filters;
cran/dev label ordinary shards; full adds oracle, exhaustive-held and slow-reference shards.
A cached clean build took 3.95 s: 105 direct hits, zero new misses.
The PCH covers the dominant distribution target; main is excluded to preserve
its doctest implementation macro. PCH-compatible ccache settings are target-local.

## The 111 s versus 570 s discrepancy

The supplied CSVs sum to about 111 s for 624 isolated fits versus 568.614 s
inside testthat. They do not record a paired DLL/build/load state.
Controlled same-DLL, same-data, same-subset runs under the slot lock gave
85.77 s without assertions and 93.83 s with SilentReporter/handlers (debug DLL).
Rprof attributed 83.85/83.99 s respectively to native calls; only about 8 s
was added by assertions/reporting, not the historical extra 457 s.
The independent baseline check's conformance file took 68.96 s with its optimized DLL.
Optimized controls gave 78.09 s bare and 94.94 s checked; the serial full-suite
conformance time was 87.29 s: no large order/reporter penalty reproduced.
Thus the historical fivefold gap is not reproducible as testthat overhead;
the CSVs alone cannot identify its extra elapsed time or prove an order/GC cause.
Treat 570 s as historical unpaired evidence, not an intrinsic conformance cost.
[Reproducer](../../validation/profile-conformance.R) retains the controlled experiment.

## Slow fits: supplied isolated n = 500 workload

Times below are the supplied baseline, not new fitter benchmarks; no fitter was changed.

| Fitter | Held coefficients | Seconds |
|---|---|---:|
| `mlbhatt` | none | 3.088 |
| `mlemg` | none | 6.113 |
| `mlfpareto` | min | 1.001 |
| `mllst` | sigma | 12.796 |
| `mlsged` | mean | 1.284 |
| `mlsged` | sd,nu,xi | 3.544 |
| `mlslash` | none | 1.845 |
| `mlsstd` | mean | 1.162 |
| `mlsstd` | nu,xi | 4.880 |
| `mlsstd` | sd,nu,xi | 1.365 |
| `mlstd` | sd | 20.453 |
| `mltlambda` | none | 5.203 |
| `mltlambda` | mu | 1.426 |
| `mltlambda` | sigma | 23.785 |
| `mltlambda` | mu,sigma | 6.153 |

## Limits and parallelism

No cross-file dependency fixes were needed. An always-run setup is required:
serial source loading resets the edition after helpers, unlike parallel workers.
One full conformance file and long C++ held-sweep cases remain serial tail costs.
Two preliminary runs failed (runner edited while R read it; edition
reset in a serial control); corrected reruns above are green and replace them.
Parallel testthat result-table times measure parent event replay, not worker fit
cost; use external wall clocks. Its tables also omit some legacy top-level
expectations, so reporter totals govern the 39,040-expectation comparison.
These are finite regression/numerical checks, not new mathematical guarantees.
