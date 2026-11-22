# Test-suite long poles and strict caches

TASK-214.48 · baseline `460a0ff2` · measured November 21–22, 2026

Native CTest wall time fell from **423 to 273 s**, double-only CTest from **294 to 282 s**, and the precompiled R dev test phase from **333 to 287 s**. R full test execution took **736 and 743 s**. The four snapshot modes together took **835 and 833 s**. Warm strict fell from **134 to 118 s**, mostly through reuse of parsed refinement metadata.

The C++ case inventories match in every tier and precision build. Both C++ suites passed with matching assertion totals. The R dev and full expectation counts match in all 130 files. All four snapshot record streams match the new baseline runs and the stored master data byte for byte. Warm strict returned the same four known `tlambda` findings and zero new failures. [Equivalence results and hashes](2026-11-21-speed-e-data/equivalence.log)

The Markdown link check passed with zero new broken destinations and 47 inherited ones. [Link check](2026-11-21-speed-e-data/check-links.log)

## Timings

All runs used the workstation with GCC 15.2.0, R 4.6.1 and testthat 3.3.2. The heavy runs reserved four CPU slots and used one BLAS/OpenMP thread per worker. CPU seconds add user and system time. Baseline C++ recipe timings include slot acquisition and a fresh build. Revised recipe timings use the existing builds and shared compiler cache. The CTest rows use wall times from the two recipe runs and baseline CPU totals from a separate replay of the selected targets. R execution CPU sums the per-file worker measurements. Each paired wall result is one observation on a shared machine. [Timing data](2026-11-21-speed-e-data/timings.csv)

| Step | Baseline wall s | Revised wall s | Baseline CPU s | Revised CPU s |
|---|---:|---:|---:|---:|
| Native C++ recipe, dev | 1181.69 | 287.10 | 4553.94 | 1094.77 |
| Native CTest execution | 422.82 | 272.70 | 1030.44 | 1080.17 |
| Double-only C++ recipe, full | 878.69 | 295.69 | 3428.40 | 1142.29 |
| Double-only CTest execution | 293.80 | 282.29 | 1163.33 | 1128.75 |
| R dev test phase on compiled DLL | 332.88 | 286.71 | 1056.80 | 1090.32 |
| R full recipe, including truth check and regeneration | 1182.70 | 791.77 | 3351.57 | 3121.47 |
| R full test phase | 735.62 | 743.09 | 2659.75 | 2828.17 |
| Snapshot native | 331.59 | 328.04 | 1294.52 | 1307.95 |
| Snapshot double | 261.32 | 265.53 | 1032.36 | 1056.02 |
| Snapshot routes native | 121.45 | 120.53 | 459.22 | 453.63 |
| Snapshot routes double | 120.19 | 118.90 | 450.91 | 452.23 |
| All snapshots | 834.56 | 833.01 | 3237.00 | 3269.82 |
| Warm strict recipe | 133.92 | 118.05 | 255.35 | 243.19 |

The revised C++ builds spent 14.4 s native and 13.4 s double-only outside CTest. Each incremental build compiled two objects and linked `distribution_tests` once. The two objects came from the CMB fixture edit and the restoration of the trial discrete-gamma fixture edit. The PCH, compile options and executable count are the same. The baseline build-and-configuration remainder was 758.9 s native and 584.9 s double-only with fresh build directories, so these build totals include different cache states.

## C++ case profiles

At `460a0ff2`, CTest already dispatches one shared `distribution_tests` executable by family and by the ordinary and full-only filters. Generalized beta was the longest native dev target in the initial suite at 307 wall seconds. In the target profile, it took 236 CPU seconds native. The largest double-only target was SGED at 131 CPU seconds. Target times and isolated-case times vary across invocations. The case profile measures each registered body with doctest's duration output and process CPU accounting. Every body completed successfully. [Native targets](2026-11-21-speed-e-data/cpp-before-native.targets.csv), [double-only targets](2026-11-21-speed-e-data/cpp-before-double.targets.csv), [native cases](2026-11-21-speed-e-data/cpp-before-native.cases.csv), [double-only cases](2026-11-21-speed-e-data/cpp-before-double.cases.csv)

The table includes the ten highest-CPU cases from each precision build. Cells show CPU and wall seconds for cases in that build's top ten.

| Case | Native CPU / wall s | Double CPU / wall s | Work performed |
|---|---:|---:|---|
| PIG d/p/q/r truth table | 93.93 / 93.95 | 46.87 / 46.87 | Discrete density and tail evaluations at 2,716 value rows, 616 quantile queries and two seeded sampler runs |
| CMP slow reference round trips | 61.43 / 61.43 | 30.28 / 30.29 | Four probability representations and up to 201 lattice points per truth parameter point, with repeated CDF inversion |
| Generalized beta large replication | 61.39 / 61.42 | 86.46 / 86.49 | Fits a conditional shape to ten million identical observations and compares it with an analytic root |
| Generalized beta exact gamma and effective-endpoint power faces | 59.90 / 59.92 | — | Fits two 300-point samples designed to reach different limit families |
| Generalized beta shallow gamma-adjacent proposal | 48.55 / 48.57 | 39.06 / 39.07 | All-free fit and certificate search near the gamma face on 300 quantile-generated observations |
| Generalized beta incomplete gamma certificates | 45.83 / 45.84 | — | Default fit on a 300-point gamma-face grid, followed by a one-cell certificate exercise |
| Poisson slow reference round trips | 45.05 / 45.07 | 58.37 / 58.37 | Full parameter-point lattice scan and inverse checks across four probability representations |
| Discrete gamma ordinary starts | 40.50 / 40.51 | 41.47 / 41.50 | A 150-count sample exercises the fit's allowance accounting during remote shape probes |
| Tukey lambda every held golden subset | 37.59 / 37.61 | 55.54 / 55.55 | Repeated profile fits for all held subsets of the golden sample |
| Truncated Poisson modifier four-mode round trips | 33.35 / 33.36 | 32.20 / 32.23 | Scans the modifier lattice with lower and upper tails on natural and log scales |
| CMB free-size endpoints, invalid controls and work exhaustion | — | 95.12 / 95.13 | Exercises a finite work allowance on counts 0 and 100 with held probability `1e-8` |
| SGED half-GED payload and original-unit likelihood | — | 29.94 / 29.94 | Fits both signs of a one-sided 100-point sample and checks the recovered limit likelihood |

CTest now schedules expensive cases independently within their existing tiers for generalized beta, Bhattacharjee, beta-negative-binomial, CMB, discrete gamma, hypergeometric, SGED, skew-t and transformed gamma. Large indivisible targets such as the PIG truth table and the CMP, Poisson and Tukey lambda full-only cases receive early priorities. The existing family registration runs the rest of its cases. The saturated CMB oracle computes its 2,187 fixed-parameter grid fits once and uses each result in both assertion sweeps. Profiling showed that the trial discrete-gamma duplicate involved a cheap case, so the final change there is scheduling alone.

The final workstation runs passed **134/134 native dev registrations** and **179/179 double-only full registrations**. They matched **2,409,269** and **2,667,497** baseline assertions, respectively. The complete doctest inventories contain 2,772 native and 2,773 double-only cases in cran and dev, and 2,827 native and 2,828 double-only in full. Dev and full also include the shared diagnostic executable. The changed-family plan identified shared effects. The prescribed C++ and R recipes covered that selection. [Native inventories before](2026-11-21-speed-e-data/cpp-before-native.inventory.csv), [native after](2026-11-21-speed-e-data/cpp-after-native.inventory.csv), [double-only before](2026-11-21-speed-e-data/cpp-before-double.inventory.csv), [double-only after](2026-11-21-speed-e-data/cpp-after-double.inventory.csv), [changed-family plan](2026-11-21-speed-e-data/test-changed-plan.log)

## R file profiles

The table includes the ten highest-CPU files in each tier. Cells show worker CPU and wall seconds for files in that tier's top ten. The worker profiles count expectation events at file level, including legacy top-level expectations. [Dev before](2026-11-21-speed-e-data/r-before-dev.csv), [dev after](2026-11-21-speed-e-data/r-after-dev.csv), [full before](2026-11-21-speed-e-data/r-before-full.csv), [full after](2026-11-21-speed-e-data/r-after-full.csv)

| Test file | Dev CPU / wall s | Full CPU / wall s | Work performed |
|---|---:|---:|---|
| `dist-genbeta` | 169.03 / 169.23 | 272.15 / 272.30 | Conditional and all-free profile fits, boundary regressions and full-tier multistart comparisons |
| `modifiers` | 139.54 / 140.21 | 140.57 / 140.73 | Retired-route replay, fitted modifiers, tail and inversion checks and the modifier/hold refusal matrix |
| `conformance` | 124.25 / 125.11 | 247.35 / 248.71 | Fits golden samples across hold patterns. Full traverses every holdable subset |
| `input_checks` | 62.40 / 62.42 | 87.30 / 87.37 | Tests wrapper inputs and replays each golden fit for the density-sum comparison |
| `dist-bnbinom` | 56.90 / 56.92 | — | Enumerates held subsets and finite-label competitors and checks their certificates |
| `scale_equivariance` | 56.40 / 56.44 | — | Repeats fits on seeded samples and their affine or scale transforms |
| `golden_regression` | 51.43 / 51.44 | — | Replays the frozen golden fits and checks coefficients and likelihoods |
| `dist-std` | 47.03 / 47.31 | — | Fits held-scale and free-shape profiles, including budget and near-normal cases |
| `dist-cmb` | 39.34 / 40.08 | 94.75 / 94.78 | Free-size and dispersion searches, endpoint and exhaustion cases and dense reference grids |
| `dist-hyper` | 35.17 / 41.66 | — | Fits free integer design coordinates and compares with finite searches |
| `dist-sged` | — | 359.91 / 360.16 | Interior, classified-hold and P0b multistart comparisons with independent likelihoods |
| `dist-sstd` | — | 307.22 / 307.41 | Interior and boundary oracles plus multistart sweeps over classified holds and P0b subsets |
| `generated-dpqr` | — | 205.90 / 215.18 | Full truth rows and quantile brackets across families, with sampler and RNG checks |
| `dist-slash` | — | 103.57 / 103.59 | Independent density, multistart and mixture-integral references |
| `dist-lst` | — | 79.27 / 79.33 | Nested Student profiles and independent multistart likelihoods for held coefficients |

Testthat's `Config/testthat/start-first` starts the costly files early. Workers reuse parsed golden fit arguments within a process. The recipe bounds Make parallelism during regeneration by the reservation and starts testthat after the DLL build, with one Make job available to each worker. The tests' source files, explicit seeds and tier gates are shared by both runs. The measured full worker CPU increased by about 6%, including substantial variation in SGED and CMB, and the test wall time was nearly flat. The recipe totals also reflect a warmer compiler cache.

Both runs had zero failures. Dev had **36,451 passes, 29 warnings and 157 skips**. Full had **65,854 passes, 29 warnings and one skip**. Counts match per file and event type. [R verification](2026-11-21-speed-e-data/equivalence.log)

## Snapshot scheduling

Parallel snapshots start families in descending baseline CPU cost, separately for each precision and route mode. This changes dispatch order. The merge still uses the sorted family list. Per-family CPU estimates sum compiler CPU, probe process ticks and Python worker CPU. Route time is mostly compiler work. Base-family skew comes mainly from probes such as generalized beta, F-Pareto and discrete gamma. [Priority order](2026-11-21-speed-e-data/snapshot-priority.json)

| Mode | Three largest families by CPU | CPU s | Wall s |
|---|---|---|---|
| Native | genbeta, fpareto, dgamma | 199.22, 197.68, 69.26 | 199.23, 197.74, 69.28 |
| Double | genbeta, dgamma, gb2 | 153.80, 89.71, 54.26 | 153.78, 89.70, 54.31 |
| Routes native | zmbinom, tnorm, ztbinom | 43.46, 38.16, 33.19 | 43.50, 38.27, 33.25 |
| Routes double | zmbinom, tnorm, ztgeom | 42.25, 37.26, 31.78 | 42.27, 37.28, 31.84 |

Full per-family profiles are available for [native](2026-11-21-speed-e-data/snapshot-before-native.csv), [double](2026-11-21-speed-e-data/snapshot-before-double.csv), [native routes](2026-11-21-speed-e-data/snapshot-before-routes-native.csv) and [double routes](2026-11-21-speed-e-data/snapshot-before-routes-double.csv). The revised profiles are [native](2026-11-21-speed-e-data/snapshot-after-native.csv), [double](2026-11-21-speed-e-data/snapshot-after-double.csv), [native routes](2026-11-21-speed-e-data/snapshot-after-routes-native.csv) and [double routes](2026-11-21-speed-e-data/snapshot-after-routes-double.csv).

Each base snapshot contains 11,905 records. Each route snapshot contains 1,950. Every per-family payload and merged stream matches byte for byte. The compressed streams match after masking their four gzip timestamp bytes. The per-family resume tag and provenance record the changed snapshot source fingerprint. The measured aggregate wall change was about 1.6 s. Longer jobs finished earlier in the queue, while aggregate CPU was about 1% higher. [Snapshot hashes](2026-11-21-speed-e-data/equivalence.log)

## Warm strict and cache checks

| Script | Baseline wall s | Revised wall s | Baseline CPU s | Revised CPU s |
|---|---:|---:|---:|---:|
| Fit judge | 0.47 | 0.51 | 0.46 | 0.50 |
| Density | 24.52 | 25.96 | 24.45 | 25.89 |
| Consistency | 19.72 | 19.43 | 19.63 | 19.34 |
| Refinement, cache only | 22.35 | 3.73 | 22.32 | 3.71 |
| Certificate replay | 1.19 | 1.18 | 1.18 | 1.17 |
| Seam | 65.63 | 67.20 | 187.25 | 192.51 |
| Total | 133.92 | 118.05 | 255.35 | 243.19 |

The seam checker uses eight stable groups. Each binary's key includes its preprocessed translation unit, compiler and flags. Each result also keys the exact probe input, guard and reference sources. A local header invalidates the groups that compile it. Shared headers can invalidate all groups. A control in a scratch projection warmed exponential and uniform probes, then added an unused declaration to the exponential adapter header. The exponential probe reran, the uniform probe hit the cache and both outputs matched byte for byte. The baseline cold seam run took 487.53 wall s and 1,208.48 CPU s. The grouped cold run took 479.45 wall s and 1,667.66 CPU s, reflecting duplicated compile work across groups. A subsequent reseed using the existing grouped binaries took 305.22 wall s. Warm seams hit 9,691 of 9,694 primary probes and all three long-guard retries per mode. [Header control](2026-11-21-speed-e-data/seam-header-control.log), [cache timing runs](2026-11-21-speed-e-data/timings.csv)

Refinement reuses parsed dossier and density metadata within a run. In the original cache-only run, 26 mode-case entries appeared as missing. The corresponding original full run classified them as open after two previous case guards in each of CMB, CMP and truncated Poisson. There were eight CMB, eight CMP and ten truncated-Poisson entries. The revised cache-only run derives the same exclusions from the current cache ledger. The tracked reference cache is byte-identical. Its 3,822 previously reported records match, and the revised cache-only records including the budget notes match all 3,848 records in the original full run. [Budget exclusions](2026-11-21-speed-e-data/refine-budget-open.csv), [record checks](2026-11-21-speed-e-data/equivalence.log)

The cold certificate cache fill took 4,312 wall s and 16,160 CPU s. The baseline warm replay used that cache with the common reconstruction module, whose only source change is snapshot scheduling. Both warm runs hit all 13,054 replay entries. The comparison matched all six ratchet outputs, 7,152 certificate result records and 19,388 seam records after guard retries. Four known consistency findings remain listed. One eight-second single-slot refinement measurement overlapped the cold fill. Both warm strict runs were separate from that overlap. [Strict verification](2026-11-21-speed-e-data/equivalence.log)

## Reproduction

Run the existing recipes through `/tmp/umvl-cpu/run4`. CTest and testthat apply the new scheduling automatically. The snapshot command keeps its existing `--jobs 4` interface, with `--no-long-double` and `--modifier-routes` for the other modes. `just strict` accepts the same snapshot paths and applies the scoped seam cache automatically. [Validation guide](../../validation/README.md)

The C++ profiling script reads the CTest registrations from an existing build. Its `--time` option times the targets and each isolated doctest case. The snapshot profiler takes the snapshot script, a CSV path and the original snapshot arguments. For a baseline run, reconstruct `460a0ff2` in ignored scratch with `git archive` and use a CSV basename starting with `snapshot-before-`. For R worker profiling, place the evidence setup in the generated package's `tests/testthat` directory after regeneration, set `UMVL_FILE_PROFILE_DIR` to an existing absolute directory, and run the local test runner with a compiled DLL. [C++ profiler](2026-11-21-speed-e-data/profile-cpp.py), [snapshot profiler](2026-11-21-speed-e-data/profile-snapshot.py), [R profiling setup](2026-11-21-speed-e-data/setup-profile.R)

## Open

- Generalized beta still repeats its gamma and power face fits across the exact-face and structured-payload cases, with individual repeats in the two certificate cases. The shared inputs are the 300-point quantile grid and the squared grid. `loose-2` owns that file. The ten-million-observation conditional fit is another indivisible case under the present case contract.
- The CMB work-exhaustion fit and the discrete-gamma remote-shape-probe fit still take substantial CPU. The cases and measurements are listed above. The PIG truth check and CMP, Poisson and truncated-Poisson round trips also retain substantial inversion and tail costs.
- SGED and SSTD's multistart file workloads and the native fit sweeps in R conformance remain the principal full-tier costs. The R discrete-gamma file also repeats the same five-observation million-count stop in two cases. The R test-file bodies were outside the owned runner and helper scope.
- The auxiliary selector check `test_shared_and_docs` expects 99 fitted families and sees 85 in both the baseline checkout and this tree. It is independent of the case-shard selector change. [Baseline failure](2026-11-21-speed-e-data/selector-check-baseline.log), [revised failure](2026-11-21-speed-e-data/selector-check-after.log)
