# Faster slash fits, 2026-11-01

The golden free fit improves from 0.905377 s to 0.826082 s (1.10×); holding scale improves from 45.6382 ms to 34.1928 ms (1.33×). Across 600 seeded samples and all four held subsets, all 2,400 pairs have identical statuses, parameters, log likelihoods and iteration counts. All six required final commands passed. No golden files were changed and no commits were made.

The implementation caches `log(sigma)` once per profile likelihood pass and avoids scale-score `expm1` calls whose result already rounds to one. The cache is a scalar local to the profile evaluation: it cannot outlive or disagree with its scale. The density helper retains the original standardized-distance overflow handling, small-distance series and subtraction order. Public density validation remains in `dslash`; the fitter uses the internal helper with its computed scale. CDF, quantile and sampler algorithms are unchanged, as are the location grid, scale brackets, bisection midpoints, refinement tolerances, iteration budgets, collapse comparisons and failure statuses. The R/common bindings require no edits and receive the change through their existing C++ delegation.

For positive t, the scale-score summand is `1-2*t/expm1(t)` outside its existing small-t series. The ratio is decreasing: its derivative has numerator proportional to `exp(t)*(1-t)-1`, which decreases from zero. At t = 42 the ratio is below 4.83e-17, whereas the halfway distance immediately below binary64 one is 2^-54, about 5.55e-17. Hence round-to-nearest gives exactly one for t >= 42. The margin also accommodates ordinary libm rounding error; this is not a formal accuracy guarantee for arbitrary nonconforming math libraries or nondefault rounding modes. The old code already substituted one above 700; the patch moves that threshold to 42. The appended regression checks the neighborhood of 42 and 10,001 points spanning 40 to 45. A second appended test checks cached and public log density against the previous arithmetic over extreme locations, scales and observations, including overflowing differences. The truth-table and full R/JS tests provide additional numerical evidence.

The [counter profile](2026-11-01-slash-speed-data/profile.csv) uses the golden n = 500 sample and default C++ controls, with held coefficients at the golden estimates. Mask bit 1 holds mu; bit 2 holds sigma. Counters are generated only in ignored scratch headers by [profile.py](2026-11-01-slash-speed-data/profile.py). New `expm1` counts are the calls retained by the new threshold along the unchanged baseline path; they are not a wall-time attribution.

| Held | Unique grid points | Profile evaluations | Scale bisections | Score observation visits | Score expm1 before → after | Density evaluations |
|---|---:|---:|---:|---:|---:|---:|
| none | 4,120 | 4,155 | 260,116 | 130,058,000 | 117,537,102 → 112,521,057 | 2,077,500 |
| mu | 0 | 1 | 59 | 29,500 | 26,356 → 24,895 | 500 |
| sigma | 4,120 | 4,155 | 0 | 0 | 0 → 0 | 2,077,500 |
| mu, sigma | 0 | 1 | 0 | 0 | 0 → 0 | 500 |

Scale-score visits dominate the free fit. Holding scale removes those solves, making repeated density work the remaining target; the scalar logarithm cache then has a larger proportional effect. No Boost special functions are used in these inner calculations: the expensive calls are standard-library `expm1`, `log` and `exp`. The grid, inner-solve counts and refinement work are not reduced. Profiling counts operations rather than assigning exclusive time to functions.

Warm starts, narrower scale brackets, Newton replacements and a coarser location grid were considered but not adopted: they change represented root trajectories or the searched candidates, and the current multimodal profile has no complete mode-isolation certificate. Replacing positive-argument `expm1` algebraically with negative-argument expressions also changes rounding. The retained changes remove repeated work while preserving the original arithmetic results. Earlier residual caching and midpoint-stagnation handling remain as documented in the [fit-speed report](2026-10-31-fit-speed-report.md).

The reproducible workload is in [compare.cpp](2026-11-01-slash-speed-data/compare.cpp), paired with the frozen [baseline.hpp](2026-11-01-slash-speed-data/baseline.hpp). The baseline preserves the original fitter and log-density evaluation, with names/includes adjusted for paired compilation and the density specialized to the log output actually used by fitting. It preserves conditional evaluation of the distance term. Final timings were rerun after correcting an intermediate baseline copy that evaluated that term eagerly.

`StdRng<>(13320261101ULL)` generates 150 samples at each n = 8, 32, 128 and 500. Within each n there are 30 samples in each of five groups: ordinary slash, quarter-mass ties, two separated groups shifted by ±30, scale 1e-5, and scale 1e5. All have generating location 2; held values use that location and generating scale. Every sample runs all four held subsets with tol = 1e-9 and maxit = 200. [paired.csv](2026-11-01-slash-speed-data/paired.csv) retains all results and elapsed times; [summarize.py](2026-11-01-slash-speed-data/summarize.py) asserts acceptance and writes [summary.txt](2026-11-01-slash-speed-data/summary.txt).

The acceptance rule is `old_ll - new_ll <= 1e-12 * max(1, abs(old_ll))`. There are zero losses, zero numerical result differences and zero status changes. Every fit in both versions is Ok (2,400 each); the list of changed statuses is empty. Existing C++ and full R tests separately cover collapse, unboundedness, multimodality, invalid inputs and unfinished refinements. The seeded sweep is regression evidence, not an independent global oracle.

Mean elapsed times over 150 samples per n and subset, in milliseconds:

| n | Held | Before (ms) | After (ms) | Speedup |
|---:|---|---:|---:|---:|
| 8 | none | 1.253980 | 1.200747 | 1.04× |
| 8 | mu | 0.006110 | 0.005742 | 1.06× |
| 8 | sigma | 0.055879 | 0.042845 | 1.30× |
| 8 | mu, sigma | 0.000442 | 0.000348 | 1.27× |
| 32 | none | 6.962914 | 6.652983 | 1.05× |
| 32 | mu | 0.017882 | 0.016513 | 1.08× |
| 32 | sigma | 0.320409 | 0.238301 | 1.34× |
| 32 | mu, sigma | 0.001275 | 0.001012 | 1.26× |
| 128 | none | 64.204841 | 61.075218 | 1.05× |
| 128 | mu | 0.060352 | 0.055403 | 1.09× |
| 128 | sigma | 3.089006 | 2.291955 | 1.35× |
| 128 | mu, sigma | 0.004283 | 0.003413 | 1.26× |
| 500 | none | 836.021854 | 783.900999 | 1.07× |
| 500 | mu | 0.226201 | 0.208768 | 1.08× |
| 500 | sigma | 41.180885 | 31.054451 | 1.33× |
| 500 | mu, sigma | 0.016161 | 0.012800 | 1.26× |

Golden-data medians of three paired repetitions per held subset, with the same default C++ controls; [golden.csv](2026-11-01-slash-speed-data/golden.csv) retains all observations:

| Held | Before (ms) | After (ms) | Speedup |
|---|---:|---:|---:|
| none | 905.376923 | 826.082328 | 1.10× |
| mu | 0.219109 | 0.206120 | 1.06× |
| sigma | 45.638200 | 34.192829 | 1.33× |
| mu, sigma | 0.018099 | 0.014503 | 1.25× |

The golden free-fit log likelihood remains -1481.5640876706589. Old is timed before new within each pair. These are workstation elapsed times, not randomized timing trials; shared load and CPU frequency affect absolute numbers. The earlier approximately 0.97 s timing and this run's 0.905 s baseline should not be treated as a separate algorithmic improvement.

All final tests used the local workstation through `/tmp/umvl-cpu/run4`, with its nice priority and one-thread BLAS/OpenMP environment. Long slot waits were allowed to finish; simbox was not used. Builds and benchmark work used the slot wrappers, with scratch and temporary files under this worktree's build directory. No packages were installed, board records edited, other worktrees changed, or git write commands run.

| Final command | Result |
|---|---|
| `just cpp-test` | 105/105; repeated after final density-helper edit |
| `just cpp-test-no-long-double` | 126/126 |
| `just test` | FAIL 0; WARN 0; SKIP 151; PASS 19,950 |
| `just test-full` | FAIL 0; WARN 0; SKIP 0; PASS 41,418 |
| `just js-build` | exit 0 |
| `just js-test` | 409/409; failed 0; skipped 0 |

The development skips are tier exclusions; the full R run has none. There are no permitted test failures. An initial standalone profiling compilation omitted the installed BH include path and failed; correcting the command produced the successful profile. The JavaScript build emitted an existing binomial precision self-comparison compiler warning. Detailed local logs remain under `build/scratch/final-*.log`; the compact retained [test-results.txt](2026-11-01-slash-speed-data/test-results.txt) records outcomes and final source hashes.

The tested base is `8fab6b4909bd054db65b1c32b04a133cd293d159`, branch `codex/slash-speed`, plus the uncommitted slash changes. Environment: Linux 7.0.0-38-generic x86_64, GCC 15.2.0, C++17 `-O2` benchmark driver, R 4.6.1 and BH 1.90.0.1. Final mle.hpp SHA-256 is `b58849c2f48735fa9488534b8ec22bb2f3a1b5351f1af7c0293a72385f21e6e5`; dpqr.hpp is `5461ea7e2135243fa04cb675117929c5ddcf8e86ef3551eb3d30c2c26cb1e9e4`. Goldens and truth fixtures were neither regenerated nor edited.

Reproduce benchmarks and counters from the repository root with `bash dev/research/evaluations/2026-11-01-slash-speed-data/run.sh`; it acquires slots itself, so do not wrap it in another lock. Run the six final commands individually through `run4`, with TMPDIR, JUST_TEMPDIR and XDG_RUNTIME_DIR set to `$PWD/build/tmp`.

Open, inherited mathematical guarantee: the original observation-gap grid and local refinement do not certify that every location mode has been isolated. Held-scale certification needs outward-rounded likelihood/slope bounds and complete interval subdivision; free-scale certification additionally needs enclosed scale roots and collapse neighborhoods. The fixed lower scale bracket is also not a verified root enclosure. These are unchanged limitations, documented in the [canonical slash dossier](../../../univariateML-cpp/include/cppunivariate/distributions/slash/README.md). This job adds no numerical search, cap or stopping rule and makes no new global optimality claim. The requested speed implementation, finite-sample acceptance, final tests and report are complete.
