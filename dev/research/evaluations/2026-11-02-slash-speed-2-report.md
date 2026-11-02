# Slash free-fit speed

2026-11-02. Workstation, source based on `1bec30eadbd874bfed5d2dd97fea7a1981d14ab8`, uncommitted `mle.hpp` SHA-256 `bbd16b1c44448600627aaa166cdb32336b9b99f35a61fe1e63c12c6c9933f47f`. GCC 15.2, `-O2 -std=c++17`, BH 4.5 headers. All measurements used `/tmp/umvl-cpu/run`, one slot; final suites used `/tmp/umvl-cpu/run4` locally, sequentially. Elapsed times are individual measurements under shared load.

The free-scale profile now caches log distances from each observation to its current location before solving the scale score. For finite distances the cached value is exactly the same `log(abs(x-m))` subsequently used in `2*(log_distance-log_scale)-log(2)`; the existing overflow-safe distance path is retained. The score, bisection, score gate, likelihood and branch-and-bound certificate are unchanged in exact arithmetic; comparisons remain conditional on floating-point computation, with the existing practical margin and without outward rounding. No new global optimality theorem is asserted.

| Workload | Before elapsed | After elapsed | Result |
|---|---:|---:|---|
| Heavy tail, n=500, seed 116 | 9.114 s | 6.082 s | Both status 0, logLik -1424.1925253056531 |
| Golden, n=500 | 6.911 s | 7.339 s | Both status 0, logLik -1481.5640876706589 |
| 600 seeded samples | 119.16 s | 80.50 s | All statuses and likelihoods match to 1e-10 relative |

The [seeded generator](2026-11-02-slash-free-data/compare.cpp), [benchmark generator](2026-11-02-slash-free-data/benchmark.cpp), [golden harness](2026-11-02-slash-speed-2-data/golden-bench.cpp), and [before](2026-11-02-slash-speed-2-data/slash-compare-before.csv) and [after](2026-11-02-slash-speed-2-data/slash-compare-after.csv) results are retained. The original header was extracted with `git show HEAD:univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp`, changing only its relative include paths for the scratch build. Compile the harnesses with `-IunivariateML-cpp/include`, the installed BH include, and `-DSLASH_HEADER` naming the relevant header. The C++ random sequence depends on the standard-library implementation. The seeded comparison checks successful finite likelihoods with `abs(a-b) <= 1e-10*max(1,abs(a))` and checks all statuses.

A `g++ -pg -O2` profile of the modified heavy-tail run recorded 8,156 profile calls and 2,212 residual-bound calls. Of 1.84 sampled CPU seconds, 1.06 s was attributed to `Profile::operator()`, 0.38 s to `residual_upper`, 0.13 s to `certify_free` self time and 0.27 s to `_init`. Inlining and uninstrumented library calls prevent a reliable breakdown of special functions or allocation and prevent treating these percentages as a complete wall-time split. The observation-gap grid performs at least eight profile evaluations per distinct gap plus the 129-point grid; subdivision adds further profiles and residual bounds. These are measured call counts and code-level observations, not an independent timing attribution.

**Open:** The observed heavy-tail time exceeds the 5 s target. The golden's single after measurement is slower; shared-load noise prevents determining whether this is a repeatable regression, so the no-slower condition is not established. A separately instrumented phase split for grid, root solving, density/score special functions and branch-and-bound is still needed, as are repeated paired timings. The profile does not resolve those quantities. No claim is made that the requested performance target or golden-speed condition has been met.

Final checks, each a blocking workstation command under `/tmp/umvl-cpu/run4`, with `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` set to `build/tmp`:

| Command | Result |
|---|---|
| `just cpp-test` | Pass, 106/106 |
| `just cpp-test-no-long-double` | Pass, 127/127 |
| `just test` | Pass, 20,500 passes, 153 skips, 0 failures, 0 warnings |
| `just test-full` | Pass, 43,317 passes, 0 skips, 0 failures, 0 warnings |
| `just js-build` | Pass; compiler emitted an existing binomial self-comparison warning |
| `just js-test` | Pass, 413/413 |

The R dev skips are tier exclusions. Initial CMake Boost discovery failed before the recipes selected the locally installed BH headers and continued successfully. Shell startup printed a missing `~/.cargo/env` message without changing the successful command exits.
