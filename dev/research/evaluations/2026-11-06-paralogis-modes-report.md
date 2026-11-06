# TASK-116.8: competing paralogistic modes and large-sample speed

Both requested changes are implemented, and all final checks passed. With scale held at 1, `x = exp(-0.001)` now returns the certified global mode at shape **9262.778134534776**, log likelihood **8.126729864909168**. The large-sample certificate is faster without relaxing its root accuracy, global comparisons or proof budget. At n = 100000, matched workstation runs reduced median certificate time from **9.51 s to 0.812 s** for distinct quantiles and from **7.29 s to 0.0681 s** for replicated quantiles. All golden files are unchanged.

Tested base: `f30584cd1819744e130850bcb1e58a5d1c52ed28`, branch `codex/paralogis-modes`, with final authored sources fingerprinted in [source-manifest.json](2026-11-06-paralogis-modes-data/source-manifest.json). Authored changes are confined to the paralogis fitter/certificate/dossier, the two families' C++ and R tests, and this report/evidence directory. No commits, Git-state writes, board edits, dependency installs or changes to another checkout were made. The site check regenerated an unrelated tracked screenshot; its original bytes were restored. Settled d/p/q/r, special-function, truth and code-generation sources were not changed.

The [canonical dossier](../../../univariateML-cpp/include/cppunivariate/distributions/paralogis/README.md) records the algorithm and arithmetic policy. The historical held-scale proposal remains the first attempt. If its certificate fails, a dyadic shape scan proposes additional positive-to-negative score crossings and refines them by bisection. Feasible likelihoods select a replacement proposal. The scan establishes neither completeness nor globality: the replacement must independently pass the narrow root contraction, both tail comparisons and exhaustive compact-domain exclusion. Proposal exhaustion still stops; no unfinished optimizer or capped search estimate becomes a successful fit. The joint BFGS proposal is unchanged.

The root enclosure retains relative shape radius `1e-8`; free-scale enclosures retain the existing relative-width requirement. The search budget remains 32768 boxes per certificate. Exact repeated log-data intervals are compressed with their multiplicities, their sum is cached, and sorted interval bins at resolutions up to 128, 1024 and 8192 groups provide conservative likelihood, score and Hessian bounds. The mean-value exclusion map can also use those bounds. Small unresolved boxes and the local contraction use individual groups. Every bin contains every observation it replaces, so these substitutions preserve the exclusion argument. Sorting adds preparation cost, while most large-sample search evaluations cease to scan every observation. No unconstrained Burr fit is introduced.

Large held shapes need scale-sensitive score arithmetic. The dedicated scalar score and curvature use relative 64-epsilon operation margins plus subnormal allowances; centered logs carry margins proportional to the log operands and their difference. Other bounds retain the Burr arithmetic policy. This removes excessive absolute padding that obscured the higher root. The certification claim remains **exact-arithmetic certification conditional on the computed comparisons, with the stated practical floating-point margins**, rather than outward-rounded interval arithmetic. The analytic existence/tail bounds and contraction/exclusion argument supply the certificate; timing measurements and independent numerical oracles supply separate empirical evidence. General analytic uniqueness for arbitrary samples, and completeness of the proposal scan, remain unproved.

One intermediate development run failed the new enclosure test: a newly added guard checked rounded softplus endpoint order before applying its error margins. Near zero the two computed endpoints can reverse by one ulp. Applying the margins before checking interval validity corrected the rejection; the test was retained unchanged. The [minimal reproducer](2026-11-06-paralogis-modes-data/rounded-endpoints.cpp), [values](2026-11-06-paralogis-modes-data/rounded-endpoints.tsv), and [failed-run log](2026-11-06-paralogis-modes-data/rejected-guard-simbox.log.gz) preserve the reason for this choice. All final suites below use the corrected numerical source.

The independent [70-digit mpmath calculation](2026-11-06-paralogis-modes-data/oracle.py) uses the exact original double datum and finds three stationary points:

| Shape | Log likelihood | Curvature sign |
|---:|---:|:---:|
| 2.895379787257729 | -0.5700990971060364 | negative |
| 1140.924271780363 | -303.6829139605954 | positive |
| 9262.778134534774 | 8.126729864909167 | negative |

The certificate still rejects the first root and certifies the third; numerical agreement alone is not used as the globality argument. The [mode checker](2026-11-06-paralogis-modes-data/modes.cpp) passes for `x = exp(-d)`, held scale 1, at d = 0.1, 0.01, 0.003, 0.001, 0.0001 and 0.00001, including certified shapes up to 1424802.7812. Its [results](2026-11-06-paralogis-modes-data/modes.tsv) include repeat certificates. Permanent C++ and R tests cover the competing mode and reciprocal likelihood transport. A direct [WebAssembly check](2026-11-06-paralogis-modes-data/modes.mjs) also passed for both families, including the inverse Jacobian; [its output](2026-11-06-paralogis-modes-data/modes-wasm.txt) agrees with native results.

The retained [benchmark](2026-11-06-paralogis-modes-data/benchmark.cpp) compares the base and final source using the same executable source, GCC 15.2.0, `-O2 -std=c++17`, and one reserved workstation CPU slot. Each of three interleaved pairs runs all 23 fixtures. Every fixture certified in every run. The [analysis script](2026-11-06-paralogis-modes-data/analyze.py) checks statuses, coefficient agreement within `1e-7` relative error, and likelihood agreement within `1e-7 * (1 + abs(logLik))` before computing medians. Raw `before-paired-{1,2,3}.tsv` and `after-paired-{1,2,3}.tsv` files are retained beside the [timing summary](2026-11-06-paralogis-modes-data/timing-summary.tsv).

| Workload | n | Before certificate ms | After certificate ms | Before / after |
|---|---:|---:|---:|---:|
| Distinct quantiles | 500 | 25.643 | 27.152 | 0.94 |
| Distinct quantiles | 10000 | 769.472 | 418.069 | 1.84 |
| Distinct quantiles | 100000 | 9510.409 | 812.407 | 11.71 |
| 1000 quantiles, each repeated 100 times | 100000 | 7293.929 | 68.056 | 107.18 |

Distinct samples use probabilities `(i + 0.5)/n` from paralogis shape 1, scale 1. The replicated workload uses the corresponding 1000 midpoint quantiles. These workloads have no random seed. Proposal medians before/after were respectively 0.549/0.578, 13.774/14.637, 116.316/101.015 and 98.862/100.639 ms. Certificate clocks include their data preparation; public-wrapper witness creation and inverse reciprocal/Jacobian overhead are excluded. The inverse family inherits the parent improvement through its existing delegation.

These are shared-workstation measurements, not isolated statistical benchmarks. For distinct n = 100000, certificate ranges were 9.49–11.45 s before and 0.789–1.262 s after. The n = 500 ranges overlap, and its median shows a small overhead rather than a speed gain. Search-box counts before/after were 235/235, 345/345, 425/431 and 290/287: the large-n improvement comes from cheaper valid evaluations, not a smaller proof budget or an early acceptance rule.

Both n = 500 golden samples, their held-scale fits, five 30-point quantile shapes and their held-scale fits, one-point/constant cases, a near-tie sample and the large workloads are covered by the benchmark. Maximum relative shape/scale deviations from the stored ordinary goldens were `4.015e-8 / 2.642e-7` for paralogis and `9.478e-8 / 4.912e-7` for the inverse fixture on reciprocal data, below the existing `3e-5` tolerance. The ordinary and historical held-scale golden files were checked byte-for-byte against the base. No fixtures or tolerances were regenerated or relaxed.

| Final command/check | Machine | Result | Test elapsed |
|---|---|---|---:|
| `just cpp-test` | simbox | 106/106 dev shards | 60.51 s |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 127/127 shards | 86.38 s |
| `just cpp-test-no-long-double` | simbox | 127/127 shards | 74.61 s |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 20826 | 114.558 s |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 43642 | 242.271 s |
| Targeted `*paralogis*` doctests | workstation | 27 cases, 5113 assertions passed | — |
| `just js-build` | workstation | pass | — |
| `just js-test` | workstation | 430/430; no failures or skips | 149.847 s |
| `just relations` | workstation | 157 records, 0 failed | — |
| `just site-check` | workstation | browser/HTML/MathML/links and 99 C++17 snippets passed | — |

C++ times are CTest elapsed times; R times are `test_local` elapsed times. They exclude CPU-slot waits and preceding generation/build steps. The 154 R dev skips are intentional tier exclusions; the full tier has none. No final failures were waived. Existing unrelated compiler warnings remain unchanged. The [validation summary](2026-11-06-paralogis-modes-data/validation-summary.txt) records commands and outcomes, and compressed final logs are retained in the evidence directory.

The workstation used GCC 15.2.0 on an Intel i7-1355U, Node 22.22.1 and Emscripten 6.0.13. Simbox CMake tests used GCC 13.3.0; its R 4.6.1 package compilation used GCC 15.2.0. Heavy local work used `run4`; benchmarks and standalone checkers used `run`. Simbox builds/tests used four CMake/CTest workers. Jobs ran at nice 10 with one BLAS/OpenMP thread per worker. The final simbox runs completed without a disconnect or local fallback. See [environment.json](2026-11-06-paralogis-modes-data/environment.json).

**Open numerical availability.** For `x = nextafter(1, 0)` with scale held at 1, the scan finds a candidate near `3.6445696825298163e17` and its narrow root enclosure succeeds, but global certification remains unresolved. The padded point likelihood lower bound is about -31036, while `2*log(a)` at the smallest positive double is only about -1488.88. Consequently the existing small-shape tail comparison cannot close using that lower bound. Returning this candidate requires a tighter stable feasible-likelihood bound. The fitter retains `NumericalFailure`, and the [precision-stop checker](2026-11-06-paralogis-modes-data/precision-stop.cpp) verifies that refusal; [its output](2026-11-06-paralogis-modes-data/precision-stop.tsv) records the candidate, enclosure progress and limiting bounds. This is a finite-MLE availability limit, not a nonexistence claim. The requested `exp(-0.001)` counterexample is fully resolved.

Reproduce the suites from the worktree root, reserving CPU slots as shown:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/simbox-run bash -c 'set -e; export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4; just cpp-test; UMVL_TEST_TIER=full just cpp-test; just cpp-test-no-long-double; just test; just test-full'
# On exit 75, rerun that suite sequence locally through /tmp/umvl-cpu/run4.
/tmp/umvl-cpu/run4 just js-build
/tmp/umvl-cpu/run4 just js-test
/tmp/umvl-cpu/run4 just relations
/tmp/umvl-cpu/run4 just site-check
/tmp/umvl-cpu/run node dev/research/evaluations/2026-11-06-paralogis-modes-data/modes.mjs
```

Compile the retained C++ checkers through `run` with `c++ -O2 -std=c++17`, `-IunivariateML-cpp/include`, and a plain Boost include path. The benchmark additionally needs `-IunivariateML-cpp/tests`. The runs obtained Boost headers with `Rscript -e 'cat(system.file("include",package="BH"))'`; the executables do not link an R runtime. Compile the same benchmark against the named base and fingerprinted final sources and run three interleaved pairs; `analyze.py` reconstructs the timing table. Run `oracle.py` through `run` for the independent arbitrary-precision calculation.
