# TASK-215: modifier-route quantiles

Starting tree: `c167a97580db656bba2e8153d970cd4de69c8f64`. All **58,072 C++** and **59,234 R** eligible cases now satisfy the first adjusted crossing: **zero upward, invalid or crossing failures**. Before there were 914 C++ and 999 R crossing failures. The 628 earlier answers after the change, in each interface, are permitted crossings of atoms smaller than the requested band.

**Rule.** The existing `special::discrete_reached` rule uses `e = 64 * DBL_EPSILON` relative to the supplied number, including a supplied logarithm. Targets are lower linear `p*(1-e)`, upper linear `p*(1+e)`, lower log `p*(1+e)` and upper log `p*(1-e)`, with inclusive lower `>=` and upper `<=` crossings and the existing endpoint/overflow/near-one guards. For a window `W=[L,U]`, compare the conditional quantities `F_W(k)=Pr(L≤X≤k)/Pr(W)` or `S_W(k)=Pr(k<X≤U)/Pr(W)` on that supplied scale. The band is relative to the conditional probability or log probability, rather than a parent probability after adding a discarded prefix. Integer inversion brackets and verifies the first crossing. At a first atom whose interval subtraction is unresolved, the R inverse uses its mass `d(k_min)/Pr(W)`; the interior resolution guard remains.

The modifier kernels, signed finite-zeta evaluator, R composition and JS integer composition follow that rule. Half-atom shortcuts no longer bypass the public comparison. Sampling inverses use unadjusted comparisons. Finite-window inversion expands its bracket from the retained lower bound, avoiding remote finite-size likelihood-ratio walks. First-crossing correctness follows from monotonicity, conditional on the computed comparisons; the fixture counts and sampler replays are numerical regression evidence.

**Round trips.** Each cell gives raw equality misses `q(p(k)) != k`, **before→after**, in the order **lower linear / lower log / upper linear / upper log**. Earlier answers are accepted only at the first adjusted crossing; the tests require equality when the preceding atom is outside the band. Eligible probabilities are interior and non-flat. Original fixture windows contain 201 counts; supplemental finite/one-sided/whole windows contain five counts at every parameter point. The C++ column is native precision; the full double-mode suite repeats the assertions. Complete eligible counts and error categories are in [counts.csv](2026-11-13-qtie-2-data/counts.csv), with points/windows in [cases.json](2026-11-13-qtie-2-data/cases.json) and raw [cpp-before.csv](2026-11-13-qtie-2-data/cpp-before.csv), [cpp-after.csv](2026-11-13-qtie-2-data/cpp-after.csv), [r-before.csv](2026-11-13-qtie-2-data/r-before.csv) and [r-after.csv](2026-11-13-qtie-2-data/r-after.csv).

| Configuration | C++ raw misses | R raw misses |
|---|---|---|
| pois [0, 5] | 9→0 / 3→0 / 3→0 / 3→0 | 9→0 / 3→0 / 3→0 / 3→0 |
| pois [0, ∞] | 0→0 / 0→0 / 0→0 / 0→0 | 0→0 / 0→0 / 0→0 / 0→0 |
| pois [2, 12] | 13→0 / 3→0 / 3→0 / 3→0 | 13→0 / 3→0 / 3→0 / 3→0 |
| pois [2, ∞] | 9→0 / 2→0 / 2→0 / 4→0 | 9→0 / 2→0 / 2→0 / 4→0 |
| pois finite-window fixtures | 25→0 / 10→0 / 10→0 / 12→0 | 25→0 / 10→0 / 10→0 / 12→0 |
| pois inflated | 10→8 / 1→0 / 1→0 / 1→0 | 10→8 / 1→0 / 1→0 / 1→0 |
| pois modified | 11→11 / 4→0 / 0→0 / 0→0 | 11→11 / 4→0 / 0→0 / 0→0 |
| pois none | 7→7 / 0→0 / 0→0 / 0→0 | 7→7 / 0→0 / 0→0 / 0→0 |
| pois truncated | 8→8 / 0→0 / 0→0 / 0→0 | 8→8 / 0→0 / 0→0 / 0→0 |
| binom [0, 5] | 6→0 / 3→0 / 6→0 / 4→0 | 6→0 / 3→0 / 6→0 / 4→0 |
| binom [0, ∞] | 0→0 / 0→0 / 0→0 / 0→0 | 0→0 / 0→0 / 0→0 / 0→0 |
| binom [2, 12] | 11→0 / 9→0 / 11→0 / 11→0 | 11→0 / 9→0 / 11→0 / 11→0 |
| binom [2, ∞] | 11→0 / 10→0 / 12→0 / 12→0 | 11→0 / 10→0 / 12→0 / 12→0 |
| binom finite-window fixtures | 24→0 / 13→0 / 19→0 / 12→0 | 24→0 / 13→0 / 19→0 / 12→0 |
| binom inflated | 28→1 / 2→0 / 7→0 / 0→0 | 28→1 / 2→0 / 7→0 / 0→0 |
| binom modified | 1→1 / 6→0 / 0→0 / 0→0 | 1→1 / 6→0 / 0→0 / 0→0 |
| binom none | 1→1 / 0→0 / 0→0 / 0→0 | 1→1 / 0→0 / 0→0 / 0→0 |
| binom truncated | 1→1 / 0→0 / 0→0 / 0→0 | 1→1 / 0→0 / 0→0 / 0→0 |
| nbinom [0, 5] | — | 8→0 / 5→0 / 5→0 / 5→0 |
| nbinom [0, ∞] | — | 6→0 / 0→0 / 2→0 / 0→0 |
| nbinom [2, 12] | — | 0→0 / 0→0 / 0→0 / 0→0 |
| nbinom [2, ∞] | — | 1→0 / 0→0 / 7→0 / 0→0 |
| nbinom inflated | 127→88 / 38→0 / 29→0 / 28→0 | 127→88 / 38→0 / 29→0 / 28→0 |
| nbinom modified | 131→92 / 44→0 / 29→0 / 28→0 | 131→92 / 44→0 / 29→0 / 28→0 |
| nbinom none | 48→48 / 0→0 / 0→0 / 0→0 | 48→48 / 0→0 / 0→0 / 0→0 |
| nbinom truncated | 56→56 / 0→0 / 0→0 / 0→0 | 56→56 / 0→0 / 0→0 / 0→0 |
| geom [0, 5] | — | 4→0 / 2→0 / 2→0 / 2→0 |
| geom [0, ∞] | — | 4→0 / 0→0 / 1→0 / 0→0 |
| geom [2, 12] | — | 0→0 / 0→0 / 0→0 / 0→0 |
| geom [2, ∞] | — | 2→0 / 0→0 / 2→0 / 0→0 |
| geom inflated | 127→88 / 37→0 / 29→0 / 30→0 | 127→88 / 37→0 / 29→0 / 30→0 |
| geom modified | 107→68 / 40→0 / 29→0 / 28→0 | 107→68 / 40→0 / 29→0 / 28→0 |
| geom none | 30→30 / 0→0 / 0→0 / 0→0 | 30→30 / 0→0 / 0→0 / 0→0 |
| geom truncated | 30→30 / 0→0 / 0→0 / 0→0 | 30→30 / 0→0 / 0→0 / 0→0 |
| lgser modified | 42→42 / 3→0 / 0→0 / 0→0 | 42→42 / 3→0 / 0→0 / 0→0 |
| lgser none | 48→48 / 0→0 / 0→0 / 0→0 | 48→48 / 0→0 / 0→0 / 0→0 |
| zeta finite-window fixtures | 0→0 / 0→0 / 0→0 / 0→0 | 5→0 / 11→0 / 4→0 / 7→0 |
| zeta signed/zero windows | 4→0 / 14→0 / 3→0 / 4→0 | 4→0 / 14→0 / 3→0 / 4→0 |

Appended tests cover all retained configurations in both interfaces, dyadic-probability truncated/inflated binomial rational ties, positive geometric ties, signed finite-zeta rational masses, and the conditional geometric masses `(8,4,2,1)/15`. The independent rational generator records fractions and 100-digit log conversions: [exact.py](2026-11-13-qtie-2-data/exact.py), [exact.json](2026-11-13-qtie-2-data/exact.json). Existing public log-half/subnormal-odds expectations were updated for the requested band; raw sampling comparisons remain checked. The original R first-atom/refusal regression was retained and passes. JS tests additionally check conditional rational jumps and separation of public fuzz from sampling. A diffuse-geometric regression checks the final finite binary64 bracket.

**Samplers and fits.** C++ replay: **92,160 draws, byte-identical**; R replay: **398 models, 135,824 entries, identical**, including the 512 inherited NaNs noted below. Original-kernel replays use 512 draws per point; supplemental R windows use 16. See [sampler-cpp-comparison.txt](2026-11-13-qtie-2-data/sampler-cpp-comparison.txt), [sampler-r-comparison.txt](2026-11-13-qtie-2-data/sampler-r-comparison.txt) and the compressed replay files. Actual WASM/JS replay additionally checked **59,218 eligible round trips** (990 crossing failures before, zero after; 628 allowed earlier answers) and **135,809 identical sampler draws**. See [js-composition-counts.json](2026-11-13-qtie-2-data/js-composition-counts.json.gz) and [js-sampler-layer-comparison.json](2026-11-13-qtie-2-data/js-sampler-layer-comparison.json). The extra JS replay uses one count/draw for the expensive huge-binomial one-sided point; C++/R retain their full supplemental grids. Reproduce with [build-js-baseline.sh](2026-11-13-qtie-2-data/build-js-baseline.sh) and [probe-js.mjs](2026-11-13-qtie-2-data/probe-js.mjs). The C++ stream SHA-256 is `f150f268d16305a8dd5e35b4728025c85c24d3f52f88cde1b0f26e7c14d5ab75`.

Both precision modes have **0/1,934 fit differences** against the unchanged starting tree, ignoring no fields: [starting-final-native-diff.txt](2026-11-13-qtie-2-data/starting-final-native-diff.txt), [starting-final-double-diff.txt](2026-11-13-qtie-2-data/starting-final-double-diff.txt). Both also have **0/1,934 differences** against the supplied retirement snapshots, ignoring only the three requested outcome fields: [retirement-final-native-diff.txt](2026-11-13-qtie-2-data/retirement-final-native-diff.txt), [retirement-final-double-diff.txt](2026-11-13-qtie-2-data/retirement-final-double-diff.txt). The unchanged starting tree also matched those retirement snapshots; no inherited Poisson/hypergeometric repair difference occurred on this grid. Snapshots and compiler/source provenance are retained beside these comparisons.

**Final suites.** The runs below finished on the **workstation**, sequentially through `/tmp/umvl-cpu/run4`, with pinned worker threads and nice priority. C++ uses GCC 15. No simbox run was attempted.

| Command | Result | Evidence |
|---|---|---|
| `just cpp-test` | 109/109 passed | [final-cpp-test.log](2026-11-13-qtie-2-data/final-cpp-test.log) |
| `just cpp-test-no-long-double` | 153/153 passed (workstation GCC 15) | [final-cpp-test-no-long-double.log.gz](2026-11-13-qtie-2-data/final-cpp-test-no-long-double.log.gz) |
| `just test` | Initial: 1 stale tie expectation; targeted correction: 280 passed, 0 failures/warnings, 3 full-only skips; subsequent full suite green | [final-test.log.gz](2026-11-13-qtie-2-data/final-test.log.gz) |
| `just test-full` | 65,256 passed; 0 failures/warnings; 1 missing-reference skip | [final-test-full.log.gz](2026-11-13-qtie-2-data/final-test-full.log.gz) |
| `just js-build` | Build passed | [final-js-build.log](2026-11-13-qtie-2-data/final-js-build.log) |
| `just js-test` | Initial: 579 passed, 1 stale ZIB tie expectation; targeted correction and modifier rechecks: 5/5 passed | [final-js-test.log](2026-11-13-qtie-2-data/final-js-test.log) |

The dev R run found one stale ZIB log-half expectation (10 instead of the new public answer 0). Only that expectation was corrected; its targeted recheck passed 280 assertions, and the subsequent full R suite passed all 65,256. The JS suite similarly passed 579 checks and found the same stale ZIB log-half expectation in its existing test. Its targeted correction and both new JS modifier tests passed (5/5 selected checks). These late changes were confined to tests, so full suites were not repeated, following the requested targeted-recheck rule; see [final-js-test-recheck.log](2026-11-13-qtie-2-data/final-js-test-recheck.log). See [final-test-zib-recheck.log](2026-11-13-qtie-2-data/final-test-zib-recheck.log).

The final source changes are modifier quantile/sampling headers under `pois`, `binom`, `nbinom`, `geom`, `lgser` and `zeta`, `bindings/r/R/modifiers.R`, the JS modifier composition/sampling adapter, and modifier tests; [source-files.txt](2026-11-13-qtie-2-data/source-files.txt) lists every source file. Fit algorithms, frozen fixtures and fit expectations were retained. Reproduce measurements with [run-probes.sh](2026-11-13-qtie-2-data/run-probes.sh) (`before`, then `after`); it reconstructs the baseline commit into ignored scratch rather than retaining a source archive. Fit snapshots use the original brief's `fit-snapshot.py --modifier-routes` commands.

**Open inherited evaluation gaps.** These were measured before the change and remain explicit, not successful round trips. The 13 empty conditioning events are expected refusals; the three other constructor refusals are listed in [r-before-exclusions.json](2026-11-13-qtie-2-data/r-before-exclusions.json) and [r-after-exclusions.json](2026-11-13-qtie-2-data/r-after-exclusions.json).

1. NB `size=1e8, prob=.5`, windows `[2,12]` and `[0,5]`: R refuses the conditional normalizer at its existing floating-point resolution guard. Missing: a stable conditional normalizer for those extremely small events. The probability evaluator was not loosened.
2. Base `lgser`, `zero="none", theta=0`: the R frozen-model domain rejects this boundary, while the C++ evaluator includes the point-mass limit. Missing: a consistent R domain/closure decision; changing held fits was outside this job.
3. Base `nbinom`, `zero="none", size=0, prob=.5`: R's unchanged `stats::rnbinom` delegate returns NaNs (512/512 in the replay); the C++ sampler returns zero. Missing: R boundary-sampler handling. This pre-existing stream was preserved as required. Reproducer and case: [inspect-samplers.R](2026-11-13-qtie-2-data/inspect-samplers.R), [sampler-baseline-limits.jsonl](2026-11-13-qtie-2-data/sampler-baseline-limits.jsonl).
