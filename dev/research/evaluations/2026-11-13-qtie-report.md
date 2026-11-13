# TASK-215: discrete quantile ties

Baseline: `fbc13bf8`; worktree `codex/qtie`. All numerical runs, builds and tests used the workstation. Simbox was not contacted.

The public quantiles of all 20 surviving discrete families now share a supplied-scale left-continuity comparison. Across the truth-parameter probe, upward round-trip errors fell from **3,939 to zero**. The before/after sampler replay contains **44,032 identical draws**. No fitter, golden or truth-table file changed.

## Implemented rule

Let `e = 64 * DBL_EPSILON`. After validating the input and handling exact probability endpoints, return the first support point whose computed tail reaches the following target:

| Supplied mode | Adjusted target | Crossing |
|---|---|---|
| Lower, linear | `p * (1-e)` | computed lower tail `>= target` |
| Upper, linear | `p * (1+e)` if `1-p > 4*e`; otherwise `p` | computed survival `<= target` |
| Lower, log | `p * (1+e)` if `p > -DBL_MAX/(1+e)`; otherwise `p` | computed log lower tail `>= target` |
| Upper, log | `p * (1-e)` | computed log survival `<= target` |

The implementation is `special::discrete_reached` in `special/tails.hpp`. NaN evaluations still report failure. Generic inversion and every surviving public family search predicate use this helper; seed shortcuts must also certify the first adjusted crossing. The existing representable-double support lattice is retained beyond exact unit-integer resolution.

The adjustment is relative to the **supplied number**, including when that number is a logarithm. It is not an additive `log1p(-e)` adjustment to a log input. A near-one lower linear probability can therefore move across several atoms; there is no one-neighbour clamp. The upper linear guard prevents adjusting a probability through one. A tiny subnormal linear probability may round back to itself after multiplication. Log inputs avoid exponentiation and its underflow; the lower extreme-log guard keeps the target finite. Exact zero/one endpoints, and their log equivalents, keep their endpoint semantics.

The R-style scale multiplication and upper near-one guard follow [R's discrete search implementation](https://github.com/wch/r-source/blob/R-4-6-branch/src/nmath/qDiscrete_search.h). The explicit uniform **64** requirement is retained here: R 4.6 actually uses 8 epsilon for ordinary probabilities and 2 for logs, uses a strict upper comparison, and has different extreme-log overflow handling. This implementation uses the inclusive mirror crossing above and suppresses the overflowing lower-log multiplication instead of reversing its direction. Moreover, [R's qgeom](https://github.com/wch/r-source/blob/R-4-6-branch/src/nmath/qgeom.c) subtracts `1e-12` on the count scale, while [R's qhyper](https://github.com/wch/r-source/blob/R-4-6-branch/src/nmath/qhyper.c) uses a 1000-epsilon adjustment after lower-tail conversion. Thus this is the requested uniform convention, not bitwise emulation of every R quantile. The specification ambiguity is recorded below.

## Implementation and numerical repairs

The 16 edited family kernels are bbinom, cmb, cmp, dgamma, dlaplace, dnorm, dunif, dweibull, geom, hyper, nhyper, pig, pois, skellam, yulesimon and zeta. Binom, bnbinom, nbinom and lgser inherit the generic change without a kernel edit. Public comparisons no longer switch silently to a different tail or scale. The former CMB underflow-median shortcut was removed: a computed half-probability plateau must select its first adjusted crossing.

The comparison grid exposed two CDF accuracy problems that could otherwise produce a wrong adjusted crossing. They were repaired in the kernels rather than hidden with a larger fuzz:

| Case | Reference answer | Old CDF / new policy | Repaired answer |
|---|---|---|---|
| Pois, lambda 20, log-upper input `-0.17021094871159989` | 16 | 15 | 16 |
| Hyper, m 50, N 100, draws 20, log-upper input `-8.7930366634649135e-08` | 0 | 1 | 0 |

For ordinary Poisson counts/rates below `1e6`, tails now use the direct Boost seam; the centered physical gamma chart remains for large exact shapes. At the Poisson boundary the old relative log-tail error was 79.71 epsilon and the repaired error is 0.34 epsilon. Hypergeometric supports of width at most 4096 now normalize modal recurrence weights and form the larger tail from the smaller one; wider supports retain the existing sum. The hypergeometric boundary error fell from about 4.43 million epsilon to 2.13 epsilon. These are numerical comparisons with a 100-digit reference, not interval bounds; see [the CDF reference output](2026-11-13-qtie-data/cdf-reference.json) and its retained scripts.

Bounded thread-local caches reuse unchanged CDF evaluation results for diffuse CMP, PIG, and the expensive large-rate Skellam chart. They have at most 4096 entries, never cache NaNs, and eviction changes speed only. Appended tests compare cached results with the uncached evaluators. This keeps the new scalar round-trip searches practical without changing their answers.

## Round-trip evidence

The C++ tests cover all **201 truth-table parameter points** across the 20 surviving discrete families, with a 201-count window centered at an independent truth-table median. Only interior, non-flat matching-tail probabilities are eligible. All four modes call the corresponding probability routine directly, including logs; `log(P(k))` is not substituted for a matching log-probability call.

The literal assertion `q(P(k)) == k` cannot hold when the requested band crosses an earlier positive atom. Tests require equality when the preceding jump is outside the band; otherwise they require a finite answer no greater than k and certify both sides of the first adjusted crossing. This detects the original upward error while enforcing the stated rule for small atoms. The three unrepresentable bulk windows are listed under open limits.

Before: 62,947 eligible cases, 3,939 upward errors, zero earlier results and zero invalid results. After: 62,950 eligible cases, **zero upward errors**, 886 legitimate earlier results and zero invalid results. The extra three eligible cases come from repaired hypergeometric log-tail resolution. Every earlier result passes the adjusted-crossing checks. “Earlier” is therefore not counted as a defect under this contract.

The table gives every family and mode. Before exact matches equal “before cases minus before upward”; after exact matches equal “after cases minus after earlier”. After upward and invalid counts are zero in every row.

| Family | Mode | Before cases | Before upward | After cases | After earlier |
|---|---|---:|---:|---:|---:|
| bbinom | lower linear | 122 | 25 | 122 | 0 |
| bbinom | lower log | 122 | 0 | 122 | 0 |
| bbinom | upper linear | 123 | 26 | 123 | 0 |
| bbinom | upper log | 123 | 0 | 123 | 0 |
| binom | lower linear | 60 | 16 | 60 | 1 |
| binom | lower log | 152 | 0 | 152 | 0 |
| binom | upper linear | 152 | 7 | 152 | 0 |
| binom | upper log | 152 | 0 | 152 | 0 |
| bnbinom | lower linear | 1803 | 531 | 1803 | 28 |
| bnbinom | lower log | 2016 | 19 | 2016 | 0 |
| bnbinom | upper linear | 2016 | 142 | 2016 | 0 |
| bnbinom | upper log | 2069 | 3 | 2069 | 0 |
| cmb | lower linear | 99 | 11 | 99 | 1 |
| cmb | lower log | 196 | 1 | 196 | 0 |
| cmb | upper linear | 191 | 2 | 191 | 0 |
| cmb | upper log | 196 | 1 | 196 | 0 |
| cmp | lower linear | 768 | 160 | 768 | 14 |
| cmp | lower log | 1284 | 0 | 1284 | 0 |
| cmp | upper linear | 1284 | 96 | 1284 | 0 |
| cmp | upper log | 1529 | 0 | 1529 | 0 |
| dgamma | lower linear | 773 | 198 | 773 | 34 |
| dgamma | lower log | 1221 | 0 | 1221 | 0 |
| dgamma | upper linear | 1220 | 65 | 1220 | 0 |
| dgamma | upper log | 1348 | 0 | 1348 | 0 |
| dlaplace | lower linear | 932 | 1 | 932 | 170 |
| dlaplace | lower log | 994 | 0 | 994 | 151 |
| dlaplace | upper linear | 936 | 1 | 936 | 151 |
| dlaplace | upper log | 999 | 0 | 999 | 151 |
| dnorm | lower linear | 578 | 64 | 578 | 6 |
| dnorm | lower log | 1163 | 7 | 1163 | 0 |
| dnorm | upper linear | 578 | 101 | 578 | 0 |
| dnorm | upper log | 1165 | 0 | 1165 | 0 |
| dunif | lower linear | 420 | 59 | 420 | 0 |
| dunif | lower log | 420 | 2 | 420 | 0 |
| dunif | upper linear | 420 | 54 | 420 | 0 |
| dunif | upper log | 420 | 1 | 420 | 0 |
| dweibull | lower linear | 535 | 207 | 535 | 15 |
| dweibull | lower log | 808 | 112 | 808 | 4 |
| dweibull | upper linear | 808 | 111 | 808 | 4 |
| dweibull | upper log | 1029 | 107 | 1029 | 4 |
| geom | lower linear | 431 | 90 | 431 | 16 |
| geom | lower log | 618 | 4 | 618 | 0 |
| geom | upper linear | 618 | 27 | 618 | 0 |
| geom | upper log | 618 | 0 | 618 | 0 |
| hyper | lower linear | 17 | 0 | 17 | 0 |
| hyper | lower log | 17 | 0 | 20 | 0 |
| hyper | upper linear | 20 | 0 | 20 | 0 |
| hyper | upper log | 20 | 0 | 20 | 0 |
| lgser | lower linear | 574 | 188 | 574 | 8 |
| lgser | lower log | 751 | 0 | 751 | 0 |
| lgser | upper linear | 751 | 25 | 751 | 0 |
| lgser | upper log | 814 | 1 | 814 | 0 |
| nbinom | lower linear | 1266 | 286 | 1266 | 32 |
| nbinom | lower log | 1559 | 12 | 1559 | 0 |
| nbinom | upper linear | 1559 | 145 | 1559 | 0 |
| nbinom | upper log | 1559 | 6 | 1559 | 0 |
| nhyper | lower linear | 241 | 7 | 241 | 0 |
| nhyper | lower log | 241 | 0 | 241 | 0 |
| nhyper | upper linear | 241 | 0 | 241 | 0 |
| nhyper | upper log | 241 | 0 | 241 | 0 |
| pig | lower linear | 737 | 325 | 737 | 48 |
| pig | lower log | 1058 | 1 | 1058 | 0 |
| pig | upper linear | 1058 | 4 | 1058 | 0 |
| pig | upper log | 1124 | 1 | 1124 | 0 |
| pois | lower linear | 343 | 78 | 343 | 8 |
| pois | lower log | 766 | 0 | 766 | 0 |
| pois | upper linear | 766 | 38 | 766 | 0 |
| pois | upper log | 832 | 0 | 832 | 0 |
| skellam | lower linear | 1594 | 160 | 1594 | 16 |
| skellam | lower log | 2349 | 110 | 2349 | 0 |
| skellam | upper linear | 1596 | 184 | 1596 | 0 |
| skellam | upper log | 2350 | 117 | 2350 | 0 |
| yulesimon | lower linear | 567 | 0 | 567 | 12 |
| yulesimon | lower log | 673 | 0 | 673 | 0 |
| yulesimon | upper linear | 673 | 0 | 673 | 0 |
| yulesimon | upper log | 726 | 0 | 726 | 0 |
| zeta | lower linear | 533 | 0 | 533 | 12 |
| zeta | lower log | 604 | 0 | 604 | 0 |
| zeta | upper linear | 513 | 0 | 513 | 0 |
| zeta | upper log | 705 | 0 | 705 | 0 |

Raw counts are in [roundtrip-before.csv](2026-11-13-qtie-data/roundtrip-before.csv) and [roundtrip-final.csv](2026-11-13-qtie-data/roundtrip-final.csv); [summary.json](2026-11-13-qtie-data/summary.json) and [windows.json](2026-11-13-qtie-data/windows.json) retain the aggregation and every window.

The exact-jump generator uses integer binomial coefficients and `fractions.Fraction`. It records 430 rational jumps, of which 401 stay away from sub-band near-one jumps and yield **1,604 explicit four-mode C++ assertions**. These cover four symmetric hypergeometric half ties, including `(m,N,draws)=(5,10,5)` with answer 2, dyadic binomial/negative-binomial/geometric probabilities, and discrete uniform supports. Log literals come from 100-digit logs of the exact fractions, avoiding an intervening rounded ordinary probability. See [exact.py](2026-11-13-qtie-data/exact.py) and [exact.json](2026-11-13-qtie-data/exact.json).

The R generated-d/p/q test file exercises the same matching-mode round trips through `qml(pml(k, fit), fit)`: the development tier samples the first two parameter points per family; the full tier covers every point. Full C++ round-trip cases are classified as slow reference tests. The existing Python truth consumer's relative `1e-12` ambiguity window is unchanged. C++, R and JS truth checks additionally recognize a move caused by this explicit fuzz or an actual computed plateau, while requiring the first adjusted crossing. No `truth.json` was regenerated.

## Comparison with stats

R version: **4.6.1 (2026-06-24)**. The grid uses Poisson lambda 20; binomial size 100, prob .5; geometric prob .25; negative binomial size 10, prob .5; and hypergeometric m 50, N 100, draws 20. Each family uses 21 support values, their matching four-mode probabilities, every neighbour from 1 through 128 ulp on both sides, and 1000 uniform draws per mode from seed 215. Invalid endpoints are excluded. Original 17-digit probability strings are preserved through the R output.

All **127,424** C++ answers pass both sides of the computed adjusted-crossing test. Every C++/stats disagreement is retained in [classified.csv](2026-11-13-qtie-data/classified.csv.gz), including the input, both answers, independent reference answers before and after adjustment, boundary error, and cause. The independent reference uses 400-digit masses with explicit infinite survival remainders; “reference” below is numerical evidence, not an outward-rounded proof.

| Family | Inputs | Disagreements | C++ = unadjusted reference | stats = unadjusted reference | Adjusted-boundary rounding cases | Maximum boundary distance / epsilon |
|---|---:|---:|---:|---:|---:|---:|
| pois | 25588 | 7594 | 17 | 7577 | 44 | 0.8276 |
| binom | 25588 | 7734 | 119 | 7615 | 45 | 1.0581 |
| geom | 25588 | 3681 | 2707 | 974 | 82 | 3.6954 |
| nbinom | 25588 | 7574 | 13 | 7561 | 36 | 2.0000 |
| hyper | 25072 | 2992 | 2196 | 796 | 32 | 2.8286 |

Of the 29,575 disagreements, 5,052 have C++ equal to the unadjusted reference and 24,523 have stats equal to it. The latter are explained by the explicitly different 64-epsilon convention, not failures of its computed-CDF contract. R's geometric count adjustment and hypergeometric adjustment explain cases where its answer precedes the reference.

There are 239 cases where the computed-CDF answer differs from the high-precision adjusted-target reference at a rounding boundary. All are within 3.696 epsilon on the supplied target scale; every computed crossing is correct. No unexplained disagreement remains, and no residual CDF error beyond a four-epsilon boundary margin was found on this grid. This margin is an observed numerical bound for the grid, not a global error certificate. [classification.txt](2026-11-13-qtie-data/classification.txt) gives the cause counts; [grid.csv](2026-11-13-qtie-data/grid.csv.gz) retains every input.

## Samplers and fitters

Public fuzz is separated from historical inverse-sampling comparisons:

- Generic backend inverses in `special.hpp` pass `fuzz=false`, preserving binomial and beta-binomial RNG paths. This also preserves the original smaller-log-tail comparison below 2^53 and supplied-scale comparison above it.
- CMP, CMB, discrete Laplace, Yule–Simon and zeta have sampling paths that retain their original comparisons. CMB's binomial/geometric delegates and CMP's Poisson member use the unfuzzed backend.
- Negative hypergeometric retains its original 1000-epsilon absolute log shift in its sampling inverse. Hypergeometric retains the original extended-precision cumulative sampler and its 1000-epsilon linear shift; converting that sum to the public double comparison would risk changing draws.
- Large-count geometric, discrete-uniform and discrete-Weibull fallback inversions retain their original unfuzzed comparisons. Their ordinary closed-form sampling branches are unchanged. CMP's invalid-input uniform consumption is also tested.

The baseline and current replay files are byte-identical: **44,032 draws across 12 sampler paths**, using 512 draws per selected historical parameter point and additional large/diffuse stress points. Both SHA-256 hashes are `70b92b245cf67df7be07252aef077e3f04b16b895a8386e92d6abc964d5f74e4`. See [samplers.cpp](2026-11-13-qtie-data/samplers.cpp), [before](2026-11-13-qtie-data/samplers-before.csv.gz) and [after](2026-11-13-qtie-data/samplers-after.csv.gz). Fixed-uniform C++ tests also separate fuzzy public quantiles from historical sampling inverses.

No direct discrete-quantile call was found in fitter implementations, and no `mle.hpp` changed. R frozen-model validation in `bindings/r/R/frozen.R` and held/modifier validation in `bindings/r/R/modifiers.R` do evaluate a median via `qml`; both files are unchanged, and the full model/frozen/golden tests passed. Golden files and fit expectations were not changed. R/JS inverse-replay assertions now allow the deliberate difference between a public fuzzy inverse and its preserved sampler, and check the probability band when a recorded replay differs.

## Final verification

All runs below used the **workstation**, through the required CPU-slot wrappers, with their nice/thread controls. No simbox run was attempted.

| Check | Result | Evidence |
|---|---|---|
| `just cpp-test` | 109/109 passed | [log](2026-11-13-qtie-data/final-cpp-test.log.gz) |
| `just cpp-test-no-long-double` | 153/153 passed; workstation GCC 15 | [log](2026-11-13-qtie-data/final-cpp-test-no-long-double.log.gz) |
| `just test` development coverage | Initial run: 31,862 passed, 3 stale policy assertions; corrected CMP/dnorm recheck: 229 passed, 0 failures/warnings, 1 full-only skip | [initial log](2026-11-13-qtie-data/initial-test-dev.log.gz), [targeted final log](2026-11-13-qtie-data/targeted-r.log.gz) |
| `just test-full` after those corrections | 63,403 passed; 0 failures, warnings or skips | [log](2026-11-13-qtie-data/final-test-full.log.gz) |
| `just js-build` | passed; existing binomial self-comparison compiler warnings | [log](2026-11-13-qtie-data/final-js-build.log.gz) |
| `just js-test` final rerun | 642/642 passed; 0 failures/skips | [log](2026-11-13-qtie-data/final-js-test.log.gz) |

The R late fixes changed only the CMP and dnorm family tests: old unfuzzed crossing assertions and the old dnorm log-half expectation. Following the requested late-fix rule, only those files were rerun at development tier; the subsequent full R run covered the entire final R state.

The first JS run passed 641 tests and found one old unfuzzed large-location normal reference. Its correction uses the adjusted probability in the continuous standard-normal reference, retains the same tolerance, and retains the independent first-crossing checks. Both targeted JS regressions passed; the shared JS test file was then checked again by the complete 642-test run. No production code changed after the passing native C++ runs. The initial and targeted JS logs are also retained as [initial](2026-11-13-qtie-data/initial-js-test.log.gz) and [targeted](2026-11-13-qtie-data/targeted-js.log.gz).

## Files and reproduction

Exact edited source-file list, including the new shared test helper:

```text
bindings/js/test.mjs
bindings/r/distributions/cmp/test.R
bindings/r/distributions/dnorm/test.R
bindings/r/tests/testthat/test-generated-dpqr.R
univariateML-cpp/include/cppunivariate/distributions/README_SCHEMA.md
univariateML-cpp/include/cppunivariate/distributions/bbinom/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/bbinom/test.cpp
univariateML-cpp/include/cppunivariate/distributions/binom/test.cpp
univariateML-cpp/include/cppunivariate/distributions/bnbinom/test.cpp
univariateML-cpp/include/cppunivariate/distributions/cmb/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/cmb/test.cpp
univariateML-cpp/include/cppunivariate/distributions/cmp/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/cmp/test.cpp
univariateML-cpp/include/cppunivariate/distributions/dgamma/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/dgamma/test.cpp
univariateML-cpp/include/cppunivariate/distributions/dlaplace/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/dlaplace/test.cpp
univariateML-cpp/include/cppunivariate/distributions/dnorm/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/dnorm/test.cpp
univariateML-cpp/include/cppunivariate/distributions/dunif/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/dunif/test.cpp
univariateML-cpp/include/cppunivariate/distributions/dweibull/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/dweibull/test.cpp
univariateML-cpp/include/cppunivariate/distributions/geom/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/geom/test.cpp
univariateML-cpp/include/cppunivariate/distributions/hyper/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/hyper/test.cpp
univariateML-cpp/include/cppunivariate/distributions/lgser/test.cpp
univariateML-cpp/include/cppunivariate/distributions/nbinom/test.cpp
univariateML-cpp/include/cppunivariate/distributions/nhyper/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/nhyper/test.cpp
univariateML-cpp/include/cppunivariate/distributions/pig/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/pig/test.cpp
univariateML-cpp/include/cppunivariate/distributions/pois/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/pois/test.cpp
univariateML-cpp/include/cppunivariate/distributions/skellam/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/skellam/test.cpp
univariateML-cpp/include/cppunivariate/distributions/yulesimon/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/yulesimon/test.cpp
univariateML-cpp/include/cppunivariate/distributions/zeta/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/zeta/test.cpp
univariateML-cpp/include/cppunivariate/special/special.hpp
univariateML-cpp/include/cppunivariate/special/tails.hpp
univariateML-cpp/tests/discrete_roundtrips.hpp
univariateML-cpp/tests/distribution_tests_main.cpp
univariateML-cpp/tests/test_support.hpp
```

The report and all study scripts/outputs are under `dev/research/evaluations/2026-11-13-qtie-*`. The 16 retiring family folders, fitter/adaptor outcome work, code generators, `modifiers.R`, status/roadmap files and NEWS were not edited.

Run the retained drivers from the worktree root, without an outer slot wrapper: [run-probes.sh](2026-11-13-qtie-data/run-probes.sh), [run-comparison.sh](2026-11-13-qtie-data/run-comparison.sh), and [run-cdf-probe.sh](2026-11-13-qtie-data/run-cdf-probe.sh) acquire slots for each computation themselves. The baseline driver reconstructs headers with read-only `git archive fbc13bf8` into ignored scratch; no source archive or baseline tree is retained in the evidence. Run `windows.py` and `summarize.py` through `/tmp/umvl-cpu/run`. The rational generator is intentionally append-only/idempotent for test files.

## Open specification and representation limits

1. **Three bulk windows cannot be represented.** Bnbinom point 11, parameters `[1, 1e-8, 1]`; Yule–Simon point 6, `[1e-8]`; and zeta point 4, `[1.0000000000000002]` have oracle medians beyond finite binary64 range. Their 201-count probe window is centered at zero; existing overflow/lattice truth cases remain in force. A literal 200-point unit-count window around their bulk would require an arbitrary-precision support API and corresponding oracle coordinates, neither supplied by this task.
2. **Unconditional non-flat round-trip equality conflicts with the requested band.** A positive atom can still be smaller than 64 epsilon on the comparison scale. The tests enforce exact equality outside that circumstance and first adjusted crossing inside it. Satisfying the literal unconditional equality would require restricting the tested masses or changing the fuzz rule; the implementation follows the fuzz rule.
3. **Uniform 64 and literal R emulation are different policies.** The request specifies 64 but also says to follow R where it differs. The implemented decision is uniform 64 with the explicit finite lower-log guard and inclusive crossings above. Literal R 4.6 emulation would instead require its 8/2 constants, strict upper comparison, overflow-side choice, and family-specific geometric/hypergeometric conventions. That would be a contract change, not an unexplained numerical discrepancy.
4. **Artifact-pane delivery is blocked by the viewer's roots.** The required `bin/show` command returned HTTP 403: this worktree is outside every root registered in Nook. The report is saved at the requested path and all 28 report links were checked. Display requires opening a Nook agent thread in this worktree or adding this path to the viewer's configured roots. No viewer configuration or other worktree was changed.

No unclassified numerical disagreement remains in the comparison grid.


Note added at the merge: `grid.csv`, `classified.csv`, `disagreements.csv`,
`samplers-before.csv` and `samplers-after.csv` are stored gzipped (12 MB
otherwise). The retained scripts write and read the plain files; `gunzip`
them first to rerun a step.
