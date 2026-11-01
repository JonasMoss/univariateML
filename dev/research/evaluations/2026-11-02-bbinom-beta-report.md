# TASK-113: beta-binomial free size with beta held

Tested source: `6fea876ebeec88594a6de4b8af0db4adb5a7d452` plus the uncommitted changes in this worktree. The held-beta subset is implemented. Held alpha and both shapes free remain unsupported. Goldens and the settled d/p/q/r code were not changed.

## Mathematics and implementation

The [distribution dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md) gives the derivation, the fixed-size score bounds and the face classification. At fixed size the log likelihood need not be concave in log alpha: for a single observation `x = N = 2`, `beta = 10`, its second derivative in log alpha is positive near zero. The fixed-size optimizer brackets stationary points, refines candidates, and uses tangent/chord upper bounds over the complete bracket; it does not assume unimodality. Its acceptance allowance is `1e-8 * sample_size` in log likelihood. Unresolved optimization or a failed bound stops.

For maximum count `M`, positive-count frequency `m`, sample size `h`, and held beta `b`, comparison of consecutive probabilities shows that the profile is nonincreasing after the first `N >= M` satisfying

```text
m * (N + 1 - M) >= h * M * (b - 1) * sum(j=0,...,N) 1/(b+j).
```

For `b <= 1`, it is nonincreasing from `M`. For `b > 1`, the score at size `N+1` is positive below `m/(h H)`, with `H` the displayed harmonic sum; every finite profile maximizer is above that value. At such alpha the exact consecutive-size probability ratio is at most one for every observed count. The left-hand factor divided by `H` increases with `N`, so the condition persists. This proves the tail comparison in exact arithmetic; it is stronger for practical enumeration than the previous joint logarithmic tail bound, not a proof that the entire profile is monotone.

The search evaluates each size through the first certified cutoff, stops if that cutoff exceeds 1000, propagates the fixed-size likelihood allowances, and stops on a cross-size tie within `2e-8 * h`. The cutoff comparison uses a relative margin `1 + 256 * epsilon * (1001 + b)`. These are practical floating-point margins, not outward-rounded interval arithmetic; the certificate is conditional on the computed comparisons. A candidate returned as `Ok` is within the fixed-size likelihood allowance of its profile maximum, and its size is separated from the other enumerated sizes at the stated margin. Failure to establish an interval bound, a size comparison, or the tail cutoff stops instead of returning an unchecked estimate.

All-zero data attain likelihood one at size zero and leave alpha unidentified. For constant positive data the point mass at the common count is a supremum, not an attained finite-shape fit. For nonconstant data the fixed-size endpoint faces have zero likelihood, the earlier uniform tail bound tends to zero, and a finite maximum exists. The size search may nevertheless stop with a budget or numerical failure. The R binding reports these outcomes, and generated metadata exposes the held-beta subset. The common binding also serves the JavaScript interface.

## Cutoff workload

[Reproducer](2026-11-02-bbinom-beta-data/cutoffs.py) and [all 500 sample results](2026-11-02-bbinom-beta-data/cutoffs.csv): Python 3.14.5, seed 113, 100 independently generated samples per row. Each observation draws a beta probability and then a binomial count. Cutoffs use the mathematical inequality without the tiny implementation rounding margin. The final column counts cutoffs above 1000, which trigger the implemented budget stop before profile enumeration; it is not a measurement of runtime or other numerical failures.

| Generating size, alpha, beta; sample size | Median cutoff | Range | Above budget |
|---|---:|---:|---:|
| 5, 2, 0.5; 30 | 5 | 5–5 | 0/100 |
| 10, 2, 3; 30 | 78 | 55–94 | 0/100 |
| 10, 2, 3; 100 | 84 | 70–93 | 0/100 |
| 20, 0.5, 8; 30 | 397 | 65–1257 | 4/100 |
| 40, 5, 20; 100 | 1639 | 1296–2526 | 100/100 |

These are sampled workloads, not population guarantees. No samples in this workload were constant. The cap limits the largest enumerated size, while the fixed-size interval certificate separately limits its own work.

## Validation

All compute ran locally under `/tmp/umvl-cpu/run` or `run4`, with the supplied single-thread BLAS/OpenMP settings. Environment: Linux x86-64 7.0.0-38-generic, GCC 15.2.0, R 4.6.1. Temporary and test logs were kept under `build/`. No simbox, package installation, commit or push was used. The site dependency hard-link copy failed with `Invalid cross-device link` under the sandbox; a regular copy of the existing dependency tree into this worktree succeeded.

C++ and R tests cover a finite held-beta fit, a point-mass stop, all-zero nonidentification, a budget stop, held metadata, and dense fixed-size enumeration through size 40 on six seeded nonconstant samples in each language (`beta = 0.5` or `1.2`; log-likelihood tolerance `1e-7`). These compare the free-size result to the existing certified fixed-size solver over a finite grid; they are not an independent high-precision proof. Conformance accepts the explicit budget or unresolved-tie outcomes. Existing fixed-size goldens remain unchanged. The cutoff simulation checks the formula's practical sizes, not the optimizer's coverage or floating-point error.

| Final command | Result, local workstation |
|---|---|
| `just cpp-test` | Pass, 106/106 development shards. |
| `just cpp-test-no-long-double` | Pass, 127/127 tests. |
| `just test` | Pass: 20,117 passes, 0 failures, 0 warnings, 151 skips. |
| `just test-full` | Pass: 41,720 passes, 0 failures, 0 warnings, 0 skips. |
| `just js-build` | Pass. |
| `just js-test` | Pass, 413/413 tests. |
| `just site-check` | Pass. |
| `git diff --check` | Pass. |

Open validation limits: there is no outward-rounded proof of the floating-point comparisons, no independent high-precision sweep of the new fixed-size and cross-size certificates, and no explicit constructed near-tie test for the tie stop. The seeded brute-force checks exercise small counts and two held-beta values. The earlier held-alpha and unrestricted-size tail/face comparisons remain open as described in the [prior investigation](2026-11-01-bbinom-size-report.md).
