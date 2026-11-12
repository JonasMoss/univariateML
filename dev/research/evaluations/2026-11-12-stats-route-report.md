# TASK-198: R evaluation through the native d/p/q

The 14 former `stats` families now evaluate `dml`, `pml` and `qml` through generated internal C++ wrappers: beta, gamma, hyper, lnorm, cauchy, logis, unif, exp, binom, pois, weibull, geom, nbinom and norm. Sampling retains each existing `stats::r*` function, including its RNG stream. Work is uncommitted against `85f2bb6790d7678b780c3bf104b5c50c3e48125a`.

## Implementation

Each dossier now declares `r.density: core_d<id>` and retains its sampler. The generator emits `core_d*`, `core_p*` and `core_q*`, handles Rmath macro collisions, and validates numeric inputs before entering Rcpp. These wrappers preserve attributes from the first longest numeric argument, matching the former R Math convention. The schema documents the split between evaluation and sampling.

Dispatch resolves recognized models against current metadata, including saved fits whose density attribute still names `stats`. Hypergeometric evaluation passes the native coefficients `m,N,k`; its sampler alone applies the existing `m,n=N-m,k` argument map. All-held fits and modifier base-law evaluations use the same resolver. Modifier algorithms were not restructured.

The truth-table consumer now includes the 14 generated R interfaces. It skips unregistered, metadata-less scaffolds such as expweibull. Tests cover golden likelihood seams, ordinary comparisons with explicit tolerance, vectorization, recycling, attributes, invalid storage types, missing values, infinities, support edges, discrete warnings, tails, log probabilities and unchanged sampling streams. Existing stats comparisons remain useful cross-checks rather than exact-output requirements.

No fitter, native numerical header, golden sample or truth fixture changed. The required JS/site generation also refreshed a stale derived modifier inventory: its sole metadata addition is the already-existing gengamma family.

## Numerical evidence and material differences

The reproducible [comparison script](2026-11-12-stats-route-data/compare.R) checks 135 valid parameter points: golden and simulation coefficients plus independent ordinary coefficient variations. It checks d/p/q in both tail and log modes, including support boundaries and bulk locations. [All grid differences above 1e-12 relative](2026-11-12-stats-route-data/ordinary-changes.csv) are retained with hexadecimal binary64 inputs and outputs; [the complete summary](2026-11-12-stats-route-data/ordinary-summary.csv) also records smaller differences. Relative error uses the stats value as denominator, with absolute error when that value is zero. Matching missing results count as equal.

There are no density or CDF differences above 1e-12 relative on this grid. The 46 material quantile rows, including repeated parameter configurations, comprise:

| Family | Rows | Explanation |
|---|---:|---|
| logis | 24 | Near-zero quantiles amplify small absolute differences; maximum absolute difference 1.11e-14. |
| norm | 8 | Near-zero quantiles, including log-probability conversion; maximum absolute difference 2.91e-15. |
| hyper | 6 | Lattice-jump comparisons and the stats negative-infinite log-probability endpoint described below. |
| geom | 4 | Upper-tail probability immediately below one half: native result 1 versus stats 0. |
| nbinom | 4 | The same jump for size 1, probability one half. |

For the 32 normal/logistic rows, an independent [100-digit inversion](2026-11-12-stats-route-data/near-zero-reference.py) starts from the exact binary64 probability, taking the exponential at high precision for log inputs. The [reference results](2026-11-12-stats-route-data/near-zero-reference.csv) give maximum relative error 1.69e-16 for the native route and 0.5 for stats. This is numerical evidence for improved handling of cancellation and log probabilities, not a proof over the parameter domain.

For geometric probability one half, the upper-tail quantile immediately below one half is exactly 1; stats' fuzz around a jump returns 0. The same statement holds for negative binomial size 1. At exactly one half both return 0. These are mathematical jump comparisons; [adjacent binary64 probes](2026-11-12-stats-route-data/median-boundaries.csv) retain the values.

R 4.6.1 `qhyper(-Inf, ..., log.p=TRUE)` produces NaN. The native interface returns the appropriate support endpoint, consistent with probability zero in the corresponding tail. The ordinary test explicitly allows this correction.

Additional boundary and invalid-parameter probes are in [conventions.json](2026-11-12-stats-route-data/conventions.json). The [124 material value differences](2026-11-12-stats-route-data/convention-changes.csv) include invalid/degenerate parameter conventions and near-integer probes; they are separate from the ordinary grid. One valid near-boundary continuous difference is gamma at probability `1 - 1e-12` and its golden coefficients: native 14.981745219178348 versus stats 14.981745219039302 (9.28e-12 relative). An independent [100-digit upper incomplete gamma inversion](2026-11-12-stats-route-data/gamma-reference.json) gives native relative error 2.31e-16 and stats 9.28e-12.

At all 14 golden fits, `sum(dml(x, fit, log=TRUE))` agrees with `logLik(fit)` within 5.77e-15 relative, comfortably inside 1e-10: see [each family](2026-11-12-stats-route-data/golden-seams.csv). At the previously archived concentrated beta fit, the new sum is 135.278887128587, matching the independent point evaluation; stats gives 135.278912314049, differing by 1.86e-7 relative. The [archived regression](2026-11-12-stats-route-data/archived-beta-seam.csv) also exercises a saved stats density attribute.

## Argument conventions

Numeric/logical vectors recycle without a recycling warning, empty numeric arguments yield empty output, and names/dimensions follow the first longest argument. Non-numeric storage and factors fail with the R Math non-numeric argument error. `log`, `lower.tail` and `log.p` preserve their existing scalar-flag convention. Ordinary missing-value and support-edge behavior is tested.

Deliberate native-domain differences are:

- Discrete densities require exactly integral finite observations. Values such as `1 +/- 1e-12` yield zero and a warning rather than stats' near-integer rounding. Discrete CDFs floor the supplied observation literally rather than applying stats' integer fuzz. Binomial size and hypergeometric count parameters likewise require exact integers.
- Fractional discrete observations produce one generic `non-integer x` warning per vector, rather than stats' per-value messages. Warning order and warnings outside support can differ.
- Parameter validity follows the dossier domains. Zero-scale, infinite-parameter and other degenerate continuous limits may produce NaN with a warning where stats returns a limiting point mass or another extension. Proper finite discrete endpoints remain supported. The convention files record each probe rather than claiming interchangeability on invalid domains.

## Open follow-ups

**Native hypergeometric quantile tie.** `core_qhyper(0.5, m=5, N=10, k=5)` returns 3; the exact quantile is 2. The masses are `[1,25,100,100,25,1]/252`, so `F(2)=1/2` exactly. The immediately smaller probability also incorrectly returns 3. Native rounded tail comparisons need a tie/comparison correction in protected `hyper/dpqr.hpp`; changing that header is outside this job's authorization. Existing truth tests accept adjacent answers at numerically ambiguous jumps, so their pass does not settle this case. The exact combinatorial calculation establishes the defect independently of stats.

**Existing modifier vectorization.** For a geometric law with probability .3 or negative binomial with size 2 and probability .3, both truncated to `[2,7]`, vector `pml(3:5, fit)` repeats its first answer. Scalar evaluation produces the distinct expected values. Replaying baseline stats dispatch and current native dispatch gives the same bug: [reproducer](2026-11-12-stats-route-data/modifier-vectorization.R), [results](2026-11-12-stats-route-data/modifier-vectorization.csv). In the modifier mass helper, a scalar upper endpoint makes `use_lower` scalar, and `ifelse` truncates a vector lower endpoint. This needs a vectorization fix in the separate modifiers-2 job; no modifier algorithm was changed here.

## Completed validation

All final completed runs used the workstation. Commands ran in the foreground through the prescribed CPU reservations, with single-threaded evidence scripts using one slot.

| Check | Result |
|---|---|
| C++ dev via `just test-changed` (all 100 families selected) | 108/108 shards passed, GCC 15.2 |
| `just cpp-test-no-long-double` | 135/135 passed, GCC 15.2 |
| `just test` | 31,002 passes, 155 expected skips, zero failures/warnings |
| `just test-full` | 60,882 passes, zero skips/failures/warnings |
| `just js-build` / `just js-test` | Build passed; 624/624 tests passed |
| `just site-check` | Passed |
| Final comparison and independent reference scripts | Passed |

The initial R dev run exposed the new consumer's metadata-less scaffold error; it was corrected before the final complete dev and full runs. No tests were deleted to accommodate numerical failures.

Simbox attempts exited 75 after availability/connection failures. Its full R run had reached 17,040 passes without failure; its GCC 13.3 no-long-double run had completed 101 of 135 tests before disconnecting. The required workstation no-long-double run passed in full. A complete GCC 13 run remains unavailable; interrupted runs are not counted as passing suites. Local fallback completed all checks above. [Machine-readable validation](2026-11-12-stats-route-data/validation.json) and [session information](2026-11-12-stats-route-data/session-info.txt) retain the counts and environment. Raw build logs remain under ignored `build/scratch/`.

`git diff --check` passed. All 15 report links resolve. The repository-wide Markdown link checker reported 45 existing broken destinations or parser false positives; each destination was confirmed in baseline source, and none belongs to this report. They were left outside this job's scope.

The requested artifact-pane display was attempted. The viewer returned HTTP 403 because this worktree is outside its configured readable roots. The report remains available at its requested worktree path; no viewer configuration or files outside the worktree were changed.
