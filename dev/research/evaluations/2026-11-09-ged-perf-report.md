# Faster certified GED free-shape fits — TASK-164

Completed in `codex/ged-perf`, based on `52820d8c9094a256aefd2d00223e9419350b22ed`. No commits, merges or rebases were made by this job.

All four free-shape subsets are faster on both measured samples: median elapsed speedups range from 1.55 to 2.41. All eight benchmark fits retain identical binary64 coefficients and likelihoods. The 480-call comparison retains every status, certification flag, budget flag, comparison-tie flag and limit-model classification; accepted coefficients have zero drift. Required final suites are green, including no-long-double on both machines.

## Implementation and certificate argument

Changes are confined to GED's `mle.hpp`, `shape_certificate.hpp`, `free_shape_bounds.hpp`, dossier prose and appended C++ tests.

1. A parent shape interval's location enclosure contains every minimizer for every child interval and endpoint shape. The child location solve starts from this enclosure. This uses set containment, without assuming a monotone location path or a unimodal shape profile.
2. The likelihood bound reuses that location enclosure in the existing convex supporting-line moment bound. For a convex residual-power objective F and a bracket containing its minimizer, the supporting line at the bracket midpoint gives a lower bound over the bracket; evaluating F at that midpoint gives an upper bound on its minimum. Reusing a certified bracket therefore eliminates a duplicate solve without removing any possible maximizer. The exact median treatment at shape one remains in place.
3. Per-residual endpoint powers and logarithms are cached and shared between T, U and V (value, shape-score and curvature moments). The formulas and interior extrema are unchanged. Lower powers for H and G remain direct evaluations: underflow of r^p cannot justify setting a representable r^(p-2) to zero. The cache uses eight doubles per observation, about 0.64 MB at n=10000, and is freed after each box evaluation.
4. Conditional and original-coordinate witness likelihoods compute their shape normalizers once per sum. Each observation retains the original density arithmetic and compensated summation order.

For budgets shorter than `numeric_limits<double>::digits` (53 here), location and moment enclosures retain the cold-start route. An initial differential check found that unrestricted reuse rescued some numerical stops at iteration limits 1–2. The compatibility guard preserves these stop semantics; the final 480-call comparison has no differences. No work cap, default, acceptance tolerance or floating-point margin was increased or loosened.

The certificate remains the package-standard exact-arithmetic argument conditional on computed comparisons, with the existing practical floating-point margins. It does not claim outward-rounded interval arithmetic. The independent numerical checks below validate the implementation of the inequalities; they are separate from their mathematical justification. The complete underlying score, curvature and tail derivations remain in the [GED dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ged/README.md#finite-shape-score-certificate).

## Before/after measurements

Workstation GCC 15.2, C++17 `-O2`, tol=1e-10, iterlim=200; median of three uninstrumented fits per case, excluding compilation and sample generation. The 500-point sample is the frozen golden. The 10000-point sample uses distinct deterministic GED(0,1,3) quantiles q((j+1/2)/10000). Golden holds use frozen coefficients; quantile holds use mean zero and sd one. All reported fits are finite and certified.

The baseline is the already certified implementation at the base commit, reconstructed with read-only `git show`. Raw measurements and 17-digit coefficients are retained in the data directory. These are shared-workstation elapsed measurements, rather than controlled causal speedup estimates.

| n | Free coefficients | Before ms | After ms | Speedup | Certificate evaluations before → after |
|---:|---|---:|---:|---:|---:|
| 500 | mean,sd,nu | 455.552 | 262.293 | 1.74 | 342 → 342 |
| 500 | sd,nu | 53.638 | 22.268 | 2.41 | 250 → 250 |
| 500 | mean,nu | 362.475 | 206.729 | 1.75 | 319 → 319 |
| 500 | nu | 39.668 | 18.906 | 2.10 | 209 → 209 |
| 10000 | mean,sd,nu | 4934.476 | 3183.703 | 1.55 | 410 → 410 |
| 10000 | sd,nu | 1151.345 | 524.416 | 2.20 | 277 → 277 |
| 10000 | mean,nu | 3840.650 | 2476.874 | 1.55 | 322 → 318 |
| 10000 | nu | 720.481 | 356.866 | 2.02 | 195 → 195 |

## Where certificate work goes

Instrumented shadow headers supply call counts and inclusive phase timings; instrumentation never enters production headers or timed benchmark binaries. The certificate evaluation column above is the fitter's work ledger. Function calls overlap within a ledger evaluation and must not be summed as independent evaluations.

| n, all free | Shape cells before → after | Maximum subdivision depth | Location-score passes before → after | Moment location iterations before → after | Tail calls before → after |
|---|---:|---:|---:|---:|---:|
| 500 | 219 → 219 | 11 → 11 | 7088 → 4098 | 5679 → 199 | 5 → 5 |
| 10000 | 267 → 267 | 12 → 12 | 3761 → 1885 | 186 → 5 | 5 → 5 |

Tail closure is cheap on these samples (a few calls and milliseconds). Shape subdivision depth stays unchanged in the all-free cases. The main savings come from fewer location-enclosure passes, eliminating repeated moment location solves, and sharing residual powers across score and curvature passes. At n=10000, all-free conditional witnesses still perform 6750 location iterations; this remaining cost was not replaced by an unchecked proposal. Full per-subset phase timings, Hessian-pass counts, tree sizes and depths are in [profiles.tsv](2026-11-09-ged-perf-data/profiles.tsv).

## Validation

- Targeted binary: 42 cases, 5764 assertions, all passed. Appended cases check inherited location enclosures, supporting moment bounds, zero and underflow residuals, and short-budget numerical stops.
- Independent 70-digit raw-likelihood checks: 2746 enclosure comparisons each with fresh and inherited brackets, zero violations; four independent golden roots, zero violations. Largest absolute shape error is about 2.20e-10 (relative error below 1e-10).
- Independent 80-digit checks with inherited moment brackets: 108 rows, 756 enclosure comparisons and 26 analytic inequality checks, zero violations.
- Differential comparison: 480 calls over ten sample constructions, four free-shape masks, held scales 0.1/1/10 and iteration limits 1/2/10/200. Some calls repeat when scale is free. No status or accepted-coefficient changes.
- Frozen golden bytes and tolerances are unchanged. Native dev/full regression suites pass. Tests were appended to the existing C++ file; its original prefix is retained.

| Final command | Machine | Result |
|---|---|---|
| just cpp-test | Workstation GCC 15 | 106/106 |
| UMVL_TEST_TIER=full just cpp-test | Workstation GCC 15 | 129/129 |
| just cpp-test-no-long-double | Workstation GCC 15 | 129/129 |
| just cpp-test-no-long-double | simbox GCC 13 | 129/129 |
| just test | simbox, R compiler g++-15 | FAIL 0, WARN 0, SKIP 154, PASS 22186 |
| just test-full | simbox, R compiler g++-15 | FAIL 0, WARN 0, SKIP 0, PASS 45018 |
| just site-check | Workstation | Green: 99 family pages, MathML, links, C++17 snippets and browser checks |

Native C++ suites ran sequentially through a one-slot foreground launcher; site validation used one CPU. R dev/full and the second-machine no-long-double check ran through simbox-run, foreground and blocking. The original unavailable-machine attempts and the usage-limit interruption are recorded in the evidence; all outstanding suites subsequently finished green after resumption. No estimates or test results were inferred from a still-running command.

No fitter correctness or implementation items remain open. Protected d/p/q/r, special-function and truth sources, R adapters, code generators, NEWS, status and roadmap files were not changed. No dependencies were installed or board entries edited.

## Evidence and reproduction

[validation.json](2026-11-09-ged-perf-data/validation.json) records tested source hashes, command durations, machine assignments, golden protection, timing summaries and comparison results. Completed native, remote and site logs are archived alongside it.

Use the slot wrappers for computations, without nesting them. Run [benchmark.sh](2026-11-09-ged-perf-data/benchmark.sh), then [compare.sh](2026-11-09-ged-perf-data/compare.sh) and [checks.sh](2026-11-09-ged-perf-data/checks.sh); [bounds.sh](2026-11-09-ged-perf-data/bounds.sh) checks inherited moment bounds. [final_cpp.sh](2026-11-09-ged-perf-data/final_cpp.sh), [final_suites.sh](2026-11-09-ged-perf-data/final_suites.sh) and [site.sh](2026-11-09-ged-perf-data/site.sh) reproduce the completed suites with their documented worker limits. Finally [collect.py](2026-11-09-ged-perf-data/collect.py) checks source consistency and collects the completed logs. No pushes or commits are part of this workflow.
