# TASK-116.48: sstd certification report

Implemented a global xi-only certificate with mean, sd and nu held. It uses the actual moment constraints and returns a certified finite maximizer or a structured limit, tie, budget or numerical stop. The binding-neutral route serves both R and JS. Larger subsets remain open as specified below; the family's `fit.certified` remains `partial`, and its `fit.note` now identifies the new route and the remaining uncertified searches.

The default fit and frozen golden file are unchanged. On the frozen 500-observation sample, all 15 unchanged held-subset routes retain identical statuses, certification flags, coefficients and likelihoods in all three paired repetitions. No golden, reference value or tolerance was rewritten. The base is `09ab21746631b00f56aed243e16f52066f461be2`; no commit was made.

**Implemented certificate.** The new [skew certificate](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/skew_certificate.hpp) is selected by [sstd_mle](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/mle.hpp) when mean, sd and nu are held and xi is free. [The common binding](../../../bindings/common/distributions/sstd/fit.hpp) transports its structured stops, including reflected half-t witnesses in the model's `(mu, nu, sigma)` order.

Let `m = E|Z_nu|` for the variance-one t, `d = (x-mean)/sd`, and

```
q = xi^2/(1+xi^2)
H = 1-m^2 + (4*m^2-3)*q*(1-q)
N = d*sqrt(H) + m*(2*q-1).
```

The density is `2*sqrt(H)/sd` times the variance-one t density at `N/q` for positive N, or `N/(1-q)` otherwise. Thus the entire unbounded skew domain becomes `(0,1)` while retaining both held moments. H is positive throughout. Its endpoint values are `1-m^2`, and its sole possible interior extremum is `H(1/2)=1/4`.

Interval bounds enclose likelihood and analytic score on every cell, including join crossings. A cell is discarded only by a likelihood upper bound or a definite finite score sign. Both supported skew endpoints are explicit constrained half-t candidates; unsupported tails are excluded by likelihood bounds. All surviving stationary components are compared, including possible minima. No grid window, unimodality assumption or retained multistart supplies coverage.

Leaf widths are at most `2e-13*min(q,1-q)`; merging requires the full component span to be at most `1e-10*min(q,1-q)`, giving a log-skew span of approximately `1e-10`. The representative is the full component's midpoint. Integrated score remainders must also fit within the point-likelihood allowance. Public-coordinate likelihood recovery is checked, and nonfinite arithmetic or normalization collisions stop numerically. Logarithmic kernel bounds avoid overflowing squared extreme residuals; infinite score bounds never authorize exclusion.

Work is metered by `128*min(iterlim,100000)` cell evaluations. Budget stops retain a finite witness when representable and an infinite global upper bound. These are exact-arithmetic certificates conditional on computed enclosures and comparisons, with a practical padding of 128 binary64 epsilons times one plus the absolute term magnitudes, including normalizer cancellation. They are not outward-rounded interval certificates. The [derivation](2026-11-08-sstd-free-data/derivation.md) states the equations and proof obligations in detail.

**Endpoint contacts and large held shape.** Exact contact with a constrained half-t endpoint requires the shrinking-side limit, not the ordinary half-t endpoint height. With `r=sqrt(1-m^2)`, that residual tends to `m/(2*r^2)` in absolute value. This factor is derived, but near-contact classification and the corresponding boundary-path witness are not implemented. A contact within the practical floating-point margin returns a specific numerical stop.

For held nu at least 128, the new route returns a specific numerical stop for unresolved skew-moment gamma-ratio arithmetic. This is a conservative arithmetic guard, not a parameter restriction or a shape search cap. Increasing iterlim does not remove it. A symmetric parent witness is retained when representable.

The [80-digit moment check](2026-11-08-sstd-free-data/moment-checks.json), using [this probe](2026-11-08-sstd-free-data/moment-probe.cpp) and [independent checker](2026-11-08-sstd-free-data/check-moment.py), found the following workstation errors in the settled `sstd_sigma` moment calculation:

| Held nu | Absolute moment error |
|---:|---:|
| 127 | 7.55e-17 |
| 128 | 1.92e-14 |
| 10,000 | 1.22e-12 |
| 100,000,000 | 1.22e-8 |
| 1,000,000,000,000 | 1.49e-4 |

At nu `1e16`, the computed moment is about `5.64e7` instead of approximately `0.79788456`. The t normalizer already switches to stable asymptotic arithmetic at 128; the skew moment still subtracts large log-gamma values. The new certificate does not certify a skew estimate using that unresolved moment.

The needed repair is a stable half-step gamma ratio or bounded asymptotic moment in `special/skewed_densities.hpp`, through the permitted special-function seam, followed by large-nu checks of all fitter and d/p/q/r consumers. Existing fixed-skew scalar routes also consume this moment. No special-function, d/p/q/r, truth or code-generator source was changed, as instructed; the broader seam repair is an open item.

**Uniqueness and the additional inner-profile result.** For observations `(-1,1)`, held mean zero, sd one and nu five, the certificate leaves the two reflected half-t faces as tied global winners. Reflection proves exact equality; their dominance over finite stationary components is certified conditional on the computed comparisons. The ordinary endpoints are approximately `-1.0843010164` and `1.0843010164`, with ordinary scale `1.1425520629` and nu five. The tie is retained in C++, R and JS rather than choosing a side.

Existing finite nonuniqueness remains covered: observations `(-2,2)` with held sd one, nu three and xi one have two mean maxima at `+/-sqrt(3)`. The [six-point asymmetric scale example](2026-11-06-sstd-cert-data/skew-scale.json) has three scale-score roots, so a single conditional scale root is insufficient.

A new exact inner-profile lemma reduces the fixed-nu, both-moments-free problem. At an interior mode c, let `T_L(a)` and `T_R(b)` sum squared residuals divided by `nu*scale^2 + squared residual`, and put `C=n/(nu+1)`. Stationarity gives `T_L(a)=C*r`, `T_R(b)=C*(1-r)`, with `r=a/(a+b)`. Each side inverse is unique. The equation `a_L(r)/r - b_R(r)/(1-r)=0` is strictly decreasing and crosses zero exactly once when both sides are nonempty and `nu*(n-k)>k`. Boundary coercivity then proves that this stationary pair is the unique global side-scale maximizer. The optimizing r increases with mode.

This lemma is proved in the [derivation](2026-11-08-sstd-free-data/derivation.md). [Nine 50-digit numerical solutions](2026-11-08-sstd-free-data/side-scales.json) check both side scores, the mode ordering and 225 scale perturbations, with zero violations. It is not wired as a global fitter: exhaustive mode comparison and endpoint bounds are still missing. Holding either moment invalidates the independent side-scale equations and needs a separate constrained analysis.

**Independent coefficient evidence.** The [70-digit checker](2026-11-08-sstd-free-data/check.py) found zero violations at 900 interior probes of likelihood and score bounds across 100 cells, at held shapes 2.1, 3, 5 and 20. Eight small-sample finite roots and one frozen-sample root were independently solved. These numerical checks supplement the enclosure proof; they do not replace it.

The largest absolute xi error across the eight small-sample roots is 4.25e-14. For the frozen sample at held mean zero, sd 1.4 and nu five, the independent root is xi `1.3282051723019642`, with log likelihood `-654.1024214570797`; the certificate's xi error is 1.1e-14, and likelihood error 7.47e-13. [Full results](2026-11-08-sstd-free-data/math-checks.json).

**Timings and subset coverage.** Workstation GCC 15.2, C++17/O2, one thread under the slot reservation, three paired repetitions, medians in milliseconds. The data are the frozen 500 observations. Held values are mean zero, sd 1.4, nu five and xi 1.4; controls are unchanged: gradtol `1.220703e-4`, iterlim 200. Both stages used matching priority settings under shared load. Differences on unchanged algorithms are not claimed as speedups. The new certificate adds proof work; its cost is visible below.

| Held coefficients | Before ms | After ms | Certificate on this run |
|---|---:|---:|---|
| none | 62.412 | 59.894 | no |
| mean | 910.232 | 897.215 | no |
| sd | 123.314 | 123.141 | no |
| mean, sd | 47.337 | 47.337 | no |
| nu | 38.902 | 38.912 | no |
| mean, nu | 1437.446 | 1422.025 | no |
| sd, nu | 36.295 | 37.875 | no |
| mean, sd, nu | 3.580 | 206.098 | yes, new |
| xi | 31.419 | 32.772 | no |
| mean, xi | 13.231 | 17.736 | no |
| sd, xi | 9.022 | 19.358 | no |
| mean, sd, xi | 2.609 | 4.939 | no |
| nu, xi | 4665.897 | 3888.378 | no |
| mean, nu, xi | 6.566 | 3.412 | yes |
| sd, nu, xi | 4.008 | 2.055 | yes |
| mean, sd, nu, xi | 0.016 | 0.009 | yes, evaluation |

[Raw timing records and coverage](2026-11-08-sstd-free-data/timings.json), [benchmark source](2026-11-08-sstd-free-data/timing.cpp) and [runner](2026-11-08-sstd-free-data/benchmark.sh) are retained. The exactly held xi-one route with both moments and nu free continues to delegate to the certified std parent; that special reduction is not represented by the xi-1.4 rows above.

**Open entries.** These identify missing certificates, not numerical evidence promoted to proof.

| Subset or case | What remains missing |
|---|---|
| Free nu, asymmetric held xi, with either/both/no moments held | Exhaustive constrained mode/scale stationary enclosures; uniform nu-down-to-two and large-nu profile bounds; constrained face comparisons. Held mean can produce multiple scale roots. |
| Fixed nu, free xi, both moments free | Implement and meter the proved inner scale inversions, propagate coefficient enclosures, exhaustively compare the mode intervals and knots, and close endpoint neighborhoods with quantitative one-sided bounds. The vanishing side scale has a fractional-power endpoint behavior. |
| Fixed nu, free xi, exactly one moment held | Derive and enclose the genuinely constrained profiles; the free-moment side-scale lemma does not apply. Compare all reachable held-moment half-t faces. |
| Fixed nu and xi, both moments free | Replace the existing finite mode scan by exhaustive global mode enclosures and justified scale-tail handling. |
| Nu and xi free, including all-free | Supply the inner certificates above and uniform bounds at joint shape/skew/scale corners. Pointwise t, skew-normal or half-t limits and sampled face profiles do not supply those bounds. |
| Fixed-moment endpoint contact | Integrate the known shrinking-side factor, distinguish rounded near-contact sides safely, and retain an appropriate boundary-path witness. Current outcome is numerical failure. |
| Large held nu | Repair the settled skew-moment seam and verify all consumers before lifting the new arithmetic guard. Current new-route outcome is numerical failure. |

The larger numerical searches retain their previous behavior and remain uncertified. Their finite results and selected limit faces are not global-maximizer or nonexistence proofs. No all-free certificate is claimed by this job. Other held-moment symmetric reductions are not added here.

**Final validation.** All required commands finished green on the final fitter, binding and test sources. [Validation JSON](2026-11-08-sstd-free-data/validation.json), [summary](2026-11-08-sstd-free-data/validation.txt), [production hashes](2026-11-08-sstd-free-data/source.sha256) and [golden checksum](2026-11-08-sstd-free-data/golden.sha256) are retained. No simbox fallback was needed.

| Check | Machine | Result |
|---|---|---|
| [C++ dev](2026-11-08-sstd-free-data/frozen-cpp-dev.txt) | simbox GCC 13.3 | 106/106 passed |
| [C++ full](2026-11-08-sstd-free-data/frozen-cpp-full.txt) | simbox GCC 13.3 | 129/129 passed |
| [No-long-double full](2026-11-08-sstd-free-data/frozen-cpp-no-long-double.txt) | simbox GCC 13.3 | 129/129 passed |
| [No-long-double full](2026-11-08-sstd-free-data/frozen-cpp-no-long-double-local.txt) | workstation GCC 15.2 | 129/129 passed |
| [R dev](2026-11-08-sstd-free-data/frozen-r-dev.txt) | simbox | FAIL 0, WARN 0, SKIP 154, PASS 22086 |
| [R full](2026-11-08-sstd-free-data/frozen-r-full.txt) | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44918 |
| [JS build](2026-11-08-sstd-free-data/js-build.txt) | workstation | Passed |
| [JS full](2026-11-08-sstd-free-data/js-test.txt) | workstation | 548/548, no failures or skips |
| [Site](2026-11-08-sstd-free-data/site-check.txt) | workstation | Passed: 99 family pages, MathML/browser checks, 99 C++17 snippets |
| [Targeted sstd C++](2026-11-08-sstd-free-data/targeted.txt) | workstation | 35 cases, 11,549 assertions passed |

The workstation no-long-double evidence is CTest's preserved per-test log: 129 `Test Passed` records and no failed records. A log-fetch pattern had overwritten its redirected summary; the persisted CTest log was recovered and the fetch helper corrected to exclude workstation logs. No test result was inferred from a golden match or a simbox-only pass.

Five C++ cases and two R cases were appended to the family tests, and one combined JS case was appended to the shared fit tests. Production edits are confined to sstd and its binding. No core or generic binding header was changed. The site check's unrelated generated PNG was restored to its original bytes.

[reproduce.sh](2026-11-08-sstd-free-data/reproduce.sh) rebuilds the baseline through read-only Git access, reruns the targeted checks, paired timings and independent numerical checks. Long runs use [validate-simbox.sh](2026-11-08-sstd-free-data/validate-simbox.sh) through `/tmp/umvl-cpu/simbox-run`, and [validate-local.sh](2026-11-08-sstd-free-data/validate-local.sh) directly; the latter reserves slots per computing command. [fetch-simbox-logs.sh](2026-11-08-sstd-free-data/fetch-simbox-logs.sh) retrieves only simbox logs. [collect-validation.py](2026-11-08-sstd-free-data/collect-validation.py) checks hashes and all final result counts.

**Artifact delivery.** The requested viewer command returned HTTP 403 because this worktree is outside its configured readable roots. The report is retained at the requested path; no viewer configuration or other checkout was changed.
