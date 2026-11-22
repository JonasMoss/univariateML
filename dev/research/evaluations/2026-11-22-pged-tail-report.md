# TASK-221 · GED natural-tail accuracy at large shape

The GED CDF now switches to its three-word radius when rounding the double
radius could move the log tail by more than $10^{-11}$. The maximum measured
natural CDF error fell from $1.19\times10^{-5}$ to $8.20\times10^{-13}$ across the
affine stress grid. All measured CDF, density and quantile comparisons passed
in both precision modes, including all 496 positive subnormal CDF probes.
GED and SGED fit snapshots matched all 458 stored cases per mode.

## Numerical comparison

The independent reference uses mpmath 1.3.0 at 80 digits and the exact
binary64 inputs. It integrates the gamma tail, evaluates the GED log density
directly, and inverts the tail in a checked log-radius bracket of width at most
$10^{-24}$. The grid contains 25 log-spaced shapes from 1 to $10^6$, plus
999999 just below the existing precise-path cutoff. It probes both physical
sides and both requested CDF tails at gamma powers from $10^{-40}$ to $10^6$.
The four $(\mathrm{mean},\mathrm{sd})$ pairs are $(0,1)$, $(0.3,1.7)$,
$(-10^{100},10^{100})$, and $(10^{-100},10^{-100})$. Quantile inputs include
natural probabilities through $10^{-300}$ and log probabilities through
$-10^4$. [Sweep and reference](2026-11-22-pged-tail-data/sweep.py)

The maxima and failure counts below are identical in the native and
double-only builds. The natural-scale maxima use relative error for normal
positive results. Failures also include incorrect rounding to zero and
subnormal failures. A positive subnormal reference requires a finite,
positive result within $10^{-10}$ times the reference plus one subnormal
spacing, $2^{-1074}$. Log errors are divided by
$\max(1,|\mathrm{reference}|)$. Density on the natural scale is an additional
diagnostic with the same $10^{-10}$ tolerance as natural CDF values.
Quantile errors are relative in the returned coordinate, with tolerance
$10^{-9}$. The CDF at `qged` is evaluated against the reference at the
returned binary64 coordinate. [Numerical results and worst inputs](2026-11-22-pged-tail-data/accuracy.json)

| Evaluation | Scale | Checks per build | Baseline max | Baseline failures (NaNs) | Gated max | Gated failures (NaNs) |
|---|---|---:|---:|---:|---:|---:|
| CDF | Natural | 7,072 | 1.19e−5 | 814 (8) | 8.20e−13 | 0 (0) |
| CDF | Log | 7,072 | 2.40e−8 | 394 (0) | 3.85e−14 | 0 (0) |
| Density | Natural | 3,536 | 1.14e−7 finite | 443 (1) | 7.68e−13 | 0 (0) |
| Density | Log | 3,536 | 1.80e−10 | 17 (0) | 2.30e−13 | 0 (0) |
| Quantile | Natural input | 2,288 | 2.49e−13 | 0 (0) | 2.49e−13 | 0 (0) |
| Quantile | Log input | 2,496 | 9.05e−14 | 0 (0) | 9.05e−14 | 0 (0) |
| CDF at quantile | Natural | 2,288 | 7.51e−6 | 350 (0) | 6.88e−13 | 0 (0) |
| CDF at quantile | Log | 2,496 | 1.11e−8 | 215 (0) | 2.11e−14 | 0 (0) |

The unit-scale slice shows how the error changes with shape.

| Shape | Baseline | Gated |
|---:|---:|---:|
| 1,000 | 2.86e−11 | 3.10e−13 |
| 10,000 | 6.46e−10 | 3.13e−13 |
| 100,000 | 8.21e−9 | 2.15e−13 |
| 999,999 | 3.37e−8 | 3.38e−13 |
| 1,000,000 | 6.03e−14 | 6.03e−14 |

All 496 positive subnormal CDF probes returned positive finite values in each
build. This includes the eight NaNs from `b6a00871` and all 240 NaNs from the
initial gated implementation. The baseline failed 191 of the 496 subnormal CDF
checks, including its eight NaNs. The revised gate also passed all 208 positive
subnormal density probes per build. The log CDF passed at every corresponding
abscissa.

## Gate and error bound

Write $a=1/\nu$, $w=\log|q-\mu|-\log\sigma-\log\lambda_\nu$, and
$t=\exp(\nu w)$. The smaller GED tail is $\Gamma(a,t)/(2\Gamma(a))$.
For $a\le1$, the shifted integral and $\log(1+s/t)\le s/t$ give

$$
\Gamma(a,t)=t^{a-1}e^{-t}\int_0^\infty e^{-s}(1+s/t)^{a-1}\,ds
\ \ge\ \frac{t^a e^{-t}}{t+1-a}.
$$

Consequently the magnitude of the derivative of the log smaller tail
with respect to $\log t$ is $t^ae^{-t}/\Gamma(a,t)\le t+1-a$.
The derivative for the log complement is smaller by the odds of the
smaller tail, which are at most one. If $h$ bounds the error in the
computed $\nu w$, integrating over that interval bounds the error induced
in either log tail by $h(1+\exp(\nu\widehat w+h))$. This bound holds in
exact arithmetic given the radius error.

The implementation sets $h=\nu E_w$ with
$E_w=8\epsilon(1+|\log|q-\mu||+|\log\sigma|+|\log\lambda_\nu|+|\widehat w|)$.
The magnitude terms cover the logarithm evaluations, the two subtractions,
and rounding in $\nu\widehat w$ expressed in radius units. The unit term
covers relative error in the physical subtraction and the scale recurrence
$2\log\lambda_\nu=\log 3+\log\Gamma(1+a)-\log\Gamma(1+3a)$.
Its log-Gamma arguments are in $[1,4]$, where the absolute digamma is below
1.26. This is a practical binary64 margin conditional on near-ulp library
evaluations. The gate allocates one tenth of the $10^{-10}$ natural-tail
tolerance to the fast radius. The existing three-word path then compares the
selected tail at both radius endpoints with its evaluator allowance. The
unconditional precise path at $\nu\ge10^6$ is retained.
[dpqr.hpp:95](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L95)
[dpqr.hpp:131](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L131)

The precise GED CDF retains the relative enclosure for normal natural
probabilities. If the enclosure fails and `exp` of the selected log tail
falls below the normal range, the log enclosure decides whether the
conversion is accepted. The resulting `exp` conversion rounds on the
subnormal grid and returns zero when the probability represented by the
checked log tail lies below half a spacing.
[dpqr.hpp:56](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L56)

The density has its own instance of the same amplification in the power
contribution $-t$. For $h<\log 2$, its change is bounded by $2th$. The density
uses this bound on its usual standardized radius, or on the separate
physical logarithms when the quotient is out of range. When it exceeds
the radius budget, GED uses the precise density evaluator with a $10^{-10}$
natural-scale allowance. This accommodates the arithmetic margin for a
large log normalizer when the density is still representable. The shared
evaluator's $10^{-11}$ default remains in use for its non-unit-skew SGED
caller. The quantile inversion stays in the gamma log-radius coordinate.
[dpqr.hpp:32](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L32)
[dpqr.hpp:103](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L103)

## Two-word comparison

The alternative retains the physical subtraction residual and a cached
two-word scale, compensates the quotient with FMA, then applies `log1p` near
the edge. It uses the same fast-path gate and tail enclosure. Cases outside
its normal-input or quotient range use the three-word fallback. Its worst
measured natural and log CDF errors were $9.23\times10^{-13}$ and
$3.85\times10^{-14}$, with zero misses in either build. I kept the gate with
the existing three-word radius because it reuses the physical-coordinate
enclosure across the whole routed domain. The two-word alternative gives a
substantial speedup on the large-shape workload and would need its own
rounding and fallback validation for production. [Comparison probe](2026-11-22-pged-tail-data/probe.cpp)

Median CPU microseconds per CDF call over three runs, excluding parsing.
The ordinary workload has shapes 1 through 10 and powers through 20.
The grouped workload fixes each shape for 768 calls with powers through 700.
The changing workload cycles the 26 shapes, using power 400.
[Timing data](2026-11-22-pged-tail-data/timing.json)

| Variant | Ordinary native / double | Grouped native / double | Changing native / double |
|---|---:|---:|---:|
| `b6a00871` | 0.463 / 0.463 | 6.06 / 6.03 | 106 / 109 |
| Gated three-word | 0.446 / 0.446 | 101 / 101 | 10,750 / 10,925 |
| Gated two-word | 0.451 / 0.449 | 21.9 / 21.7 | 11,822 / 11,651 |

## Fixtures and final checks

`just dpqr-truth ged` appended the shape points $10^3$, $10^4$, and $10^6$,
with three normalization rows, 57 density/CDF rows, and 132 quantile rows.
Every previous numerical row and sampler index matches the baseline.
The input fingerprint changed with `truth.py`. Three appended doctest cases
cover reflected deep natural tails, the log complement, density and quantile
checks at five shapes, large and small affine scales, and subnormal rounding.
The subnormal regression checks both reflected tails at three inputs against
`exp` of the log CDF and checks a separate input below the zero-rounding
threshold. The dossier records the gate and rounding policy.
[Truth route](../../../univariateML-cpp/include/cppunivariate/distributions/ged/truth.py#L8)
[GED regressions](../../../univariateML-cpp/include/cppunivariate/distributions/ged/test.cpp#L780)
[Subnormal regression](../../../univariateML-cpp/include/cppunivariate/distributions/ged/test.cpp#L824)

All final runs used the workstation, with GCC 15.2.0 for native C++. The
double-only build defines `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`.

| Check | Result |
|---|---|
| `just truth-check` | 36 passed. The recipe checks table metadata globally and spot-regenerates Exp and Uniform. The family C++ tests exercise the generated GED and stored SGED reference rows directly. |
| `just test-changed --no-long-double` | Rerun after the subnormal revision. GED and SGED, 5 CTest shards passed per build. R reported 1,274 passes, 12 skips, 0 warnings and 0 failures. The JS build succeeded and all 234 JS tests passed. |
| Supplemental CTest | LST and Norm, 2 passed per build. Their test translation units include GED transitively through SGED and shared inference, respectively. Their dev shards also exercise their truth tables. |
| Symmetric SGED replay | 8,018 prior oracle comparisons through the `xi=1` GED delegate, zero failures per build. |
| GED and SGED fit snapshots | 173 GED and 285 SGED, zero differences or missing cases per build against the stored snapshots. |
| `just site-check` | Passed on the final dossier, including 85 family pages and 85 compiled C++ snippets. |

The CDF consumer is SGED. Its unit-skew path delegates directly and some
non-unit paths also use the GED CDF. The snapshot comparison covers the
fitting outcomes and likelihoods, while the truth fixtures cover the
distribution functions. [Symmetric replay](2026-11-22-pged-tail-data/symmetric-sged.json)
[Snapshot comparison](2026-11-22-pged-tail-data/snapshot-diff.txt)

From the worktree root, the study can be repeated with the following
single-slot commands. The builder reconstructs the baseline headers with
`git archive b6a00871` inside the ignored scratch directory.

```sh
data=dev/research/evaluations/2026-11-22-pged-tail-data
export PYTHONPATH="$PWD/dev/validation"
/tmp/umvl-cpu/run sh "$data/build.sh"
/tmp/umvl-cpu/run python3 "$data/sweep.py"
/tmp/umvl-cpu/run python3 "$data/benchmark.py"
/tmp/umvl-cpu/run python3 "$data/replay_symmetric.py"
/tmp/umvl-cpu/run sh "$data/snapshots.sh"
```

## Open

- The guarded path is expensive for large-shape CDF workloads. A production
  two-word radius needs an error analysis for exponent rescaling and
  cancellation in the input affine transformation, followed by independent
  checks of its fallback boundaries. It would use the same subnormal output
  rounding policy. The changing-shape benchmark includes the cost of
  recomputing the scale when the one-entry cache misses.
- At large shape and extreme affine scales, the natural probability at
  `qged(p)` can differ from the input by up to $2.02\times10^{-5}$ relative
  in this sweep, even though the CDF at that coordinate agrees with the
  oracle to $6.88\times10^{-13}$. Rounding the oracle quantile to the nearest
  binary64 coordinate alone gives up to $8.95\times10^{-8}$ relative
  discrepancy. The larger residual involves the affine reconstruction in
  `qged`. A separate quantile study would need to bound that reconstruction
  against the adjacent exact-reference coordinates.
