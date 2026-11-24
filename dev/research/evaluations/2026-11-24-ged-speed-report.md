# TASK-225 · Guarded GED CDF performance

The guarded three-word GED CDF is about **5× faster for grouped shapes** and
**48–49× faster for changing shapes** than master `0480604f`. The largest
measured natural CDF error was $8.20\times 10^{-13}$ in both builds, with zero
failures including all 496 positive subnormal CDF probes per build. All four
fit snapshots matched their stored baselines.

## Profile

The native probe used GCC 15.2, `-O2 -fno-inline -pg` and the inputs from the
TASK-221 benchmark. To collect more samples, the changing workload made 32
passes through the 26 shapes. The table gives inclusive percentages from the
instrumented call graph and the recorded call count in parentheses. Inclusive
percentages overlap, and the instrumented build is used for attribution.

| Function | Grouped before | Grouped after | Changing before | Changing after |
|---|---:|---:|---:|---:|
| `physical_radius` | 100.0% (13,056) | 91.9% (13,056) | 100.0% (800) | 100.0% (800) |
| `logscale_uncached` | 11.5% (26) | 0.9% (26) | 98.5% (800) | 81.8% (800) |
| Three-word `log_gamma` | 11.5% (52) | 0.8% (52) | 98.5% (1,600) | 81.0% (1,600) |
| Three-word `logarithm` | 99.8% (29,487) | 91.1% (26,268) | 99.1% (105,568) | 72.7% (6,400) |

Before the change, cache misses dominated changing-shape calls. On grouped
shapes, the physical-distance and scale logarithms dominated, and three-word
division accounted for 76.4% of inclusive sampled time. After the change,
the changing workload uses eight three-word logarithms per guarded call,
down from about 132. Three-word `exponential` had no calls in these CDF
profiles. Its bounded exit benefits the shared arithmetic used in quantiles
and skew coordinates. [Grouped before](2026-11-24-ged-speed-data/profile-before-grouped.txt) ·
[Grouped after](2026-11-24-ged-speed-data/profile-after-grouped.txt) ·
[Changing before](2026-11-24-ged-speed-data/profile-before-changing.txt) ·
[Changing after](2026-11-24-ged-speed-data/profile-after-changing.txt)

## Arithmetic and bounds

The precise GED log scale still uses the difference of two log-Gamma values.
For a positive $x$ shifted by $n$ to the Stirling region, the shared
routine now evaluates the exact identity

$$
\log\Gamma(x)=\log\Gamma(x+n)-\log x
             -\log\left(\prod_{j=1}^{n-1}(x+j)\right).
$$

The first factor can be subnormal and is evaluated separately in logs. The
remaining product contains at most 63 factors in $[1,64]$ and is bounded
by $64^{63}=2^{378}$. Each shift now takes a multiplication in place of
a full logarithm. With relative product error $\delta$, the absolute
error in its logarithm is bounded by $|\delta|/(1-|\delta|)$ for
$|\delta|<1$. The existing Stirling evaluation is applied at the shifted
argument. [Log-Gamma recurrence](../../../univariateML-cpp/include/cppunivariate/special/three_word.hpp#L120)

The logarithm balances its mantissa around one using the cutoff $\sqrt{2}$,
which gives $|z|<0.172$ for $z=(m-1)/(m+1)$. The shared direct
`log1p` reduction has $|z|\le 1/3$ in exact arithmetic. After term $k$
of the doubled atanh series, the exact tail is at most

$$
\frac{2|z|^{2k+1}|z|^2}{(2k+3)(1-|z|^2)}.
$$

Using $|z|\le0.334$ and a practical factor-of-two allowance on the leading
word of the last power, the implemented exit bounds the omitted log series
by $10^{-47}|z_{\rm high}|$. The 64-term cap bounds the absolute tail
by $10^{-62}$. [Log reductions and exit](../../../univariateML-cpp/include/cppunivariate/special/three_word.hpp#L75)

The reduced exponential argument satisfies $|r|<0.347$. After term $k$,
the geometric tail bound is $|t_k|q/(1-q)$ with $q=0.347/(k+1)$.
With the same practical factor-of-two allowance on the leading word,
the early exit omits less than $10^{-47}$ before power-of-two rescaling.
The 48-term cap bounds the omitted reduced series by $10^{-85}$.
[Exponential exit](../../../univariateML-cpp/include/cppunivariate/special/three_word.hpp#L104)

The truncation bounds are exact-arithmetic statements conditional on the
stated term bounds. Floating-point expansion truncations and recurrence
rounding are covered by the existing practical allowance
$32768\epsilon^3(1+\text{magnitude})+128\cdot 2^{-1074}$, conditional
on normal intermediates. GED still applies that allowance in the three-word
radius and tests both endpoints with the original gamma evaluator allowance
and subnormal rounding rule. [Arithmetic margin](../../../univariateML-cpp/include/cppunivariate/special/three_word.hpp#L138) ·
[GED radius and enclosure](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L56) ·
[TASK-221 gate derivation](2026-11-22-pged-tail-report.md#gate-and-error-bound)

## Numerical comparison

The TASK-221 sweep uses exact binary64 inputs, 80-digit mpmath references,
26 shapes from 1 through $10^6$, four affine settings, both physical sides
and both requested CDF tails. Natural-scale maxima below use relative error
for normal positive results. Positive subnormal results require a positive
finite output within the $10^{-10}$ relative allowance plus one subnormal
spacing. Log errors are scaled by $\max(1,|\mathrm{reference}|)$, with
tolerance $10^{-10}$. Quantile tolerance is $10^{-9}$ relative in the
returned coordinate. CDF at quantile compares to the oracle at that binary64
coordinate. Both native and double-only builds gave the displayed maxima
and counts. [Sweep](2026-11-22-pged-tail-data/sweep.py) ·
[Full results and worst inputs](2026-11-24-ged-speed-data/accuracy.json)

| Evaluation | Scale | Checks per build | `b6a00871` max | `b6a00871` failures (NaNs) | Master max | Optimized max | Master / optimized failures (NaNs) |
|---|---|---:|---:|---:|---:|---:|---:|
| CDF | Natural | 7,072 | 1.19e−5 | 814 (8) | 8.20e−13 | 8.20e−13 | 0 (0) / 0 (0) |
| CDF | Log | 7,072 | 2.40e−8 | 394 (0) | 3.85e−14 | 3.85e−14 | 0 (0) / 0 (0) |
| Density | Natural | 3,536 | 1.14e−7 finite | 443 (1) | 7.68e−13 | 7.68e−13 | 0 (0) / 0 (0) |
| Density | Log | 3,536 | 1.80e−10 | 17 (0) | 2.30e−13 | 2.30e−13 | 0 (0) / 0 (0) |
| Quantile | Natural input | 2,288 | 2.49e−13 | 0 (0) | 1.06e−16 | 1.06e−16 | 0 (0) / 0 (0) |
| Quantile | Log input | 2,496 | 9.05e−14 | 0 (0) | 1.02e−16 | 1.02e−16 | 0 (0) / 0 (0) |
| CDF at quantile | Natural | 2,288 | 7.51e−6 | 350 (0) | 6.82e−13 | 6.82e−13 | 0 (0) / 0 (0) |
| CDF at quantile | Log | 2,496 | 1.11e−8 | 215 (0) | 2.38e−14 | 2.38e−14 | 0 (0) / 0 (0) |

Unit-scale slice of the maximum natural CDF relative error.

| Shape | `b6a00871` | Master | Optimized |
|---:|---:|---:|---:|
| 1,000 | 2.86e−11 | 3.10e−13 | 3.10e−13 |
| 10,000 | 6.46e−10 | 3.13e−13 | 3.13e−13 |
| 100,000 | 8.21e−9 | 2.15e−13 | 2.15e−13 |
| 999,999 | 3.37e−8 | 3.38e−13 | 3.38e−13 |
| 1,000,000 | 6.03e−14 | 6.03e−14 | 6.03e−14 |

All 496 positive subnormal CDF probes and all 208 positive subnormal density
probes passed in each optimized build. The older baseline failed 191 of the
496 subnormal CDF comparisons, including eight NaNs. The corresponding
log CDF evaluations all passed in the optimized builds.
[Subnormal results](2026-11-24-ged-speed-data/accuracy.json)

The symmetric replay called `psged` with `xi=1` for 8,018 frozen comparisons
per build. The worst error was $7.71\times10^{-13}$, with zero failures in
each build. In a supplemental asymmetric SGED and skew-Student replay,
all 40,659 outputs per build matched master, including one SGED log
probability whose reference exceeds the magnitude of the largest finite
binary64 value. Both versions return `-Inf` there. The raw comparator counts
that case as a mismatch to the finite arbitrary-precision reference.
[Symmetric replay](2026-11-24-ged-speed-data/symmetric-sged.json) ·
[Asymmetric replay and overflow input](2026-11-24-ged-speed-data/skew-replay.json)

## Timing

Median CPU microseconds per CDF call over three runs, excluding parsing,
pinned to logical CPU 2 on the workstation. The ordinary workload has
10,240 calls at shapes 1 through 10 and powers through 20. The grouped
workload fixes each of 26 shapes for 768 calls with powers through 700.
The changing workload cycles the same shapes four times, using power 400.
[Raw timings](2026-11-24-ged-speed-data/timing.json)

| Revision | Ordinary native / double | Grouped native / double | Changing native / double |
|---|---:|---:|---:|
| `b6a00871` | 0.211 / 0.210 | 2.714 / 2.722 | 42.47 / 42.39 |
| Master `0480604f` | 0.200 / 0.209 | 45.571 / 46.196 | 3,939.65 / 4,055.58 |
| Optimized | 0.212 / 0.216 | 9.056 / 9.323 | 81.07 / 83.66 |

## Final checks

Simbox returned exit 75 before the suite run. The workstation ran all listed
checks. The shared header selected all 85 fitted families in
`just test-changed --no-long-double`. A scratch `just` wrapper dispatched its
R step to `just test-full`, which runs `just truth-check` and the full R tier
in one pass. The C++ and JS steps used the runner's usual recipes. The GED
and SGED truth fixtures and goldens matched their starting Git blobs
byte-for-byte.

| Check | Machine | Result |
|---|---|---|
| `just cpp-test` | Workstation, GCC 15.2 | 135/135 passed |
| `just cpp-test-no-long-double` | Workstation, GCC 15.2 | 180/180 passed |
| `just truth-check` | Workstation | 36/36 passed |
| `just test-full` | Workstation, R 4.6.1 | 67,041 passes, one skip, zero warnings or failures |
| `just js-build` and `just js-test` | Workstation | Build passed, 593/593 tests passed |
| `just site-check` | Workstation | Passed, including 85 family pages and 85 compiled C++ snippets |
| Fit snapshots | Workstation, GCC 15.2 | 12,053 each in native and double-only, 2,006 each in the two modifier-route modes. Zero differences, additions or omissions. |

[Four snapshot comparisons](2026-11-24-ged-speed-data/snapshot-diff.txt)

Reproduce the measurements from the repository root. The builder reconstructs
both historical header sets with `git archive` in ignored scratch space.
The double-only binaries define `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`.

```sh
data=dev/research/evaluations/2026-11-24-ged-speed-data
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export PYTHONPATH="$PWD/dev/research/evaluations/2026-11-22-pged-tail-data:$PWD/dev/validation"
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run sh "$data/build.sh"
/tmp/umvl-cpu/run python3 "$data/profile.py" before build/scratch/ged-speed/master/univariateML-cpp/include
/tmp/umvl-cpu/run python3 "$data/profile.py" after univariateML-cpp/include
/tmp/umvl-cpu/run python3 "$data/study.py" sweep replay_symmetric
/tmp/umvl-cpu/run taskset -c 2 python3 "$data/study.py" benchmark
/tmp/umvl-cpu/run python3 "$data/skew_replay.py"
/tmp/umvl-cpu/run4 sh "$data/snapshots.sh"
```

## Open

- The grouped workload still costs 3.3–3.4 times the historical
  `b6a00871` workload. The after-profile attributes most of the remaining
  grouped time to the per-call three-word radius logarithms. Closing that
  gap would require a cheaper radius evaluation with bounds for exponent
  rescaling, affine subtraction residuals and cancellation.
- The ordinary benchmark moved by about 0.007–0.012 µs per call with the
  same fast-path arithmetic. The current three-run measurement leaves the
  source and stability of that small difference open.
