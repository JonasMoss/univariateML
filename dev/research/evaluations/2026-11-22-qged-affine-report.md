# TASK-226 · GED quantile affine reconstruction

The fixed `qged` returned the nearest binary64 coordinate on all 4,784
probes in each precision mode. The maximum natural-probability round-trip
error fell from $2.02\times10^{-5}$ to $8.95\times10^{-8}$. The largest
coordinate error fell from 1,783 binary64 steps to zero. With the relaxed gate and a fast upper-tail residual, the largest
median timing ratio for the ordinary affine pairs was 1.46 relative to
master across the natural and log workloads. The dossier
specifies $10^{-9}$ relative accuracy in the returned coordinate. The
truth harness accepts both neighbours of the exact coordinate and uses
an absolute floor at exact zero. Nearest rounding across this sweep is a
numerical result. [GED dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ged/README.md#L478)

## Probability error and the binary64 grid

The sweep uses the 26 shapes from TASK-221, from 1 through $10^6$, including
999999. It uses both probability tails and the affine pairs $(0,1)$,
$(0.3,1.7)$, $(-10^{100},10^{100})$, and $(10^{-100},10^{-100})$. Natural
inputs reach $10^{-300}$ and log inputs reach $-10^4$. The independent
reference evaluates the gamma integrals with mpmath at 100 digits using
the exact binary64 inputs. Its log-radius inverse has a checked bracket
of width below $10^{-24}$. The replay checks that this bracket locates the
same two adjacent coordinates and the same nearest coordinate throughout
the bracket. [TASK-221 sweep](2026-11-22-pged-tail-report.md) · [Quantile replay](2026-11-22-qged-affine-data/sweep.py)

Let $T$ be the reference tail on the supplied scale, $x_-$ and $x_+$ the
binary64 neighbours bracketing the exact quantile, $x_*$ the nearest
coordinate, and $\widehat x$ the returned coordinate. The lattice gap (a)
is $|T(x_+)-T(x_-)|$. The actual rounding contribution is $T(x_*)-p$.
The reconstruction contribution in probability (b) is
$T(\widehat x)-T(x_*)$, and its coordinate error is the number of binary64
steps from $x_*$ to $\widehat x$. The last contribution is the error of
`pged` at $\widehat x$. On the natural scale divide the probability
differences by $p$. On the log scale divide them by $\max(1,|p|)$.

Both precision modes gave identical results. The columns report separate
maxima, which can occur at different inputs.

| Input scale and build | Probes per mode | Round-trip error | (a) Neighbour gap | Nearest rounding | (b) Reconstruction shift in probability | (b) Coordinate steps |
|---|---:|---:|---:|---:|---:|---:|
| Natural, baseline | 2,288 | 2.02468e−5 | 1.96568e−7 | 8.95494e−8 | 2.02467e−5 | 1,783 |
| Natural, fixed | 2,288 | 8.95491e−8 | 1.96568e−7 | 8.95494e−8 | 0 | 0 |
| Log, baseline | 2,496 | 2.15367e−8 | 2.92388e−10 | 1.25802e−10 | 2.15383e−8 | 679 |
| Log, fixed | 2,496 | 1.25803e−10 | 2.92388e−10 | 1.25802e−10 | 0 | 0 |

The worst baseline round trip uses the upper natural tail at $p=10^{-300}$,
$\nu=999999$, and $\mu=\sigma=10^{-100}$. The returned coordinate is
103 steps below $x_*$, with a neighbour gap of $1.96568\times10^{-7}$.
The signed relative contributions are $+5.61\times10^{-11}$ from nearest
rounding, $+2.02467\times10^{-5}$ from reconstruction, and
$+3.66\times10^{-13}$ from CDF evaluation. The worst fixed round trip uses
the same input at $\nu=10^6$. It lands at $x_*$, with relative contributions
$+8.95494\times10^{-8}$ from rounding and $-3.42\times10^{-13}$ from the
CDF evaluation. The maximum relative error in the returned coordinate
across the grid was $2.49\times10^{-13}$ before the fix and
$1.06\times10^{-16}$ after it.
[Baseline native](2026-11-22-qged-affine-data/baseline-native.json) · [Baseline double](2026-11-22-qged-affine-data/baseline-double.json) · [Fixed native](2026-11-22-qged-affine-data/fixed-native.json) · [Fixed double](2026-11-22-qged-affine-data/fixed-double.json)

## Double-path bound and affine reconstruction

The previous expression formed the physical distance with
`exp(log(sd) + logscale + w)`, where $w$ is the gamma log radius. At the
extreme scales, the magnitude of $\log\sigma$ controls the precision of
the sum before exponentiation. The affine addition can then amplify the
distance error when location and distance nearly cancel.

For $\nu\ge1$ the fast path starts from the double inverse. At $\nu=1$
the seed follows directly from $Q(1,t)=e^{-t}$. Write $a=1/\nu$,
$t=e^{\nu w}$, $F=e^w/\Gamma(1+a)$, and $v$ for the supplied probability
converted to the natural scale. The lower gamma probability and its slope
in $w$ are
$$
P=F\left(1+a\sum_{n\ge1}\frac{(-t)^n}{n!(a+n)}\right),
\qquad S=F e^{-t}.
$$
The signed Newton correction $s$ uses the residual $|2v-1|-P$ and the
slope $S$. Evaluating this residual at the actual double seed accounts
for the seed's inversion and probability conversion errors. The fast
path uses two binary64 words for the series, the standardized
exponential and the affine sum. The shape cache uses the gamma
recurrence in product form up to $64+a$ and the existing three-word
Stirling expansion to supply constants on a cache miss.
[Two-word operations](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L191) · [Shape cache](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L258) · [Quantile entry point](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L443)

With $u=2^{-53}$, TwoSum and FMA retain the high-word rounding residual.
The discarded low-word products and roundings are bounded by
$16u^2$ times the operand magnitude envelope per compensated operation
with normal intermediates. The division correction uses the product
residual and thus divides an $O(u)$ residual once in binary64. For the
exponential, binary reduction and division by 16 give a Taylor argument
with magnitude below $0.022$. The degree-12 Taylor remainder is bounded
by $e^{0.022}0.022^{13}/13!$. Allowing for the two-word operations, the
split constant for $\log 2$, and four squarings gives a relative allowance
of $2^{-90}$. For the gamma series the degrees 16, 28 and 64 cover
$t\le1/16$, $t\le1$ and $t<8.01$, respectively. Each absolute truncation
error is bounded by the first omitted term because the remaining terms
decrease and alternate. The sum of absolute terms is at most $e^t$.
Under this envelope, the coefficient recurrences and Horner evaluation
contribute less than $8192u^2 e^t$. The perturbation in $t$ contributes
at most $8.01\,2^{-90}e^t$, from differentiating the absolute series.
These terms, the factor evaluation and the three-word constant margins
fit within $2^{-83}F e^t$. When $\nu w<-80$ the discarded power is less
than $e^{-80}$. [Two-word exponential](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L215) · [Series and gate](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L285)

Dividing the residual allowance by $S$ gives $2^{-83}e^{2t}$ in $w$
units. For log input, the code adds $2^{-88}v/S$ to allow for converting
the probability before forming $|2v-1|$. The final term
$32u(1+t)|s|$ covers the rounded power supplied to the slope, the
binary64 slope and the scalar division, conditional on a near-ulp
library exponential. Call the sum $E$, set $\delta=|s|+E$, and
take $K=1+2\nu t$. The guards $\nu\delta\le10^{-3}$ and
$K\delta\le10^{-4}$ imply
$|(\log S)'|=|1-\nu e^{\nu w}|\le K$ in the interval of radius $2\delta$.
The slope there is at least $S e^{-2K\delta}$, so monotonicity puts the
root in that interval. Integrating the slope change bounds the error
left by the linear Newton correction by $4K\delta^2$. Including the
evaluation margin gives the radius allowance $R=2E+4K\delta^2$.
[Residual and curvature guard](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L299)

The upper-tail fast evaluator covers $8.01<t\le e^{32}$ within the
affine range checks. It uses the same fraction as the three-word
fallback below, with a 32-level backwards enclosure of $r_1$. At each
level it evaluates the lower endpoint using the smaller numerator and
larger denominator, and vice versa for the upper endpoint. It uses
$a(1\pm2u)$ and $t(1\pm2u)$ to cover rounding the expansion inputs to
binary64. The endpoint updates pad the quotient by $16u$ in each
direction. The numerator uses two arithmetic operations, and its
subtraction near $a=1$ is exact for the rounded operands by Sterbenz's
lemma. For the denominator, the positive quantity
$t+1-a+2n-r_{n+1}$ is at least $t+n-a$ because
$0\le r_{n+1}\le n+1$. The sum of magnitudes in the denominator
calculation is less than six times that lower bound in this range.
The rounding of those operations, the quotient and the padding itself
fit within the $16u$ allowance.

Write $D=t+1-a-r_1$ and $\ell=\log Q(a,t)$. The evaluator forms
$\ell=w-t+\log a-\log\Gamma(1+a)-\log D$ in two words. For $\nu=1$
it uses $\ell=-t$ and $D=t$. Computing a logarithm in two words reduces
its argument to a binary mantissa and corrects the double logarithm
using the exponential residual. A fraction interval of width $B$
contributes at most $2B/D$ to the log-tail error because
$D\in[t-a,t+1-a]$. The $2^{-83}(1+t+|\mathrm{target}|+|\log a|)$
allowance covers the two-word power, logarithms and residual. For a log
input whose complementary probability is smaller, the allowance also
includes $4u$ for the logarithm of the rounded `expm1` result.
Dividing these terms by the slope magnitude $\nu D$ and adding
$32u|s|$ for the scalar slope and correction gives $E$ in radius units.
The curvature bound is $K=1+\nu$, since
$(\log|\ell'|)'=1+\nu(D-t)$ and $-a\le D-t\le1-a$.
This uses the same Newton remainder and affine error formulas as the
central evaluator. [Two-word logarithm](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L231) · [Upper-tail correction](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L307)

The fast distance $\widehat d$ uses $\sigma e^{w+\log\lambda}(1+s)$,
with the appropriate sign. Multiplying by the input scale directly
avoids adding $\log\sigma$ to the radius. The exponential remainder
between the exact root and $1+s$ is bounded by
$R+2\delta^2 e^{2\delta}$. The physical error allowance before final
rounding is
$$
(2R+4\delta^2+2^{-89})|\widehat d|+2^{-89}|\mu|.
$$
The $2^{-89}$ terms cover the standardized exponential and the
compensated affine operations. The gate compares the allowance with $2^{-50}|\widehat x|$, or eight
unit roundoffs relative to the returned coordinate before allowing for
the last rounding. The gate preserves the $10^{-9}$ relative accuracy
budget with room for that rounding. At cancellation the threshold
shrinks with $|\mu+\widehat d|$ while the error allowance still scales
with the operands. If the range checks or error bound fail, the
three-word path retains the scaled affine reconstruction.
[Physical error and lattice gate](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L341)

Both paths met the relative coordinate contract on every measured case.
The full 4,784-point grid used the fast path throughout. To exercise the
fallback separately, the replay adds 64 cases using
$(10^{200},10^{200})$ and $(10^{-200},10^{-200})$, four shapes from 1
through $10^6$, and both tails. These pairs put the scale outside the
fast reconstruction range. Both builds gave identical results.

| Path and input | Probes per build | Max steps from nearest | Max coordinate relative error | Max round-trip error |
|---|---:|---:|---:|---:|
| Fast, natural | 2,288 | 0 | 1.06e−16 | 8.95e−8 |
| Fast, log | 2,496 | 0 | 1.02e−16 | 1.26e−10 |
| Fallback, natural | 32 | 0 | 7.45e−17 | 4.61e−8 |
| Fallback, log | 32 | 0 | 9.76e−17 | 8.70e−11 |

The observed zero-step error applies to these grids. The relaxed gate
keeps deep-tail polishing on the fast path when its coordinate error
allowance fits the physical scale, and leaves more difficult affine
reconstructions to the fallback.
[Fallback native](2026-11-22-qged-affine-data/fixed-native-fallback.json) · [Fallback double](2026-11-22-qged-affine-data/fixed-double-fallback.json)

The fallback recovers the radius residual in three words
against the exact binary64 probability input. Write $a=1/\nu$ and
$t=\exp(\nu w)$. Near the centre, the lower gamma series evaluates the
absolute central mass $|2p-1|$, using $\exp(p)$ for log inputs. Its
derivative with respect to $w$ is $\exp(w-t)/\Gamma(1+a)$. Away from the
centre, the upper gamma continued fraction has denominator
$D=t+1-a-r_1$, with

$$
r_n=\frac{n(n-a)}{t+1-a+2n-r_{n+1}}.
$$

For $a\le1$, substituting the interval $[0,n+1]$ for $r_{n+1}$ maps it
into $[0,n]$. Backward evaluation of both endpoints therefore encloses
the continued fraction in exact arithmetic. The implementation checks
the computed interval width against $10^{-34}t$ in three-word arithmetic.
The derivative
of $\log Q(a,t)$ with respect to $w$ is $-\nu D$. Two Newton corrections
recover the low words, and the final residual checks the correction
before use. A rejected refinement retains the bracketed double radius.
For $-\log Q>10^{100}$, the relative correction from replacing $t$ by
$-\log Q$ in $\log t$ is below the expansion precision.
[Upper fraction](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L106) · [Radius refinement](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L127)

The fallback affine step adds the precise GED log scale to the refined radius and
reduces its exponent by a binary power. It combines the resulting mantissa
with the signed scale mantissa in a common binary exponent with the
location. The expansion keeps the FMA product residual and the low words
of the standardized radius through the affine sum. Rescaling at the end
lets an individually overflowing physical distance cancel with the
location and return a finite coordinate.
[Scaled affine sum](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L366) · [Quantile entry point](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp#L443)

## Timing

Median CPU microseconds per call on workstation CPU 0 with GCC 15.2.0
at `-O2`. Each cell shows master `955412ee` / branch. For each of
five repetitions, the runner times the two builds back to back for the
same row and alternates their order. Each measurement uses 39,960 calls
with warm shape caches. Natural inputs interleave $i/1000$ and
$10^{-300i/999}$. Log inputs interleave $-i/2$ and $-10^4 i/999$, with
$i=1,\dots,999$ in both cases. Both tail flags are timed separately.
[Source](2026-11-22-qged-affine-data/bench.cpp) · [Runner](2026-11-22-qged-affine-data/bench.sh) · [Master samples](2026-11-22-qged-affine-data/timing-master.csv) · [Branch samples](2026-11-22-qged-affine-data/timing-fixed.csv)

The largest ratio of medians on the ordinary pairs was 1.46 at shape
1000 for the unit-scale natural lower tail (4.35 / 6.33 µs). The
largest ordinary log ratio was 1.26 at shape 50 for the upper tail
at $(0.3,1.7)$ (4.12 / 5.17 µs). The
largest ratio across all pairs was 1.57 at shape 1000 for the log
upper tail at $(-10^{100},10^{100})$ (3.98 / 6.27 µs). All ordinary
rows meet the 1.5 target.

| Shape | $(\mu,\sigma)$ | Natural lower | Natural upper | Log lower | Log upper |
|---:|---|---:|---:|---:|---:|
| 1 | $(0,1)$ | 4.49 / 1.01 | 4.41 / 1.00 | 3.26 / 0.52 | 3.31 / 0.52 |
| 1 | $(0.3,1.7)$ | 4.46 / 1.01 | 4.53 / 1.02 | 3.32 / 0.52 | 3.31 / 0.53 |
| 1 | $(-10^{100},10^{100})$ | 4.54 / 1.01 | 4.49 / 1.01 | 3.28 / 0.52 | 3.31 / 0.52 |
| 1 | $(10^{-100},10^{-100})$ | 4.52 / 1.01 | 4.51 / 1.00 | 3.39 / 0.54 | 3.40 / 0.54 |
| 2 | $(0,1)$ | 9.65 / 10.35 | 9.35 / 10.34 | 7.36 / 8.46 | 6.52 / 7.97 |
| 2 | $(0.3,1.7)$ | 10.48 / 11.63 | 10.62 / 12.19 | 7.15 / 8.19 | 7.14 / 6.31 |
| 2 | $(-10^{100},10^{100})$ | 10.57 / 11.54 | 10.44 / 11.62 | 7.20 / 8.23 | 7.22 / 8.27 |
| 2 | $(10^{-100},10^{-100})$ | 10.06 / 11.82 | 10.73 / 11.62 | 7.24 / 7.94 | 7.40 / 7.79 |
| 5 | $(0,1)$ | 8.25 / 9.46 | 8.17 / 8.87 | 6.91 / 7.69 | 7.04 / 8.29 |
| 5 | $(0.3,1.7)$ | 8.47 / 9.53 | 8.03 / 9.56 | 6.95 / 8.07 | 6.64 / 7.83 |
| 5 | $(-10^{100},10^{100})$ | 8.42 / 9.51 | 8.41 / 9.51 | 6.88 / 7.87 | 6.88 / 7.93 |
| 5 | $(10^{-100},10^{-100})$ | 8.09 / 9.48 | 8.37 / 9.63 | 6.05 / 6.34 | 6.48 / 7.46 |
| 50 | $(0,1)$ | 4.75 / 5.80 | 4.74 / 5.74 | 4.61 / 5.48 | 4.65 / 5.59 |
| 50 | $(0.3,1.7)$ | 4.72 / 5.73 | 4.72 / 5.73 | 4.20 / 5.18 | 4.12 / 5.17 |
| 50 | $(-10^{100},10^{100})$ | 4.54 / 5.48 | 4.54 / 5.50 | 4.20 / 5.18 | 4.26 / 5.28 |
| 50 | $(10^{-100},10^{-100})$ | 4.57 / 5.52 | 4.54 / 5.52 | 4.92 / 4.95 | 4.48 / 5.40 |
| 1000 | $(0,1)$ | 4.35 / 6.33 | 5.23 / 6.39 | 5.39 / 6.29 | 5.17 / 5.25 |
| 1000 | $(0.3,1.7)$ | 4.41 / 5.60 | 5.34 / 5.25 | 3.86 / 4.70 | 3.85 / 4.79 |
| 1000 | $(-10^{100},10^{100})$ | 4.15 / 5.04 | 4.15 / 5.07 | 3.80 / 5.46 | 3.98 / 6.27 |
| 1000 | $(10^{-100},10^{-100})$ | 5.09 / 6.03 | 4.53 / 5.90 | 4.81 / 5.70 | 4.87 / 6.04 |
| 1000000 | $(0,1)$ | 5.27 / 6.33 | 4.83 / 6.34 | 5.43 / 6.55 | 5.43 / 6.75 |
| 1000000 | $(0.3,1.7)$ | 5.26 / 6.41 | 5.32 / 6.37 | 5.45 / 6.69 | 5.80 / 6.96 |
| 1000000 | $(-10^{100},10^{100})$ | 5.33 / 6.53 | 5.29 / 6.50 | 5.44 / 6.62 | 5.48 / 6.66 |
| 1000000 | $(10^{-100},10^{-100})$ | 5.21 / 6.13 | 5.24 / 6.50 | 4.83 / 5.83 | 5.25 / 6.34 |

## Fixtures and final checks

`just dpqr-truth ged` added both extreme affine pairs at shapes $10^3$,
999999, and $10^6$. The regenerated table adds six parameter points,
six normalization rows, 114 density/CDF rows, and 264 quantile rows.
A parsed comparison found exact equality for every earlier row and both
sets of sampler indices. The input fingerprint changed with `truth.py`.
Two appended doctest cases check both reflections against independent
quantiles, near-zero cancellation against the exact Laplace formula,
overflow cancellation, subnormal rounding, and location dominance.
[Truth route](../../../univariateML-cpp/include/cppunivariate/distributions/ged/truth.py#L8) · [Affine regression](../../../univariateML-cpp/include/cppunivariate/distributions/ged/test.cpp#L849) · [Range regression](../../../univariateML-cpp/include/cppunivariate/distributions/ged/test.cpp#L873)

The accuracy, timing, and validation reruns used the workstation. C++ used GCC 15.2.0 and the
double-only mode defined `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`.
The targeted R run used R 4.6.1.

| Check | Result |
|---|---|
| Quantile sweep | 4,784 main-grid and 64 fallback comparisons per mode matched every nearest coordinate |
| `just truth-check` | 36 passed, including metadata checks and regeneration of the spot tables |
| `just test-changed --no-long-double` | 5/5 GED and SGED CTest shards in each mode. R had 1,273 passes, 12 skips, zero warnings, and zero failures. JavaScript built and passed 234/234 tests. |
| Supplemental CTest | LST and Norm passed in both modes, 2/2 each |
| Site check from the preceding revision | Passed, including 85 family pages and 85 compiled C++ snippets. The dossier is unchanged in this revision. |
| Fit snapshots | 173 GED and 285 SGED matched branch point `7a57d011` in each mode. The stored snapshots agree on all GED fits and contain 15 historical SGED differences per mode. |

The 15 stored SGED differences are already present at the branch point
and match the held-SD location change from TASK-150.9. Nine estimates
moved and six numerical stops became certified estimates in that earlier
job. A fresh replay of the branch-point headers isolated this comparison
from TASK-226. [Snapshot replay](2026-11-22-qged-affine-data/snapshots.sh) · [Snapshot differences](2026-11-22-qged-affine-data/snapshot-diff.txt) · [TASK-150.9 results](2026-11-22-sged-loc-report.md)

To reproduce the numerical comparisons from the worktree root, run the
probe and snapshot replays in the single-slot launcher. The scripts
reconstruct the baseline from `7a57d011` under `build/scratch`.

```sh
data=dev/research/evaluations/2026-11-22-qged-affine-data
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run sh "$data/build.sh" baseline fixed
/tmp/umvl-cpu/run env PYTHONPATH=dev/validation python3 "$data/sweep.py" baseline fixed
/tmp/umvl-cpu/run env PYTHONPATH=dev/validation python3 "$data/sweep.py" fixed --fallback
/tmp/umvl-cpu/run sh "$data/snapshots.sh"
/tmp/umvl-cpu/run sh "$data/bench.sh"
```

## Open

- Unit-skew SGED delegates the entire quantile to GED and inherits this
  reconstruction. The ordinary non-unit route calls `qged` for a
  standardized quantile and then applies its own affine transform. Its
  log-distance fallback also has a separate final addition. Extending
  the lattice comparison to those non-unit routes requires an
  independent skew-GED quantile reference and joint skew/scale probes.
  [SGED quantile routes](../../../univariateML-cpp/include/cppunivariate/distributions/sged/dpqr.hpp#L281)
- A global nearest-rounding guarantee would require enclosures for the
  refinement and final scaling at rounding midpoints, including the
  subnormal grid and the limiting scale approximation. The measured
  agreement here covers the stated shape and affine grid.
