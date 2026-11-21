# Truncated-normal exponential limit payloads

TASK-214.46. The four strict nesting entries pass in native and double-only
arithmetic. Archive 7657 returns rate `-443614287124.62604` and limit log
likelihood `135.7988232106926`. Both archive cases still escape to the
truncated exponential.

## Cause

The old finite-window solver standardized the window around the rounded
absolute sample mean, solved for a tilt with standardized mean zero, and
evaluated the likelihood at that rounded sufficient statistic. For decay
rate $\lambda$, width $w=b-a$, and $u_i=x_i-a$, the score is
$n/\lambda-\sum_i u_i-nw/(e^{\lambda w}-1)$. On archive 7657, $\lambda w$
is moderate and $|\lambda x|$ is about $10^{11}$. Rounding the absolute
mean before centering therefore introduces score error on the scale of
$10^{11}\epsilon$. Replacing the exact mean with its rounded binary64
value in the 60-digit calculation reproduces the old rate
`-443512236560.93695` and limit likelihood `135.79877396527803` to
rounding. The rate error was about $2.3\times10^{-4}$ relative and the
likelihood was low by $4.925\times10^{-5}$. The old one-sided faces used
the reciprocal of the difference between the rounded mean and the
endpoint, with the profile value $n(\log\lambda-1)$.
[Archive calculation](2026-11-20-tnorm-nesting-data/check.py) ·
[Rounded-mean replay](2026-11-21-tnorm-limit-data/references.txt)

## Coefficient rule and comparison

- **Translated and reflected exponential.** With mean distance $d$ from
  the finite endpoint, the average log likelihood is
  $\log\lambda-\lambda d$. Its unique maximum is $\lambda=1/d$. The
  computation keeps the compensated sample-mean correction in the
  endpoint difference and checks the rounded reciprocal with a fused
  product residual. The profile value is $n(\log\lambda-1)$. The core
  stores the signed tilt and the payload carries a positive decay rate
  for either one-sided model. See lines 611–635 and 790–791.
  [Implementation](../../../univariateML-cpp/include/cppunivariate/distributions/norm/tnorm_mle.hpp)
- **Truncated exponential.** Scale the window to unit width and reflect
  toward the higher-density endpoint. The score for a signed tilt is
  the data mean minus the tilted mean. Its derivative is minus the
  strictly positive tilted variance, which proves uniqueness and makes
  opposite score signs enclose the maximum. The bisection uses
  midpoint-centered paired quadrature for small tilts and an
  endpoint-distance formula for large tilts. The final sign checks
  enclose the rounded public rate within its requested tolerance, using
  the contract's unit floor near zero and a practical eight-epsilon
  score margin. An exact two-word sum can verify a zero tilt when the
  numerical bracket cannot resolve it. For nonnegative tilt magnitude
  $t=|r|w$ and mean unit distance $d$ from the mode, the profile value per
  observation is $-\log w-td-\log((1-e^{-t})/t)$, with the continuous
  value at zero. See lines 329–446.
  [Implementation](../../../univariateML-cpp/include/cppunivariate/distributions/norm/tnorm_mle.hpp)

The profile derivative at the face is $n$ times the empirical variance
minus the matched face variance. The comparison uses the corrected sample
center and compares standard deviations before squaring their ratio. This
keeps the face variance comparison usable when a wide window would cause
both unit-scale variances to underflow. The accurate profile value enters
the existing finite-fit comparison at $10^{-8}$ per observation. The
coefficient enclosure uses the package's practical arithmetic margins.
See lines 611–655.
[Implementation](../../../univariateML-cpp/include/cppunivariate/distributions/norm/tnorm_mle.hpp) ·
[Accuracy contract](../../design/accuracy-contract.md)

## Snapshots and strict nesting

The comparison uses the stored 2026-11-15 snapshots, which matched the
worktree base `67ff1d73`. The route counts and maximum moves agree in
both modes.

| Route record class | Changed / replayed | Largest relative move |
| --- | ---: | --- |
| Archive limit | 19 / 29 | Rate 1 at 7651, from `1.49566e-13` to exact zero. Largest between nonzero rates `2.30096e-4` at 7657. Largest likelihood move `3.62635e-7` at 7657. |
| Archive numerical | 0 / 7 | 0 |
| Archive estimate | 0 / 220 | 0 |
| Class invalid | 0 / 4 | 0 |
| Class nonexistence | 0 / 3 | 0 |
| Class estimate | 0 / 13 | 0 |
| Golden estimate | 0 / 1 | 0 |
| Resample estimate | 0 / 8 | 0 |

All 1,545 canonical records per mode were identical. They cover `norm`
and `beta`, `burr`, `fatigue`, `ged`, `gompertz`, `invburr`, `kumar`,
`pois`, `snorm`, and `std`, whose test files include the header directly
or through the library umbrella. No route changed result kind or selected
model. Archive 7779 changed from a numerical tie selecting the translated
exponential to a certified escape to the same model. Its two observations
include the fitted lower endpoint, and both the empirical standard
deviation and mean excess equal half the gap. The quadratic profile
derivative is exactly zero, so strict concavity gives the boundary
supremum.
[Snapshot comparison](2026-11-21-tnorm-limit-data/snapshot-diff.txt) ·
[Exact case 7779 calculation](2026-11-21-tnorm-limit-data/references.txt)

The 60-digit replay covered all 21 exponential payloads per mode. The
maximum contract-scaled rate error was `2.9432e-15` and the maximum
scaled likelihood error was `2.9581e-16`. Archive 7660 is a separate
near-constant sample around +1. It returns `-145263484292.27478` and
`130.21638966013222` in both modes. Direct `fit-judge.py` replay judged
285 route records per mode and reported all four named cases as passing.
The paired held-scale comparisons are 7657 with 7689 and 7660 with 7692.
[60-digit payload replay](2026-11-21-tnorm-limit-data/payload-replay.txt) ·
[Strict judge output](2026-11-21-tnorm-limit-data/fit-judge.txt)

## Tests

The appended C++ tests check the exact binary64 input for archive 7657,
all three exponential face types at `1e-6` and `1e-13`, both signs of
finite-window tilt, and a zero tilt. Additional cases cover exact balance
in a tiny window, an interval wider than the largest double, and an
attained fit whose unit-interval exponential variance underflows. An early
native run exposed overflow in the mean correction for the extreme window.
The later standard-deviation fix landed during a double-only build,
which could link the earlier inline implementation. A fresh standalone
probe and a rebuild of every dependent test object verified the final
header in both modes. See lines 727–790 for the appended cases.
[C++ regressions](../../../univariateML-cpp/include/cppunivariate/distributions/norm/test.cpp) ·
[Fixture references](2026-11-21-tnorm-limit-data/references.py)

| Final check | Workstation result |
| --- | --- |
| `just test-changed --no-long-double` | 34/34 CTest shards in each mode with GCC 15.2. Targeted R reported 9,608 passes, 63 skips, zero failures or warnings. The JS build passed and 368/368 JS tests passed. |
| Additional C++ checks including `modifier_tnorm` | 7/7 CTest shards in each mode. |
| Dedicated R `modifier-kernel-tnorm`, full tier | 131 passes, zero failures, warnings, or skips. |
| Route and canonical snapshots, direct judge, and 60-digit replay | Both modes passed as detailed above. |
| `just site-check` | 85 family pages passed. |
| `python3 dev/tools/check_links.py` | Zero new broken links, 45 inherited. |

## Open

The finite-window coefficient check still needs a resolved score bracket or
an exact zero-score calculation. Tolerances below the arithmetic margin and
held windows whose unit-scale reciprocal exceeds binary64 can leave that
solve unresolved. A different scaling for the extreme endpoint tail and
specific attainable-tolerance diagnostics remain open.
