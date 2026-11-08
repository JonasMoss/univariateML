# TASK-157: stable skew-t absolute moments

Worktree: `codex/sstd-moment`, based on `b1b23aa8`. Validation completed
2026-11-09 (Europe/Oslo). No commits, package installations, board edits,
golden changes or truth-table changes were made.

## Result

The variance-one Student-t absolute moment now has one shared evaluation for
the fitter and Fernandez–Steel d/p/q/r paths, including the analytic Hessian's
moment value and the natural-coordinate inference jets. It avoids subtracting
large log-gamma values and remains finite through `DBL_MAX`.

Independent gamma evaluations over 2,548 binary64 shapes gave maximum moment
errors of **2.17 ulps** with the normal backend and **3.98 ulps** with
long-double math disabled. These are numerical measurements; the approximation
remainder bounds below are proved in exact arithmetic.

All required final suites passed, including workstation and simbox
no-long-double builds, full C++ and R tiers, JS build/tests, and site validation.
The consumer audit also identified separate extreme-shape probability and
sampling defects, recorded below with reproducible evidence.

## Evaluation and error bounds

Write

```text
C(v) = log Gamma(v/2 + 1/2) - log Gamma(v/2) - log(v/2)/2.

m(nu) = sqrt(2/pi) * sqrt(1 - 1/(nu-1)) * exp(-C(nu-1)).
```

For `nu < 129`, the moment uses Boost's no-throw
`tgamma_delta_ratio((nu-1)/2, 1/2)` through `special/special.hpp`, multiplied
by `sqrt((nu-2)/pi)`. This avoids overflow and log-gamma cancellation while
preserving the small distance from two. The Hessian value helper can also take
that distance directly.

For `v >= 128`, the correction is evaluated as

```text
C(v) = -1/(4v) + 1/(24v^3) - 1/(20v^5)
       + 17/(112v^7) - 31/(36v^9) + R(v),
|R(v)| <= 691/(88v^11) < 5.2e-23.
```

The bound follows from
`C(v) = -1/2 integral_0^infinity exp(-v*u)*tanh(u/2)/u du`.
The positive partial-fraction expansion
`tanh(u/2) = 4u sum_j 1/((2j+1)^2*pi^2 + u^2)` gives alternating geometric
remainders of known sign. Integrating their bounds gives the first omitted
term above. Thus the moment switches at `nu = 129`, where `v = nu-1` reaches
128; the Student normalizers switch at `nu = 128`.

The unit-variance t normalizer now combines its leading logarithms analytically:

```text
log f_nu(0) = -log(2*pi)/2 - log1p(-2/nu)/2 + C(nu).
```

This also avoids overflow in `(nu-2)*pi`. Half-step beta normalizers use the
same correction rather than a log-gamma subtraction.

For GED, recurrence removes the small-argument gamma singularities:

```text
Gamma(k/nu)/Gamma(1/nu)
  = Gamma(1+k/nu)/(k*Gamma(1+1/nu)).

m_GED(nu) = sqrt(3)/2 * exp(g(2/nu) - (g(1/nu)+g(3/nu))/2),
g(x) = log Gamma(1+x).
```

The shared `lgamma1p` helper evaluates the degree-12 Taylor series without
first rounding `1+x` when `0 <= x <= 1/32`. Its absolute truncation remainder
is at most `zeta(13)*x^13/(13*(1-x)) < 2.2e-21`; other arguments use libm.
This stabilizes the GED moment, scale and recurrent normalizers and avoids
a measured large-shape libm slowdown.

The inference jet evaluates the same large-shape correction and moment, and
rewrites the Student kernel using `log1p(t)/t` to avoid cancellation in shape
derivatives. Independent 80-digit scores and Hessians are checked at shapes
128, 129, 1e4, 1e8, 1e12 and 1e16; huge-shape jets are also checked for
finiteness. Inference eligibility and covariance validity conditions are
unchanged.

## Consumer audit

| Consumer | Result |
|---|---|
| `dens::sstd_sigma`, `fs::sstd_sigma` | Both use `std_abs_moment`; regression cases require identical shifts and scales. |
| sstd cost, Hessian moment value, certificates and held fits | Shared moment/normalizer; removed the blanket xi-only failure at held `nu >= 128`. Existing root, likelihood, face and budget checks remain. |
| sstd natural-coordinate inference | Stable value and differentiated large-shape formulas; R/JS check finite positive skew covariance for a fit with `nu = 1e12` held. |
| std, st, lst and ht normalizers; `special::dt` log path | Shared stable Student correction; half-step `lbeta` prefactors stabilized too. |
| GED/sged density helpers and FS algebra | Shared recurrent gamma ratios, moment and scale; stable recurrent density normalizer. |
| GED shape-certificate reconstruction; sged face payloads and held fits | Replaced direct small-argument gamma-log differences with the shared helpers. |
| Existing GED bounds and sged logarithmic residual fallback | Bounds already use shifted gamma arguments; the residual fallback serves underflowed centering moments, rather than the large-shape moment path. |

The ht value-profile arithmetic below 128 is retained. Replacing its moderate
values moved the golden scale beyond its existing `1e-8` tolerance along a
rounded likelihood plateau. Large-shape stabilization is applied from 128;
the frozen golden and its tolerance remain unchanged.

The xi-only certificate retains its existing exact-arithmetic argument,
conditional on computed comparisons, with its practical margin of 128 binary64
epsilons times one plus the accumulated absolute terms. Stabilizing the moment
removes the arithmetic reason for the former blanket stop, without changing
that certificate's comparison or stopping rules.

## Numerical evidence

[check.py](2026-11-08-sstd-moment-data/check.py) evaluates the original gamma
definitions independently with mpmath. Working precision rises from 100 to
408 decimal digits so huge gamma evaluations still leave at least 80 useful
digits. The grid includes the adjacent double above two, binary distances
from two, adjacent doubles at both Student switches, a logarithmic sweep, and
`DBL_MAX`.

| Quantity | Normal backend: maximum ulps | No-long-double: maximum ulps |
|---|---:|---:|
| Student absolute moment, whole grid | 2.17 | 3.98 |
| GED absolute moment, whole grid | 1.74 | 1.74 |
| GED lambda, whole grid | 1.62 | 1.62 |
| Student log normalizer, `nu >= 128` | 1.62 | 1.62 |

Detailed results are in [checks.json](2026-11-08-sstd-moment-data/checks.json)
and [checks-no-long-double.json](2026-11-08-sstd-moment-data/checks-no-long-double.json).
The C++ closed-form regression tables contain the independent 80-digit
references. The existing sstd/std truth tables reach shape 12, GED/sged tables
shape 30, and st shape 5; none covers the new large-shape range. All stored
truth files and goldens remain byte-identical, and their existing regression
tolerances pass.

## Final validation

Times are program-reported test times, excluding CPU reservation waits and
build time unless indicated. All runs used the supplied slot wrappers or
`simbox-run`, `nice -n 10`, one-thread BLAS/OpenMP, and worktree temporary files.
JS and site commands used one reserved CPU with CPU affinity restricted to
that CPU; C++/R suites reserved four.

| Command | Machine | Final result | Seconds |
|---|---|---|---:|
| `just cpp-test` | simbox, GCC 13.3 | 106/106 | 66.12 |
| `UMVL_TEST_TIER=full just cpp-test` | simbox, GCC 13.3 | 129/129 | 163.76 |
| `just cpp-test-no-long-double` | simbox, GCC 13.3 | 129/129 | 162.99 |
| `just cpp-test-no-long-double` | workstation, GCC 15.2 | 129/129 | 570.54 |
| `just test` | simbox | fail 0, warn 0, skip 154, pass 22141 | 99.9 |
| `just test-full` | simbox | fail 0, warn 0, skip 0, pass 44973 | 324.3 |
| `just js-build` | workstation | success | 179.74 |
| `just js-test` | workstation | 553/553, no skips | 429.96 |
| `just site-check` | workstation | success, including browser checks | 329.82 |

The final focused JS regressions passed too. The old JS expectation of a
large-shape numerical stop now checks success and agreement with the public
density's log likelihood. The site check regenerated an unrelated tracked
screenshot; its original bytes were restored. No changes remain in NEWS,
STATUS, KNOWN_ISSUES or roadmap files.

## Hot-path timings

[benchmark.cpp](2026-11-08-sstd-moment-data/benchmark.cpp) preserves the original
formulas explicitly. These are medians of three paired trials of 200,000 calls,
workstation GCC 15, C++17 `-O2`, one reserved thread under shared load. Absolute
times depend on that load; these are scalar-function measurements, rather than
end-to-end fit timings.

| Function / shape | Before, ns/call | After, ns/call |
|---|---:|---:|
| Student moment, 5 | 117 | 1051 |
| Student moment, 129 | 123 | 27 |
| Student moment, 1e12 | 114 | 27 |
| Student log normalizer, 128 | 51 | 21 |
| GED moment, 5 | 370 | 171 |
| GED moment, 1e12 | 234 | 46 |

The accurate backend ratio costs more at moderate Student shapes. The
large-shape branches are faster, and the sstd cost already hoists its moment
and normalizer out of the observation loop. Raw trials and medians are retained
in the data directory.

## Open consumer follow-ups outside the moment correction

These are inherited tail/sampling defects, rather than remaining moment
cancellation. The settled d/p/q/r implementations require separate changes
for them. [consumers.cpp](2026-11-08-sstd-moment-data/consumers.cpp) and the
[current simbox output](2026-11-08-sstd-moment-data/consumers-simbox.csv) retain
the deterministic observations.

1. **Student beta-tail bridge/inversion.** `qsstd(.25,0,1,1e100,2)` returns
   NaN, and `psstd(0,0,1,DBL_MAX,2)` returns NaN. Stable half-step log-beta
   normalization alone does not repair the remaining extreme-shape beta-tail
   evaluation/inversion. This shared path needs a stable large-shape bridge
   and verified inversion. R/JS round-trip regressions in this job cover shapes
   through `1e16`; density/moment regressions extend through `DBL_MAX`.
2. **GED gamma-log inversion.** `qsged(.25,0,1,1e100,2)` returns approximately
   `+sqrt(3)`, whereas its uniform limit gives `-sqrt(3)/2`. The same probe
   gives `psged(0) = .5`, so the positive returned quantile also contradicts
   monotonic inversion. `ged/tails.hpp` needs stable tiny-shape gamma-log
   probabilities, a valid bracket, and a checked inversion result.
3. **GED gamma sampling underflow.** At shape `1e8`, all 100 seeded skew-GED
   draws in the probe collapse to approximately `-1.03923048454`, because
   `rgamma(1/nu)` underflows before its power transform. This requires a
   logarithmic or augmentation-based gamma sampling path that preserves the
   final GED amplitude.
4. **WASM libc++ large-gamma nontermination.** The SDK gamma sampler forms
   `3*alpha - .75`; with `alpha = DBL_MAX/2` this is infinity and every
   rejection-loop candidate fails. Thus the standard-library t sampler can
   hang at `DBL_MAX`. The bounded
   [diagnostic](2026-11-08-sstd-moment-data/check-wasm-sampler.py) confirms
   completion at `1e100`, followed by a ten-second timeout at `DBL_MAX`; the
   child is killed and reaped. Its
   [record](2026-11-08-sstd-moment-data/wasm-sampler-check.json) retains the
   markers. An overflow-safe large-gamma sampler is needed. The JS sampler
   regression excludes that endpoint, while its density/moment checks retain
   it; the R sampler endpoint check passed on its different RNG backend.
