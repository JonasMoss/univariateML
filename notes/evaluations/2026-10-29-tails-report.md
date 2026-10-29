# TASK-78.1 — tail helpers and three pilots

Base: `fab96c190e1abfc43965ca6490ad99a1d0915e19`; branch `codex/tails`.
No commits, board edits, golden changes, fitter edits, or other-family edits.
All builds, tests, R probes and mpmath runs use `/tmp/umvl-cpu/run`.
Scratch scripts and full logs: `/tmp/umvl-codex/tails/`.

## Files and implementation

- New `cppunivariate/include/cppunivariate/special/tails.hpp`: documented
  tail rules; `log1mexp`, `log_sum_exp`, `log_diff_exp`,
  `log_neg_log1mexp`, `LogTails`, `tails_from_log`, `tail_probability`,
  `quantile_log_tails`, `log1p_product`, `log_log1p_product`, and
  `scaled_difference`.
- `special/special.hpp` includes that header; `fs_log1mexp` delegates to
  `log1mexp`. Existing `fs_*` interfaces and log-quantile inversion remain.
- `distributions/lomax/dpqr.hpp`: log survival from `log1p`, accurate lower
  complement, quantiles from log survival using `expm1` or its logarithm.
  Tiny lower inputs and large rate/shape combinations retain their logs.
  Density at zero is the right-hand limit `lambda*kappa`.
- `distributions/gumbel/dpqr.hpp`: direct log CDF and complementary log
  survival, with the analytic far-right limit before `exp(-z)` underflows.
  Quantiles use log lower tails or the stable nested logarithm of the
  upper tail. Infinite density arguments return zero.
- `distributions/kumar/dpqr.hpp`: stable log density, log survival and
  lower CDF; log-domain quantiles retain tiny inputs before the shape
  transform. Endpoint densities handle unit shapes without `0*Inf`.
- The three `test.cpp` files add high-precision extreme cases and flag,
  endpoint, invalid-input and representable-neighbour round-trip checks.
  Their comments include the mpmath generating snippets (500 decimal digits).
- New `cppunivariate/tests/test_tails.cpp` and registration in
  `cppunivariate/tests/CMakeLists.txt`: new `tails_tests` CTest target.
- This report. All three sampler bodies match HEAD byte for byte.

## Shared API decisions for subsequent batches

All helpers live in `cppunivariate::special`; include `special/tails.hpp`
for elementary helpers, or `special/special.hpp` for the Boost seam too.
`LogTails` contains `{double lower, double upper, bool valid}`.
`tails_from_log(log_tail, lower_tail)` retains the supplied log and derives
its complement; callers should supply the smaller, accurately computed tail.
`tail_probability(tails, lower_tail, log_p)` selects the log or exponentiates
it. A caller may supply two independently accurate logs in a `LogTails`.

`quantile_log_tails(p, lower_tail, log_p)` preserves both logs directly
(`log(p)` and `log1p(-p)` for natural input). NaN, natural inputs outside
[0,1], and positive log inputs return `{NaN,NaN,false}`. Log input `-Inf`
and `0`, and natural input `0` and `1`, are valid endpoints. Callers map
these to their support; no exceptions or implicit clipping.

The helpers impose no continuous/discrete support convention. Discrete
callers retain responsibility for flooring CDF arguments and selecting the
smallest support point reaching the target. `log_diff_exp(x,y)` returns
NaN for `y>x` or `Inf-Inf`; equal finite inputs and `(-Inf,-Inf)` give
`-Inf`. `log_sum_exp(-Inf,-Inf)=-Inf`; NaNs propagate.

`log1mexp` and `log_neg_log1mexp` accept double/long-double arithmetic
(and integer literals promote as `std::log` does). The nested-log helper
uses its asymptote below -36; omitted relative correction is <1.2e-16.
Reviewer portability revision: all three pilots now use **double only**.
`log1p_product` guards multiplication before overflow; its nested-log variant
retains products below the subnormal range. Lomax scales hazards and
quantiles in log space. Kumaraswamy retains tiny powers and the final shape
transform in log space. Gumbel divides before an overflowing subtraction
and uses `std::fma` for a finite affine quantile despite product overflow.
The header explicitly forbids relying on extended range: long double equals
double on some CRAN platforms. No pilot contains a long-double expression.
Parameters must be finite and positive (Gumbel location finite); NaNs
propagate, infinite observations obey the support limits.

## Validation

Baseline before source edits:

- `just cpp-test`: **5/5 targets passed**; 399 cases, 522649 assertions.
  Distribution target: 381 cases, 511749 assertions.
- `just test`: **[ FAIL 0 | WARN 0 | SKIP 1 | PASS 8402 ]**.
  The skip is the optional uninstalled `sn` comparison package.

Post-review checks (double-only revision):

- `just cpp-test`: **6/6 targets passed** (baseline 5); **412 cases,
  522941 assertions**. Distribution tests: 390 cases / 511894 assertions;
  helper tests: 4 cases / 147 assertions.
- `just test`: **[ FAIL 0 | WARN 0 | SKIP 1 | PASS 8402 ]**,
  unchanged from baseline; the same optional `sn` comparison is skipped.
- Earlier `just quality lomax gumbel kumar`: all three passed logLik,
  score/Hessian and eight-start oracle checks (maximum gap 7.96e-13).
- Native and `-mlong-double-64` builds each pass the original **760**
  mpmath comparisons and an expanded **26784** comparisons, with no skips.
  Expanded grid: 144 rate/shape pairs each for Lomax and Kumaraswamy;
  72 location/scale pairs for Gumbel. The 12 positive parameter values are
  `1e-200,1e-100,1e-20,.1,.5,1,2,3,10,1e20,1e100,1e200`;
  Gumbel locations are `-1e200,-1,0,1,3,1e200`. Both d/p scales and both
  q tails/scales are checked. Observations: Lomax 0..1e300 (including 1e-300),
  Kumaraswamy 0..1, Gumbel -1000..1e4; log inputs through -1e6.
- At 100 decimal digits, maximum normalized error on the expanded grid is
  **1.602e-13**, below **5e-12**; the original grid maximum is 7.179e-14.
  Normalization is relative with a 1e-300 floor for natural outputs and
  `max(1,abs(truth))` for log d/p. No tested parameter range was lost.
- Build flags: `g++ -std=c++17 -O2 -shared -fPIC -Icppunivariate/include`,
  adding `-mlong-double-64 -DUMVL_ASSERT_LONG_DOUBLE_64` for the second build.
  Static assertions verify double precision and exponent range for long
  double. The final shared libraries are byte-identical and use double libm.
  Scripts: `grid-portable.cpp`, `grid-portable.py`, `grid-wide.py` in scratch;
  final results: `portable-confirm.log` (both builds).
- Added doctests cover product underflow/overflow and Gumbel affine overflow;
  500-digit constants also cover Kumaraswamy shape 1e-320. Separate
  `portable-edges.py`: **8/8 pass in each build**, maximum error 4.590e-14,
  including shapes 1e-320 and affine parameters 1e308. Focused optimized
  doctests: **13 cases / 292 assertions passed**.
- Samplers and goldens remain unchanged; `git diff --check` passes.

## Initial probe results (before portability revision)

The original `gen.R`/`probe2.R` are copied into scratch, paths redirected
here, and generation restricted to the three requested pilots. The
unmodified baseline probe reports 24/14/25 wrong checks for
Lomax/Gumbel/Kumaraswamy respectively.

Initial implementation raw probe (`r2-final.csv`): **1/2/8 flagged checks** respectively.
These are harness false positives, verified against closed forms:
- Lomax upper log input -10000 requires `expm1(5000)`, beyond double range;
  the raw harness unconditionally rejects an infinite quantile.
- Gumbel log inputs -10000: correct quantiles -24.63102111592855 and 30003
  produce quadrature answers -10171.32815 and -10017.37689 instead of -10000.
- Kumaraswamy: 3 natural and 5 log upper-tail inputs round correctly to 1;
  quadrature cannot bracket these using the raw harness's neighbours.

`probe2-closed-form.R` retains the original grid, comparisons and tolerance,
substitutes independent analytic pilot CDFs for quadrature, and permits
endpoint/overflow bracketing. **849 checks, ours_wrong=0 for all three**
(`r2-closed-form.csv`). It does not substitute our C++ CDF as truth.
The separate 100-digit mpmath grid validates the formulas independently.
Both final probes load the package with `pkgload::load_all(..., compile=FALSE)`
and force `sourceCpp(..., rebuild=TRUE)` to avoid a stale absolute-header cache.
No tracked probe scripts are changed; all variants/results remain in scratch.

Reproduce after `just test`, each command through `/tmp/umvl-cpu/run`:
`Rscript /tmp/umvl-codex/tails/gen.R`, then
`Rscript /tmp/umvl-codex/tails/probe2.R final lomax gumbel kumar`, and
`Rscript /tmp/umvl-codex/tails/probe2-closed-form.R closed-form lomax gumbel kumar`.

Open question for the shared truth-table work: promote the pilot closed-form
oracles and support/overflow bracketing into its harness? Literal zero wrong
checks with the original quadrature harness is not met; changing correct
quantiles to satisfy those numerical artifacts would violate this job's
accuracy requirement. No remaining pilot implementation failures were found.
