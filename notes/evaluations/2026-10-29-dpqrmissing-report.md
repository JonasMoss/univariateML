# TASK-78.3: nine density-only families

Base: fab96c190e1abfc43965ca6490ad99a1d0915e19; no commits made.

## Changes

- `dpqr.hpp` and `test.cpp` in pareto2/pareto3/pareto4/fpareto/gb2/
  genbeta/pig/frechet/nsbeta; new `gb2/beta_tails.hpp`;
  four no-throw incomplete-beta wrappers in `special/special.hpp`.
- Density signatures now use README parameter order, explicit scale/log,
  and shape1/shape2/shape3 names for generalized beta. Existing C++
  density test callers updated. No fitter/adapter called these densities.
- Pareto II/III/IV: translated Lomax/log-logistic/Burr p/q/r.
- Feller-Pareto: translated GB2 p/q/r.
- Frechet: translated inverse Weibull p/q/r.
- Nonstandard beta: affine beta p/q/r.
- Delegated wrappers validate parameters/probabilities and handle exact
  endpoints locally; interior numerical behaviour belongs to delegates.
- GB2/generalized beta: direct beta tails, complementary arguments retained
  in log coordinates; log prefactor plus continued fraction after probability
  underflow. Boost inverse beta for ordinary inputs; log-odds bisection for
  deep tails. Gamma-ratio samplers.
- PIG: constant-storage log-mass streaming; smaller tail summed directly;
  geometric upper bound certifies omitted upper-tail mass below 2e-16 of
  the sum. Quantiles use integer bracketing/bisection; sampler mixes Poisson
  with the existing inverse Gaussian sampler.
- No README, R, codegen, golden, or out-of-scope fitter changes.

## Validation

Scratch scripts/logs: `/tmp/umvl-codex/dpqrmissing/`.

Baseline at base commit:
- `just cpp-test`: 5/5 ctest targets; distribution_tests 381 cases,
  511749 assertions; all passed (174.40 s total).
- `just test`: [ FAIL 0 | WARN 0 | SKIP 1 | PASS 8402 ].

Final:
- `just test`: [ FAIL 0 | WARN 0 | SKIP 1 | PASS 8402 ], identical to baseline.
  Skip: optional `{sn}` is not installed.
- `just cpp-test`: 5/5 targets passed (73.62 s). distribution_tests:
  399 cases / 511975 assertions; all five binaries total 417 cases /
  522875 assertions, all passed. Added 18 cases / 226 assertions.
- `git diff --check` passes; no goldens changed; no commits or board edits.

## Limits and integration notes

- Translated/affine wrappers inherit delegate tail defects; see reproducers
  below. These delegates are intentionally not edited by this job.
- A continuous quantile can round to its finite bound or overflow; a
  round-trip at that double then cannot recover the requested tail.
- PIG p/q reject infinite mean (outside README's real parameter domain),
  and return NaN when counts or certified summation exceed 10,000,000.
  Existing density behaviour, including its infinite-mean limit, is retained.
- PIG's geometric bound can be slow/unavailable for extremely heavy tails;
  NaN denotes an uncertified result, never a silently truncated sum.
- Local beta helpers are isolated in `gb2/beta_tails.hpp` for replacement
  with the parallel `special/tails.hpp` work.

Open questions: no decision needed; merge the delegate tail fixes before
claiming tail accuracy for the translated/affine families.

## Reference-package defects (one-line reproducers)

Installed-reference reproducers (no workarounds applied):
- `actuar::qtrbeta(.99,.3,2,.4)` returns 1056.33128562328;
  mpmath/ours 1056.33128570916 (relative defect 8.13e-11).
- `actuar::dgenbeta(.999999551905692,.4,.3,2)` has relative error
  3.97e-11; log upper CDF there has scaled error 3.70e-12.
- `actuar::qtrbeta(-10000,20,2,20,log.p=TRUE)` returns 0;
  ours/mathematical value is about 1.43028e-109.
- `actuar::qgenbeta(-10000,20,1000,2,log.p=TRUE)` returns 0;
  ours/mathematical value is about 2.42093e-110.
- `extraDistr::pfrechet(.01,lambda=2,log.p=TRUE)` returns -Inf; true -10000.
- `extraDistr::qfrechet(-800,lambda=2,log.p=TRUE)` returns 0;
  true 1/sqrt(800).
- `extraDistr::qnsbeta(-800,20,20,log.p=TRUE)` returns 0; true positive.
- `actuar::ppoisinvgauss(99,2,3,lower.tail=FALSE,log.p=TRUE)` returns
  -Inf; mpmath/ours -36.218853990576662.
- `actuar::ppoisinvgauss(1000,1000,1e12,log.p=TRUE)` returns -Inf.
- `actuar::dpoisinvgauss(2,2,1e20,log=TRUE)` returns +log(2);
  true -1.306852819440055 (Poisson limit).

Inherited C++ delegate defects, before the parallel tail fixes:
- `qpareto2(log(1e-300),2,1,0,true,true)` and
  `qpareto4(log(1e-300),2,2,1,0,true,true)` return 0 (true positive).
- `qpareto3(-800,2,1,0,true,true)`,
  `qfrechet(-800,2,0,1,true,true)`, and
  `qnsbeta(-800,20,20,0,1,true,true)` return 0 (true positive).
- Pareto II/IV upper quantile at 1e-12 has log-tail residual 8.89e-5;
  Pareto III/Frechet 2.21e-5, from complementing ordinary probabilities.

## Numerical details

- `probe.R` / `body-points.csv`: 668 d/p/q evaluations against installed
  actuar/extraDistr at README defaults, both tails and scales, body inputs
  at reference quantiles .01/.1/.5/.9/.99 (PIG counts 0:20).
  Error metric `abs(ours-ref)/max(1,abs(ref))`: 655 within 1e-12;
  the other 13 are the reference defects below. `body-truth.py` (100-digit
  mpmath) confirms ours within 1.1e-14 at those 13 points.
- `tails.cpp`, `tails.py`: 180 quantile/CDF rows across nine families;
  probabilities 1e-300/1e-100/1e-30/1e-12 on both input scales and tails,
  plus log inputs -800/-10000. New-tail parameter grids: GB2/Feller-Pareto
  (20,2,20,scale=1), generalized beta (20,1000,2,scale=1), PIG (2,3).
  GB2/Feller-Pareto/generalized beta/PIG:
  80/80 independent checks pass; PIG uses the discrete bracket criterion.
  Maximum absolute log-CDF error against mpmath at returned q:
  GB2/Feller-Pareto 1.14e-13, generalized beta 1.82e-12, PIG 1.71e-12.
- Generalized beta upper log input -10000: q=0.9999792970071513;
  true log-tail residual 2.334e-9; next doubles have residuals +7.697e-9
  and -3.028e-9, so the requested probability lies between neighbors.
  This is output rounding, not incomplete-beta underflow.
- The new cases include 20,000 fixed-seed draws per family and CDFs at
  three points within 5 binomial SE, plus an exact large-shape beta check:
  `pgb2(1e20,1,1,1e20,1,true,true)` = -0.9999999999999992 (true -1).
- `just quality pareto2 pareto3 pareto4 fpareto gb2 genbeta pig frechet nsbeta`:
  all nine pass.
