# Skellam implementation report

Status: complete; all final checks passed. No commits or git writes.

## Files changed

New family files under `univariateML-cpp/include/cppunivariate/distributions/skellam/`:
`README.md`, `mle.hpp`, `dpqr.hpp`, `adapter.cpp`, `dpqr.R`, `test.cpp`,
`test.R`, `golden.json`, `spec.py`, `validate-numerics.py`, `validate-profile.R`.
Only this family’s golden was generated (500 observations, seed 13090).

Shared files (each listed separately):

- `univariateML-cpp/include/cppunivariate/cppunivariate.hpp`: fitter include.
- `univariateML-cpp/include/cppunivariate/special/special.hpp`: no-throw noncentral chi-square CDF seam and Boost include.
- `bindings/r/CANONICAL_NAMES.md`: canonical `mu1`, `mu2` registration.
- `bindings/r/R/code_helpers.R`: runtime-checked extraDistr density repair.
- `codex-report.md`: this report.

None of `dskellam`, `pskellam`, `qskellam`, `rskellam` is imported.
The only extraDistr import is `dlomax`.
No existing goldens or prohibited files changed.

## Mathematics: proved

- Independent Poisson difference has mean `mu1-mu2`, variance `mu1+mu2`;
  these identify both rates, including zero.
- At fixed `s=sqrt(mu1*mu2)`, the theta score is
  `sum(x)-2*n*s*sinh(theta)`, strictly decreasing. Therefore the joint
  interior MLE matches the empirical mean exactly in mathematical arithmetic.
- The second score equation is
  `mean(I'_|x|(2*s)/I_|x|(2*s)) = cosh(theta)`;
  `I'_v(z)=I_(v+1)(z)+v*I_v(z)/z`.
- A convolution mass is bounded by the largest mass of either Poisson
  factor. Stirling bounds this by `1/sqrt(2*pi*floor(rate))` for rate>=1.
  Thus likelihood vanishes as either rate diverges. Continuity on the
  closed quadrant gives an attained finite MLE for every integer sample.
  The same compactness argument applies to any supported fixed subset.
- The zero-rate faces are actual members, rather than D1 escapes. They
  are returned directly, consistent with the attained-boundary convention
  in dev/research/ml-behaviour.tex. Both-sided samples exclude both faces.
- For constant data k>0, condition on the negative count and bound each
  positive Poisson mass by its maximum at the corresponding integer mode;
  those maxima decrease with the mode. The unique joint fit is (k,0).
  Reflect for k<0; all-zero data have fit (0,0).
- At the positive Poisson face with nonnegative data and positive mean,
  the inward profile score per observation is
  `mean(x)*mean(1/(x+1))-1`. The variance-minus-mean rule is only heuristic.
- With one positive rate held, the other coordinate has strictly concave
  log likelihood. For the conditional latent count J,
  `(j+1)*p(j+1)/p(j)=mu1*mu2/(j+abs(k)+1)` decreases, so
  `Var(J)<E(J)`. The rate Hessian is `(Var(Y)-E(Y))/rate^2<0`.
  A bracketed monotone score solve finds this unique fixed-subset maximum.
- Fixed zero-rate fits reduce exactly to signed Poisson MLEs. Unsupported
  samples have zero likelihood and are rejected. Both fixed rates merely
  evaluate the likelihood. There is no unbounded likelihood or escape.

## Mathematics: checked numerically / assumed

- No joint-profile uniqueness theorem is assumed. Candidate maxima are nominated on a
  log-rate grid, refined separately, and compared with the exact face;
  faces win ties within `1e-8*n`. The far-tail exclusion is rigorous, but
  grid resolution is numerical and cannot certify finding every interior
  peak. Rates below exp(-32) are represented by the face comparison.
- Bessel series is centered at its maximal positive term. Above
  hypot(order,argument)=10000, three uniform Debye correction terms from
  [DLMF 10.41(ii)](https://dlmf.nist.gov/10.41.ii) are
  used, with a cancellation-resistant exponent. Truncation accuracy is
  checked against mpmath; no formal uniform remainder bound is asserted.
- CDFs use the noncentral chi-square identity, direct complements, and
  direct outward log summation if the ordinary tail underflows. Quantiles
  bracket and bisect using the smaller log tail. RNG is two R Poisson draws.

## Validation

All builds, tests and long probes ran through `/tmp/umvl-cpu/run`, one thread
per command, under nice 10; no parallel build flags.

- `just cpp-test`: 5/5 CTest targets; 348/348 cases and 517384/517384
  assertions passed. Distribution target: 330 cases, 506484 assertions.
  Skellam contributes 4 cases and 66 assertions, including two independent
  finite-difference scores at the golden fit.
- Final `just test`: FAIL 0, WARN 0, SKIP 1, PASS 6708 (baseline PASS 6541).
  The skip is the unchanged optional sn-package comparison. Seven Skellam
  test blocks cover independent reference optimizations, both fixed-rate
  subsets, both held, zero-rate faces, nonconstant underdispersion, errors,
  log tails down to -1000, recycling/NA, seeded RNG, dml/pml/qml/rml,
  plotting, bootstrap with a held rate, and model selection.
- Earlier full R run, before the last boundary tests and golden: PASS 6698,
  FAIL 0, WARN 0, SKIP 1. Initial C++ run lacked the new golden fixture;
  the final C++ run above includes it and is entirely green.
- `just quality skellam`: 1/1 family passed; no skipped coordinates.
  Symbolic logLik difference -4.09e-12; standardized score 3.08e-7;
  observed information positive definite; 8 oracle starts, 16 feasible
  results, no improvement (gap -3.64e-12).
- `UMVL_GOLDEN_IDS=skellam just goldens`: exactly 1 new golden; existing
  goldens unchanged. Golden rates (4.253274679562562, 3.087274679562562),
  logLik -1205.2790293184653, n=500, seed=13090.
- `validate-numerics.py`: 18/18 independent mpmath log-mass comparisons
  passed at 180 decimal digits, rates 1e-200 through 1e100, orders through
  1e50, including the series/asymptotic switch.
- `validate-profile.R`: all 246 multisets of sizes 2–5 from {-2,-1,0,1,2}
  checked on an independent 241-point reference-density grid. Zero samples
  with multiple interior grid peaks; worst likelihood improvement 0.
  Four additional Poisson-face log masses through rate 1e12 match stats::dpois.
- `git diff --check`: clean.

Reproduce the independent probes from the worktree root with
`/tmp/umvl-cpu/run python3 univariateML-cpp/include/cppunivariate/distributions/skellam/validate-numerics.py`
and `/tmp/umvl-cpu/run Rscript univariateML-cpp/include/cppunivariate/distributions/skellam/validate-profile.R`.
Logs: `/tmp/umvl-codex/skellam/`; quality details: `build/quality/skellam.json`.

## Ready-to-paste KNOWN_ISSUES.md rows

| extraDistr 1.10.0.5 | `dskellam(log=TRUE)` overflows its unscaled Bessel function | `extraDistr::dskellam(0, 1000, 1000, log=TRUE)` gives NaN, should be -4.71932724734257 | own d/p/q/r; runtime-checked density replacement in `univariateML_reference_fixes` |
| extraDistr 1.10.0.5 | `dskellam` does not handle signed Poisson zero-rate members | `extraDistr::dskellam(-1, 0, 1)` gives NaN, should be exp(-1); `extraDistr::dskellam(-1, 0, 0)` gives NaN, should be 0 | own d/p/q/r; same runtime repair |
| extraDistr 1.10.0.5 | `dskellam(log=TRUE)` underflows a finite log mass at tiny rates | `extraDistr::dskellam(1, 1e-200, 1e-200, log=TRUE)` gives -Inf, should be -460.517018598809 | own d/p/q/r; same runtime repair |

## Open questions

- A proof of profile uniqueness or a certified interior root isolation
  method would strengthen the numerical global search.
- Extreme CDF parameters remain subject to Boost's noncentral chi-square
  accuracy and iteration limits. Very large integer quantiles also face
  the representability limit of double-precision integers.
