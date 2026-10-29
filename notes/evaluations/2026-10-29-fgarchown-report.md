# fgarchown review report

Implementation and validation complete. All long commands used the shared CPU lock.
No commits, dependency installations, board edits, fitter changes, or golden changes.

## Files

For each of `std`, `sstd`, `ged`, `sged`, `snorm` under
`cppunivariate/include/cppunivariate/distributions/`:
- `dpqr.hpp`: scalar densities, direct tails, log probabilities and quantiles;
  retain portable RNG templates with parameter validation.
- `dpqr.R` (new): documented exported R functions with fGarch argument names.
- `adapter.cpp`: vector recycling/NA/warning adapters and R RNG bridge.
- `README.md`: bare package-defined density/sampler registrations.
- `test.R`, `test.cpp`: probability and integration checks, optional fGarch checks.

`snorm/fs.hpp` (new): common two-piece probability algebra and stable moments.

Shared files changed:
- `cppunivariate/include/cppunivariate/special/special.hpp`: new no-throw Boost
  log-tail and log-quantile primitives;
  existing backend entry points retain their implementations.
- `univariateML/DESCRIPTION`: fGarch moves from Imports to Suggests.
- `univariateML/R/univariateML-package.R`: remove fGarch import directive.
- `univariateML/tests/testthat/test_input_checks.R`: use owned RNGs.
- `univariateML/tests/testthat/test_golden_regression.R`: replay stored datasets
  for historical fGarch goldens, without requiring fGarch or changing fixtures.
- `codex-report.md`: this report.

## Mathematics

Proved algebraically:
- With symmetric unit-variance base density f and m=E|Z|, the unstandardised
  two-piece density is g*f(y/xi) on y>=0 and g*f(y*xi) on y<0,
  g=2/(xi+1/xi). Its mean is m*(xi-1/xi); variance is
  (1-m^2)*(xi^2+xi^-2)+2*m^2-1. Affine standardisation gives mean/sd.
- On either side the outward tail is g*side_scale*F(-|y|/side_scale).
  Dividing the appropriate side probability by g*side_scale inverts through
  the symmetric quantile. Complements use log1p/expm1, retaining small tails.
- For GED, a=exp((lgamma(1/nu)-lgamma(3/nu))/2) and
  (|Z|/a)^nu is Gamma(1/nu,1). This gives its tails and gamma quantiles.
- Student t rescales by sqrt((nu-2)/nu). Its density uses the existing stable
  log gamma ratio rather than taking the logarithm of an underflowed density.

Checked numerically:
- Density integrals agree with both CDF tails and integrate to one for all five.
- Inverse/tail round trips include log probabilities -1000 and -20000, and
  near-one log probabilities -1e-30; log densities remain finite in far tails.
- fGarch central-grid agreement: 105 vector comparisons / 525 scalar values,
  maximum relative errors std 2.89e-15, sstd 5.92e-13, ged 2.00e-15,
  sged 5.77e-15, snorm 9.33e-15. All below 1e-12.
- The specified dstd(1e6, nu=1e6, log=TRUE) defect is fixed, checked against dt.
- Seeded owned RNGs, missing values, invalid parameters, fitted log likelihoods
  and dml/pml/qml/rml dispatch are covered by the R tests.

Assumptions / numerical limits:
- Boost primitives and R RNG primitives obey their documented distributions.
- Extreme log-tail fallbacks use the Mills/gamma asymptotic expansions or
  the incomplete-beta continued fraction, then monotone scalar inversion.
  These numerical fallbacks are checked, not claimed as formal error bounds.
- Quantiles outside finite double range return infinity; arbitrary combinations
  of finite parameters near floating-point limits are not exhaustively certified.

## Validation

- `just quality std sstd ged sged snorm`: 5/5 pass.
- Final `just test`: 5325 expectations pass, 0 failures, 0 warnings, 0 skips.
  All 71 golden files are unchanged; five historical fGarch golden datasets
  replay from their recorded observations. Development checks fixed the gamma
  CDF infinity endpoint and random successful-fit fixtures; fGarch comparisons
  use central probability positions to avoid its inaccurate compressed tails.
- `just cpp-test`: 5/5 ctest targets pass; 312 doctest cases, 506174
  assertions, 0 failed/skipped. Includes 5 new cases with 40 log-tail checks.
  This checkout already includes the fifth `cmp_tests` target.
- Consumer probes: 5/5 models pass model_select, 15 plot calls (d/p/q),
  and 10 bootstrap refits, with fGarch absent from loaded namespaces.
- Final logs: `/tmp/umvl-codex/fgarchown/{cpp.log,r-pass.log,quality.log,probe2.log}`.
- `git diff --check` passes. No prohibited files changed.
- Artifact-pane display was attempted; its server returned 403 because this
  worktree is outside the viewer's configured readable roots.

## Open questions

None requiring an API decision. Historical golden sampler strings remain
unchanged as provenance; their recorded observations are used for regression.
The owned generators are seeded/reproducible and follow the same laws, but
vector RNG ordering need not reproduce fGarch's exact seeded sequences.
