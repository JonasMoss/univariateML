# smallown: owned logit-normal and Nakagami d/p/q/r

Implemented, uncommitted. No git-writing commands, board edits, installs or agents.

## Files changed

Under `cppunivariate/include/cppunivariate/distributions/`, both `logitnorm/`
and `naka/`: `README.md`, `dpqr.hpp`, `adapter.cpp`, `test.cpp`, `test.R`,
and new `dpqr.R`.

Shared files (complete list):
- `cppunivariate/include/cppunivariate/special/special.hpp`: two one-line
  extended-precision Boost CDF wrappers, using the existing no-throw policy.
- `univariateML/DESCRIPTION`: move logitnorm and nakagami to Suggests.
- `univariateML/R/univariateML-package.R`: remove both density imports.
- `codex-report.md`: this report.

Generated assembly remains in ignored `univariateML/package/`.
No existing golden or prohibited reviewer-maintained file was edited.

## Behavior and compatibility

The installed APIs are `*(x, mu=0, sigma=1)` for logitnorm and
`*(x, shape, scale)` for naka (shape precedes scale). Public density, CDF,
quantile and RNG names are unchanged. Logitnorm also retains `...`.
New explicit tail/log flags follow stats conventions. README bare names select
owned functions throughout fitted-model dispatch. R vector recycling, missing
values, zero lengths and warning policy use the existing shared mechanism.
Finite location and positive finite scales/shapes are required.
R sampling transforms R normal/gamma draws; set.seed controls it.
Nakagami CDF below zero is now zero; density at zero follows its shape limit.

## Mathematics: proved analytically

- For logit-normal, Y=logit(X) is N(mu,sigma^2); the Jacobian is
  1/[x(1-x)]. Monotonicity transfers CDFs, upper tails and quantiles.
- For Nakagami, X^2 is Gamma(shape, scale/shape), with Jacobian 2x.
  Thus negative observations have zero density/CDF. At zero the density
  is infinite for shape<1/2, sqrt(2/(pi*scale)) at 1/2, and zero above 1/2.
- Complementary log tails use log1p/expm1 and direct smaller-tail evaluation.
  Log probabilities are inverted without first exponentiating extreme inputs.
- Normal Mills expansion through 105/t^8 is used for t>100; integration by
  parts bounds the omitted relative term by 945/t^10 (<9.45e-18).
- The tiny gamma quantile leading term applies below log(unit-quantile)=-36;
  its relative correction is bounded at double precision by x/(shape+1).

## Mathematics: checked numerically

Both density integrals; normal/gamma transformation identities; optional
reference d/p/q agreement; ordinary and logarithmic inverse round trips;
extreme log probabilities -1000, -10000 and near-one log probabilities;
support edges and shape-dependent zero density; seeded RNG transformation
identities; fitted logLik equals sum of owned log densities.

## Assumptions / limits

Boost's no-throw CDF/quantile accuracy is inherited. Gamma underflow recovery
uses the incomplete-gamma power series or Lentz continued fraction, with a
10,000-iteration cap; this is not an exhaustive proof over all finite doubles.
Ordinary probability round trips require probabilities distinguishable from
zero/one. Returned quantiles and draws can round to support endpoints when the
mathematical result is outside floating-point resolution.

## Validation

All builds/tests/computations use `/tmp/umvl-cpu/run` (one thread, nice 10).
Final `just cpp-test`: 5/5 CTest targets passed (the supplied baseline said four;
the actual checkout also has cmp_tests). Includes 26 added scalar assertions.
Full `just test`: 4,921 passes, 0 failures, 0 warnings, 0 skips.
Targeted R after the final tiny-shape log-tail correction: 102 passes,
0 failures, 0 warnings, 0 skips (both distribution test files).
`just quality logitnorm naka`: 2/2 passed.
`git diff --check`: clean.
Initial R run: 4906 passes, 3 failures, 0 warnings, 0 skips. Its failures were
new test tolerances: default integration accuracy and saturated ordinary CDF
round trips; both tests were corrected before the final run.
Goldens remain byte-for-byte unchanged; existing fixed-data regression tests
validate them rather than regenerating data from the newly owned samplers.
Logs: `/tmp/umvl-codex/smallown/`.

## Open questions

None requiring an author decision.
