# TASK-78.5 / fixB

Base: f4d1c1224bee8e71a907b383b31c8d1b2eafe8d4. No commits or board edits.

## Changes

Files: the 13 fixed families' `dpqr.hpp` and `test.cpp`; Frechet `test.cpp`;
`special/tails.hpp`; this report.
No signatures, fitters, adapters, metadata, goldens, or truth data changed.
All seeded sampling arithmetic is preserved. Huber and triangular samplers
call copies of their original inverse transforms, isolated from the repaired
public quantiles; the other sampler bodies are unchanged.
Shared helper `probability_minus_half` in `special/tails.hpp` retains the
rounded-log-half offset using a two-part log(2). Used by Laplace, logistic,
Cauchy and Huber; needed because their zero-location median offsets are
representable but disappear on ordinary subtraction of rounded tail logs.

- gompertz: cancelled `exp(b*x)-1`, complemented rounded CDFs, and lost log quantiles; log cumulative hazard, `expm1`, log complements, and guarded inverse softplus.
- rayleigh: squared observations/scales overflowed or underflowed and tails cancelled; log hazard/density and log-domain square-root quantiles.
- fatigue: normal tails underflowed, upper tails cancelled, negative normal quantiles caused subtraction cancellation; direct normal log tails with double-only Mills expansion, log-tail inversion, and an asinh/log quantile transform.
- laplace: complemented rounded CDFs and exponentiated log inputs; direct smaller log tail, stable standardization, and signed log-tail quantiles.
- invweibull: reciprocal scale intermediates and tail/quantile cancellation; retain the current rate parametrization in log coordinates and invert accurate log tails.
- invexp: reciprocating exponential arguments/quantiles lost range; delegate d/p/q directly to shape-one inverse Weibull, preserving current parametrization.
- weibull: ratios/powers lost range and survival tails cancelled; log-coordinate density/hazard and log-tail quantiles, including endpoint densities.
- exp: upper-tail subtraction and log-input exponentiation lost information; direct log survival, stable tiny lower tails, log-domain inverse and support endpoints.
- cauchy: atan CDF saturation, squared standardized values and tangent quantiles lost tails/range; atan2 smaller tail, log density with log standardization, reciprocal tangent and log asymptotic quantiles; split quarter-quantile correction preserves cancellation with location.
- logis: delegated p/q lost deep log tails and density cancellation could produce NaN; symmetric softplus log tails/density and log-odds quantiles.
- huber: exponential tails underflowed and central normal quantiles rounded to infinity; log exponential tails, log-input handling and inversion of the central log tail.
- triang: complemented rounded tails and underflowed products before square roots; direct log polynomial tails, positive-term opposite-tail formulas for endpoint modes, log density factors and log-domain square-root quantiles.
- unif: density used an underflowed reciprocal, upper tails cancelled, log inputs lost scale rescue; direct log distances/widths, both tail logs and nearest-endpoint quantiles.
- frechet: missing full truth wiring; add uniform-signature truth lambdas for d/p/q/r over the repaired inverse-Weibull delegate.

## Numerical validation

All 14 full truth cases pass with default tolerances and no `may_fail`.
Focused simbox run: **65 cases, 32248 assertions, zero failures**.
Tables cover **71 parameter points, 1414 observation rows, 3124 quantile
rows, 9832 numerical comparisons**, plus **1420000 seeded sampler draws**.
No oracle or truth table changed and no tolerance was loosened.
100-digit mpmath independently confirms rounded log-half offsets and the
Cauchy quarter-quantile residual; no extended-precision C++ is required.
Focused regression comments give mpmath generating formulas at 100 digits.
Coverage includes both tails/scales, log quantiles through -10000,
underflowed intermediates rescued by scale, support endpoints and infinities.
The independent stored tables also test seeded sampler validity and CDFs.
Log d/p tolerance is 1e-10 (relative, absolute near zero); quantile
tolerance is 1e-9 relative or the oracle adjacent-double bracket.
Samplers use 20000 draws/point, seed 20261029+point, and five binomial
standard errors plus 1/n for empirical CDF agreement.

## Suite baseline and final checks

Baseline on simbox at the unchanged base:
- CTest **5/6 targets pass**; `distribution_tests`: **503 cases, 501 pass,
  2 fail, 3 skipped; 955567 assertions, 946618 pass, 8949 fail**. Assertion
  failures include the existing permitted truth failures. Unpermitted cases:
  `betapr n20 independent slow oracle fixtures` and the `std` golden fit.
- R: **[ FAIL 3 | WARN 0 | SKIP 0 | PASS 10547 ]**. Existing failures:
  frozen sstd/fGarch quantile (nu=100, xi=4); bnbinom golden; std golden.
- `just quality` for all **14 assigned families: 14/14 pass** (logLik,
  score/information and multi-start oracle checks).
- Final R: **[ FAIL 3 | WARN 0 | SKIP 0 | PASS 10547 ]**, identical
  failures and counts to baseline; no new regression.
- Final CTest: **5/6 targets pass**. Distribution tests: **527 cases,
  525 pass, 2 fail, 3 skipped; 958180 assertions, 950941 pass, 7239 fail**.
  The same two unpermitted baseline cases fail; all assigned cases pass.
  Remaining assertion failures include permitted other-family truth defects.
- Final focused rerun: **65/65 cases, 32248/32248 assertions pass**;
  465 unrelated cases filtered out. Includes triangular endpoint-mode
  log tails/density and preserved Huber/triangular sampling transforms.
- Source comparison confirms all 14 original sampler transforms are retained.
- `git diff --check` passes; no golden or unassigned-family file changed.

## Execution

Initial local baseline commands waited for occupied CPU slots and were stopped
by the reviewer before executing. Builds and suites moved to simbox via
`/tmp/umvl-simbox/run`, per reviewer instruction. Scratch scripts/logs:
`/tmp/umvl-codex/fixB/`.

Open questions: the unrelated simbox baseline failures above require their
owners/reviewer; this batch does not change those families or goldens.

Final logs: `verified-focused.log`, `verified-cpp.log`, `final-r.log`,
`quality.log`; baseline: `baseline-cpp-captured.log`,
`baseline-r-captured.log`, all in the scratch directory above.
