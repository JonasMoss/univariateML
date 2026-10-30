# discrete1 implementation report

No commits or Git-writing commands were run. No existing golden was changed.

## Files changed

New distribution folders (under `univariateML-cpp/include/cppunivariate/distributions/`):
- `tpois/`: README.md, mle.hpp, profile.hpp, dpqr.hpp, dpqr_core.hpp, adapter.cpp, spec.py, test.cpp, test.R, golden.json.
- `tbinom/`: README.md, mle.hpp, dpqr.hpp, adapter.cpp, spec.py, test.cpp, test.R, golden.json.
- `dlaplace/`: README.md, mle.hpp, dpqr.hpp, adapter.cpp, test.cpp, test.R, golden.json.

Shared files, individually:
- `univariateML-cpp/include/cppunivariate/cppunivariate.hpp`: three fitter includes.
- `bindings/r/CANONICAL_NAMES.md`: three registration rows.
- `bindings/r/R/code_helpers.R`: runtime-checked truncated d/p/q/r repairs; discrete-Laplace log CDF repair and missing-quantile registration; two-sided integer inversion and repair-aware CDF resolution.
- `bindings/r/tests/testthat/test_input_checks.R`: required-design switches; held Laplace location avoids an intentional median-tie warning in the generic successful-fit test.
- `bindings/r/tests/testthat/test-audit-input.R`: required-design switches.
- `codex-report.md`: this report.

Generated R package files and build outputs are gitignored.

## Interface decisions

`mltpois(x, a = ..., b = ..., lambda = NULL)` requires both known truncation bounds.
`mltbinom(x, a = ..., b = ..., size = ..., prob = NULL)` also requires known size. Bounds follow
extraDistr's strict/inclusive convention `a < x <= b`, including fractional bounds.
Explicit design arguments avoid accidentally fitting the untruncated sampling law.
Estimating size would require a different discrete profile and Poisson-face analysis.
Bounds and size are held coefficients and have no likelihood degrees of freedom.
Existing `mlztpois` is unchanged: actuar extends zero rate to a point mass at one;
extraDistr does not, so `mltpois(a=0,b=Inf)` agrees in the interior and reports
an escape for all-one samples.

`mldlaplace(x, location = NULL, scale = NULL)` fits integer locations only.
Scale is extraDistr's geometric probability, not a continuous-Laplace scale.
The smallest integer median is chosen, warning exactly when multiple integer
medians exist. Both coefficients can be held, including simultaneously.

## Mathematics: proved

- Truncated Poisson and known-size binomial are one-parameter exponential families.
  With natural parameter log(lambda) or logit(prob), the mean derivative is the
  conditional variance. On nonsingleton support this is strictly positive.
  Thus a sample mean strictly between the support edges has a unique global MLE;
  bisection of the conditional mean solves the full likelihood, with no competing
  local maxima or unbounded likelihood. Singleton support does not identify it.
- Edge-only data select point masses. A zero rate/probability is attained if the
  conditioning event includes zero; probability one is attained if it includes
  binomial size. Conditioning on events with zero parent probability instead
  yields unattained point-mass limits, reported explicitly (no invented ml model).
  The finite upper Poisson edge is an unattained lambda-to-infinity limit.
- With a held interior parameter the conditional likelihood is evaluated directly,
  including singleton supports. Held endpoint parameters require compatible data
  and a conditioning event with positive probability.
- Discrete Laplace log likelihood is n[log(1-p)-log(1+p)]+S log(p).
  Integer medians minimize S; for S>0, d=S/n gives d(1-p^2)=2p and the unique
  maximum p=d/(sqrt(1+d^2)+1). This stable form avoids cancellation for small d.
  S=0 escapes to a point mass when scale is free. A different held location gives
  S>0 even for constant samples. Holding scale leaves the median problem only.
- Mode-centred probability recurrences normalize the truncated laws without CDF
  subtraction. Both tail ratios decrease away from the mode; the omitted tail
  is bounded by a geometric series. The stopping threshold is 1e-17 times mass.

## Mathematics: checked numerically

- Twenty independent likelihood searches per family use extraDistr density oracles;
  the Laplace oracle enumerates all integer locations in the sample range.
  Laplace also compares fixed-location profiles at three locations per sample.
- Tests cover fixed subsets, attained boundaries, both truncation escapes,
  singleton nonidentification, support errors, seeded RNGs, tail inversion,
  extreme-tail reference repairs, and Poisson/zero-truncated-Poisson overlap.
- C++ density sums, upper log-tail quantile inequalities, and three golden fits.
- Full R testthat (before the final numerical/edge refinements): `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 6817 ]` (the existing missing-sn skip).
- Final C++ ctest: 5/5 targets passed, including 335 distribution cases (9 new family cases), with explicit work-budget and nonfinite-normalizer failure-path checks.
- `just quality tpois tbinom dlaplace`: 3/3 passed (log likelihood, score/information, eight-start oracle per family).
- Three new goldens generated with `UMVL_GOLDEN_IDS="tpois tbinom dlaplace" just goldens`; existing goldens unchanged.
- Final R-helper probes: five translated CDF probabilities matched the centred reference, eight empty-vector assertions passed, and three zero-rate quantile probabilities returned zero. These changes do not alter interior fitting.

## Assumptions / numerical limits

Floating-point special functions and the final bisection are numerical, not interval
certified. Recurrences have a two-million-term budget per tail and return an explicit
numerical failure if exceeded or if the log normalizer is nonfinite. Very large counts can lose precision through log-gamma
subtraction; no claim of uniform relative accuracy over the entire double range.
The quality harness holds integer location fixed; its Laplace scale score/information
check therefore does not independently establish the median rule.

## Ready-to-paste KNOWN_ISSUES.md rows

| Package | Defect | Reproducer | Effect here |
|---|---|---|---|
| extraDistr 1.10.0.5 | Truncated Poisson d/p/q/r subtract rounded parent CDFs; density is infinite, CDF is NaN, quantiles and samples can be infinite | `extraDistr::dtpois(101,2,100,110,log=TRUE)` is Inf, should be about -0.01979875; `extraDistr::ptpois(109,2,100,110,FALSE,TRUE)` is NaN; `extraDistr::qtpois(.5,2,100,110)` is Inf | runtime-checked d/p/q/r repairs using stable parent log tails and integer inversion |
| extraDistr 1.10.0.5 | Truncated binomial d/p/q/r fail under severe truncation; quantiles and RNG collapse to the upper bound | `extraDistr::dtbinom(91,100,.1,90,100,log=TRUE)` is Inf, should be about -0.01091454; `extraDistr::qtbinom(.5,100,.1,90,100)` is 100, should be 91 | runtime-checked d/p/q/r repairs using stable parent log tails and integer inversion |
| extraDistr 1.10.0.5 | Truncated quantile at probability zero returns excluded lower bound | `extraDistr::qtpois(0,2,100,110)` is 100, should be 101; `extraDistr::qtbinom(0,100,.1,90,100)` is 90, should be 91 | repaired integer-support endpoints |
| extraDistr 1.10.0.5 | Discrete-Laplace upper log tail is lost by complementing rounded CDF | `extraDistr::pdlaplace(1000,0,.5,FALSE,TRUE)` is -Inf, should be `1001*log(.5)-log1p(.5)` | runtime-checked log-tail repair |
| extraDistr 1.10.0.5 | Discrete-Laplace CDF chooses its branch using q instead of q-location and can be negative | `extraDistr::pdlaplace(3,5,.5)` is -1/3, should be 1/6 | runtime-checked CDF repair also corrects translation |
| extraDistr 1.10.0.5 | Discrete-Laplace density and RNG disagree on lattice membership for noninteger location | `extraDistr::ddlaplace(0,.5,.4)` is positive, while `extraDistr::rdlaplace(10,.5,.4)` lies on .5+Z | unreachable here: fitter explicitly restricts location to integers |
| extraDistr 1.10.0.5 | No discrete-Laplace quantile | `"qdlaplace" %in% getNamespaceExports("extraDistr")` is FALSE | registered two-sided integer inversion of the repaired reference CDF |

Reference caveat: a noninteger location shifts `rdlaplace`'s lattice, but its density
checks the observation's integrality instead of lattice membership. For example,
`extraDistr::ddlaplace(0,.5,.4)` is positive, whereas RNG output has fractional part
.5. This fitter rejects noninteger held locations, so that inconsistency is unreachable.

## Open questions

None requiring a decision for these interfaces. Reviewers may choose a different
nonidentification policy for singleton truncation support (currently an explicit error).
