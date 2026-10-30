# owndpqr implementation report

No commits or Git writes. Protected files and the bnbinom/CMP jobs were untouched.

## Files changed

Paths below are relative to the worktree; distribution paths share
`univariateML-cpp/include/cppunivariate/distributions/`.

- `cmb/`: new `README.md`, `mle.hpp`, `dpqr.hpp`, `adapter.cpp`, `dpqr.R`,
  `test.cpp`, `test.R`, `golden.json`.
- `zipf/`: `README.md`, `dpqr.hpp`, `adapter.cpp`, `dpqr.R`, `test.cpp`, `test.R`.
- Distribution `README_SCHEMA.md`; `univariateML-cpp/include/cppunivariate/cppunivariate.hpp`.
- `bindings/r/scripts/assemble_package.R`: generic per-folder `dpqr.R` assembly.
- `bindings/r/src/dpqr.h`, `bindings/r/R/dpqr_helpers.R`: shared R-shell
  recycling, missing values, warnings, and sample-size handling.
- `bindings/r/R/code_helpers.R`: removed sads repairs and the shape-zero helper.
- `bindings/r/R/code_model_select.R`: passes `...` to fitters, including required size.
- `bindings/r/R/univariateML-package.R`, `bindings/r/DESCRIPTION`,
  `bindings/r/CANONICAL_NAMES.md`: own functions and CMB registration/docs.
- `bindings/r/tests/testthat/test_input_checks.R`, `test-audit-input.R`:
  CMB required-design arguments; this report.

## Implementation

Eight exported R d/p/q/r wrappers use the existing bare-name resolver. Both
families recycle numeric arguments, preserve NA/NaN, floor CDF arguments,
warn for invalid parameters and fractional density arguments, and use R's RNG.
Quantiles bisect using the caller's tail and probability scale, including log
probabilities below the exponential underflow threshold and CDF atoms.
Quantile endpoints follow R conventions, including degenerate binomial members.

Zipf prefixes reuse the fitter's harmonic moments; independent scaled ranges
supply upper tails without subtracting two harmonic sums. The uniform member
uses direct ratios. CDF/quantile/random evaluation uses bounded memory through
integer.max; the optional legacy C++ CDF-table helper still explicitly allocates
its requested table. The fitter and every existing golden are unchanged.
sads is removed from Imports; it remains an optional Suggests comparison only.

CMB enumerates finite log weights, normalizes around their maximum, and caches
both log tails once per distinct parameter tuple in each vectorized R call.
Mean matching uses safeguarded Newton with Var(X); dispersion uses a bracketed
scalar solve. Returned-coordinate moments are checked, and logLik is evaluated
at the probability actually returned. Required size and held parameters are
recorded in `fixed`; unidentified size-one dispersion is warned about and held.

## Mathematics: proved by derivation

- With h(k)=log choose(N,k), the joint Hessian is -n Cov(X,h(X)). For N>=2,
  h is strictly concave on the integer support and is not affine, so this
  covariance is positive definite at finite canonical parameters.
- The upper convex-support edges join adjacent integers; its lower edge joins
  0 and N. Interior empirical statistics give a unique finite joint MLE;
  exposed edges/vertices give the declared adjacent, endpoint, or point limits.
- Along mean matching, d E[h]/d nu = Var(h)-Cov(X,h)^2/Var(X)>0.
  Fixed nu has a finite fit iff 0<mean(x)<N. Fixed prob has one iff
  0<mean(h)<max(h); its upper face is central, its lower face endpoint-only.
  Constants away from those fixed-prob faces can therefore have finite fits.
- At size one h is identically zero, so nu is unidentified. At nu=1 the
  normalizing binomial theorem gives the ordinary binomial distribution.
- Zipf s=0 is uniform; upper-tail summation and integer generalized inversion
  give the stated CDF/quantile identities in exact arithmetic.

## Mathematics: checked numerically

Small exhaustive Zipf sums, fractional CDF arguments, tails/logs, direct sums
at 10000, integral-test brackets at N=1e9, integer.max uniform inversion,
inverse inequalities and CDF atoms, seeded RNG and chi-square sanity checks.
CMB finite sums and binomial d/p/q comparisons, negative dispersion, an
independent optim likelihood oracle, all reachable faces and fixed subsets,
df/fixed metadata, and dml/pml/qml/rml/plot/bootstrap/model_select integration.
For the CMB golden, independent symbolic logLik differs by 3.87e-12; the
standardized score is 4.51e-9 and the eight-start oracle gap is below 3e-12.

## Mathematics: assumed / practical limits

Standard floating-point and special-function accuracy. Euler--Maclaurin and
roundoff errors are tested, not globally certified over every floating-point
input. CMB has a documented size ceiling of 1e7 for allocation safety; fitting
uses strictly interior prob, while d/p/q/r also admit prob=0 and prob=1.
A fitted canonical probability that rounds to an endpoint reports iteration
failure rather than returning inconsistent coefficients and logLik.

## Validation

- `/tmp/umvl-cpu/run just cpp-test`: **4/4 targets**; **291 test cases,
  505192 assertions**, all passing (distribution target: 281 cases / 494911 assertions).
- `/tmp/umvl-cpu/run just test`: **4737 passes**, **0 failures, 0 warnings,
  0 skips**. The last change afterward corrected a roxygen description only.
- `/tmp/umvl-cpu/run just quality`: **70/70 distributions pass**.
- `/tmp/umvl-cpu/run just quality cmb`: **1/1 passes**.
- `UMVL_GOLDEN_IDS=cmb /tmp/umvl-cpu/run just goldens`: exactly one new golden;
  no existing golden changed.
- Final post-generation roxygen + `tools::checkRd` + `tools::codoc` sanity:
  **9/9 new exports documented, 3/3 Rd files valid; zero warnings or link diagnostics**.
  Initial assembly still emits pre-existing stale-source-NAMESPACE diagnostics;
  these are absent from the final documentation pass.
- All builds/tests/long computations used `/tmp/umvl-cpu/run` (one thread, nice 10).
- Logs: `/tmp/umvl-codex/owndpqr/{cpp-final2,test,quality-all,quality-cmb,docs-final}.log`.

## Open questions

None requiring an author decision. Practical numerical limits are stated above.

Artifact display: the viewer rejected this worktree with HTTP 403 (outside its
readable roots); the report remains available at the worktree path.
