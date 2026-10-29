# TASK-82 P3 restrictions: p3-restrict

No commits, Git writes, board edits, sub-agents or dependency installs.

## Files

Under `cppunivariate/include/cppunivariate/distributions/`, changed
`README.md`, `mle.hpp`, `adapter.cpp`, `test.cpp` for each of
`lst frechet ht gev gpd tlambda ged sged`, and `test.R` for each except ht.
Also changed only construction/warning helpers in
`univariateML/R/code_helpers.R`, added
`univariateML/tests/testthat/test_restricted.R`, and this report.
No support lines, density implementations, truth fixtures, goldens or
shared fitter infrastructure changed.

## Implementation and evidence

- All eight result structs have default-zero `unsigned restricted`.
  Public fitter entrypoints set bits in README coefficient order, lower
  at 2i and upper at 2i+1, only for estimated, nonexempt coefficients.
  Adapters transport named character vectors. Header constants are checked
  against generated README fitting domains by an R regression.
- The constructor emits `univariateML_restricted` with coefficient, bound,
  side and the exact README reason. Attribute transport is retained;
  empty restrictions and held bounds do not warn. Existing family oracle
  tests suppress only this expected warning class where appropriate.
- LST: df=1 face is flagged. Held mu exempts the lower bound and permits
  df>0; free scale with observations at held mu diagnoses the resulting
  unrestricted collapse. Proper held df below one is accepted when its
  conditional MLE exists. Failed default BFGS results require the analytic
  score norm below 1e-3 in standardized fitting coordinates.
- Frechet: lambda=1 is flagged with free mu, including held sigma;
  holding mu retains the inverse-Weibull exemption and allows lambda<1.
  Existing exact scale/concave shape reductions and failure statuses remain.
- HT: result and adapter infrastructure is ready. Its current fixed-zero
  threshold is the declared exemption, so no current fit is flagged and
  nu<1 remains available. Translation belongs to P4.
- GEV: free xi is restricted to [-1,1]. At xi=-1 the closed reversed
  exponential has endpoint M=max(x). Free scale/location give
  sigma=mean(M-x), mu=M-sigma; held scale gives mu=M-sigma;
  held mu gives sigma=max(M-mu,mu-mean(x)). These are direct likelihood
  maximizations, independent of the reference's zero endpoint density.
  Both shape bounds are flagged only when xi is estimated. Proper held
  shapes outside free restrictions are accepted where the hold permits
  a finite fit. Existing collapse diagnostics and scalar failure statuses
  remain conservative.
- GPD: xi>=-1 replaces -1/2. The xi=-1 uniform likelihood is evaluated
  directly on the closed interval; free scale is exactly max(x)-mu.
  The shape profile compares that attained face with interior roots.
  The held-scale path also compares the closed face. Threshold observations
  are accepted; free-scale collapse is diagnosed using (n-k)/xi-k,
  and any threshold tie permits collapse when shape is also free.
  Fully held proper shapes below -1 can be evaluated at interior data.
- Tukey lambda: estimated lower/upper faces are flagged; held mu exempts
  the lower bound. Conditional searches extend below -1 and diagnose
  threshold-tie collapse when scale is free. Finite held shapes may lie
  outside either estimated bound; fixed negative-shape collapse uses its
  actual exponent. Held uniform aliases are not flagged.
- GED/SGED: estimated nu=1 faces are flagged, with no sd exemption.
  Failed BFGS results require total score norm <1e-3 in the fitter's
  standardized, transformed coordinates. Adapters stop on remaining
  BfgsFailure statuses on every supported path. GED accepts proper held
  nu<1: concavity between observations reduces location minimization to
  observation candidates. Existing unresolved SGED hold subsets still stop
  with their declared P2 reason.
- README domains distinguish proper finite laws from fitting restrictions.
  GEV/GPD prose explicitly identifies [-1,-1/2) fits as non-regular.
  Endpoint, held-bound, exemption, mask and failure regressions were added.
  The dense/exhaustive GEV/GPD oracle fixtures now cover the agreed range;
  non-regular score checks use smaller finite-difference steps.

The closed-face formulas and collapse exponents are analytic reductions.
Multistart, grid and dense-profile agreement is numerical evidence, not
certified global mode isolation. Existing SGED existence/exhaustion gaps
from P0 remain open.

## Goldens

No golden file changed. Default numerical paths remain except the required
GEV/GPD domain expansion, endpoint attainment, and failed-optimizer policy.
Held-location exemptions and proper held-shape acceptance widen conditional
fits; these do not regenerate the frozen default data or estimates.

## Final validation

Final simbox `just cpp-test`: six CTest targets; the five auxiliary targets
pass. Distribution suite: 644 cases, 642 pass, two allowed fixture failures,
three skipped; 957,851 assertions, 948,902 pass, 8,949 reported failed.
Of those assertion failures, 8,947 are TASK-78 `may_fail` truth assertions;
only betapr/test.cpp:33 and std/test.cpp:40 fail ordinary cases.
The other targets add 22 passing cases and 11,047 passing assertions:
combined 666 executed cases (664 pass, two known failures), plus three skipped,
968,898 assertions (959,949 pass, 8,949 reported failed).
Final `just quality lst frechet ht gev gpd tlambda ged sged`: 8/8 pass.
Final simbox `just test`: 22,406 passing checks, exactly three allowed
failures (frozen sstd quantile, bnbinom golden, std golden), zero skips.
Three expected `univariateML_restricted` warnings come exclusively from
untouched test_scale_equivariance.R. No unexpected failure remains.
The new restriction file passes 99 checks; conformance passes all 2,601.
All 98 README schemas validate, and `git diff --check` passes.
Added 12 C++ cases and five R test blocks. Logs are in
`/tmp/umvl-codex/p3-restrict/`; final logs are `cpp-final2.log`,
`r-final2.log`, `quality-final2.log`, and `cpp-counts.log`.
All commands finished; no task command remains running.
The focused local C++ run passed 60 cases / 1,743 assertions (8 truth cases
excluded); the subsequent GPD threshold regression is in the full rerun.

## Outside-file integration and open questions

- The parallel closure job must supply `dml`/`univariateML_to_function`
  endpoint agreement for GEV/GPD. This branch's likelihood tests use
  independent closed-face formulas because literal extraDistr densities
  return zero at those endpoints. No shared closure function was edited.
- `univariateML/tests/testthat/test_scale_equivariance.R:73` should expect
  or suppress class `univariateML_restricted`: its three uniform-face
  Tukey-lambda calls now correctly warn. That shared test was not edited.
- Certified global profile/root isolation and the unresolved SGED held
  subsets remain open mathematics, as in P0/P2. No new unsupported hold
  was introduced.

Artifact-pane display was attempted; the viewer returned HTTP 403 because
this worktree is outside its configured readable roots. Viewer settings
were not changed; the report is available at the worktree path.
