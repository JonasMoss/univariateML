# TASK-77: scale and location equivariance

Worktree `codex/scaleq`; no commits or board edits.

## Diagnosis and implementation

- **std**: on DAX returns the raw historical BFGS start has gradient norm
  about 2610 (location component -2608). With H = I, neither of the two
  Armijo searches accepts any of its 20 steps. The result is
  `LineSearchFailure`, iteration 1, 41 objective evaluations, unchanged
  nu = 6 and logLik = 5976.05. **No convergence rule fires.** The adapter
  deliberately accepts finite BFGS failure output. The old sd < 1e-3
  normalization guard misses DAX (sd about 0.01). Always center and divide
  by the stable sample sd before all interior/face comparisons; restore
  location, scale and the likelihood Jacobian afterward.
- **sstd**: the same raw location coordinate affects its interior starts;
  comparing unsuccessful interior fits with a standardized df-two face can
  falsely select the face. Always normalize the complete problem, as for
  std. Restore half-face locations from the exact original sample extreme,
  avoiding a round-trip error in support endpoints.
- **gompertz**: the fixed start b = 0.1, exponential evaluation at b*x and
  the absolute floor in its Newton stopping test depended on data units.
  The old normalization guard misses the reported 1000*x sample. Always
  divide by the sample mean, then divide both a and b by that mean.
- **power**: epsilon was an absolute endpoint increment. It now specifies
  alpha = max(x)*(1+epsilon); the nextafter fallback remains for rounding.
- **fatigue**: the absolute Newton beta step and a scale-dependent switch
  of convergence rules changed stopping decisions. Normalize by max(x)
  and use relative beta-step convergence consistently. `abstol` retains
  its R argument name but now means a relative beta-step tolerance.
- **paralogis / invparalogis**: an uncentered log-scale makes FD increments,
  BFGS step tolerance and relative objective tolerance depend on units.
  Divide by the sample median for the free fit, restore scale/Jacobian.
  The inverse fitter inherits the correction. Held-scale shape profiling
  already uses dimensionless ratios.
- **betapr**: its analytic gradient avoids the raw-location failure, but
  the free log-scale and objective constant still affected BFGS stopping.
  Divide by the geometric mean, including supplied scale starts; restore
  scale/Jacobian. Held-scale beta transformation is already invariant.
  Constant samples are rejected before roundoff in log-variance calculation.
- **lomax**: its global search is dimensionless, but retained a Newton
  candidate fitted in raw units on ordinary-scale samples. Fit that
  candidate in mean-normalized units consistently, including lambda starts.

## Other numerical paths inspected

- **ged, sged**: already center/scale before FD-BFGS and face comparisons.
- **snorm**: exact profile after range normalization; no raw-coordinate BFGS.
- **lst**: IQR normalization (free df); range normalization and standardized
  score gate (held df).
- **burr, invburr**: centered log-data profile; dimensional scale restored
  after optimization. Fixed-scale variants depend on x/scale.
- **gb2, trgamma, invtrgamma**: standardized log-data, profiled shape/scale;
  inverse Jacobian is applied after the log-data fit.
- **genbeta**: dimensionless log(scale/x); held endpoint must transform too.
- **sn, emg, huber**: normalize observations and all held dimensional
  parameters before their profile/Newton/simplex searches.
- **foldnorm**: normalizes by a sample/held scale before its profile solve.
- Other continuous fitters use closed forms or scalar dimensionless shape
  equations, or already normalize their location/scale problems. Discrete
  BFGS paths (pig, zinb) are outside the requested continuous-family scope.

## Scope and evidence

For an affine family, writing z=(x-c)/s gives
`ell_x(mu,sigma,shape) = ell_z((mu-c)/s,sigma/s,shape)-n*log(s)`.
Positive scale families use c=0, with inverse scale/rate or variance
parameters transformed according to their density. These identities show
why normalized optimization, stopping and face comparisons are invariant
in exact arithmetic. They do not certify global maxima or eliminate
roundoff, shallow likelihood profiles, or representability limits.

The R inventory has 63 continuous models: 53 tested without required
endpoints; 10 explicit exclusions, each with a reason. Beta, Kumaraswamy
and logit-normal have fixed (0,1) support; lgamma has fixed (1,Inf) support.
The six required-endpoint models are additionally tested with their
endpoints transformed. Infinite default tnorm bounds are invariant.
Fatigue and GPD have held zero locations and are tested for scaling only;
folded normal likewise has no translation closure.

The seeded regression uses n=80 (n=24 for the expensive EMG, Bhattacharjee
and Tukey-lambda searches), scales 1e-4 and 1e4 and affine shifts for the
location families, checking coefficients, absolute likelihood differences
per datum and identical error/escape messages. Search tolerances reflect
1.22e-4 default FD scores, numerical profile ties and floating-point
objective resolution; scalar-fit tolerances are tighter. Separate DAX,
small skew-t and large Gompertz regression cases cover the reported bugs.
C++ std/sstd cases compare small/large scaled and shifted golden samples,
including statuses and likelihood Jacobians.

Extended audit: seeded n=400 defaults and all DAX returns at 1e-6 and 1e6;
positive-only models receive abs(returns)+1e-8. Required-endpoint models
are also checked with transformed endpoints. Audit scripts/logs are under
`/tmp/umvl-codex/scaleq/`.

## Validation and golden changes

- `just quality`: 98/98 passed; the first four updated goldens were independently
  rechecked with `just quality std fatigue power betapr`: 4/4 passed;
  invparalogis was rechecked separately: 1/1 passed.
- New R file alone: 56 test cases, 358 assertions, zero failures/errors,
  about 3.9 seconds. After `just test` built the package, the complete
  `testthat::test_local` rerun after the last fixture update passed:
  `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 8760 ]` (the existing missing-{sn} skip).
- `just cpp-test`: 5/5 ctest targets, 401 doctest cases and 522682
  assertions passed (distribution target: 383 cases, 511782 assertions).
  After its golden update, invparalogis additionally passed 2 cases/14 assertions.
- Golden regression alone: 98 cases, 196 assertions, no failures/errors.
- All 53 eligible models additionally checked at 1e-6 and 1e6, n=80:
  104 successful coefficient/likelihood comparisons and two unchanged
  escape decisions. Largest |delta logLik| 5.8e-12; largest coefficient
  error divided by (1+|estimate|) 1.15e-6.
- The 18-path default/DAX audit has 66 successful comparisons and six
  unchanged escape decisions; maximum coefficient error/(1+|estimate|)
  6.32e-6 and |delta logLik| 2.05e-7 (both GED on DAX). The six
  required-endpoint models also retained decisions/likelihoods when their
  endpoints were scaled (maximum |delta logLik| 3.54e-10).
- DAX std: nu=4.19447225165, logLik=5983.321865935704.

Five golden estimate/logLik records changed; their data and provenance
fields are unchanged. No regression tolerances were relaxed:

| id | largest absolute coefficient change | logLik change | justification |
|---|---:|---:|---|
| std | 8.69e-6 (nu) | -1.28e-10 | normalized FD trajectory changes approximate stopping point |
| fatigue | 6.17e-9 (beta) | 2.05e-12 | relative Newton step replaces absolute step |
| power | 1.25e-11 (beta) | 3.00e-9 | epsilon now defines a relative endpoint offset |
| betapr | 2.22e-10 (shape2) | 1.71e-13 | dimensionless BFGS trajectory changes approximate stopping point |
| invparalogis | 3.57e-7 (scale) | -1.78e-11 | inherits normalized paralogis FD trajectory; its existing golden tolerance is 1e-12 |

These exceed their original coefficient tolerances (std 1e-6, other
four 1e-12); retaining them would preserve unit-dependent stopping or the
old endpoint convention. Other golden records are untouched: sstd and
paralogis remain within 1e-6, all other ids within 1e-12 regression checks.

## Files changed

- `distributions/{std,sstd,gompertz,power,fatigue,paralogis,betapr,lomax}/mle.hpp`.
- Corresponding README controls, also invparalogis; power's endpoint formula.
- `distributions/{std,sstd}/test.cpp`, `distributions/power/test.R`.
- `bindings/r/tests/testthat/test_scale_equivariance.R` and this report.
- `distributions/{std,fatigue,power,betapr,invparalogis}/golden.json` (table above).

## Limitations / open questions

No claim of exact equivariance after information-destroying floating-point
maps, or for arbitrary overflow/underflow scales. Required endpoints and
held dimensional coefficients must accompany the data transformation.
No change to BFGS acceptance of finite iteration/line-search failure output.
