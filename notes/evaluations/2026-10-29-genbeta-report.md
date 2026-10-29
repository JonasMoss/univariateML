# genbeta implementation report

No commits or git mutations. No existing golden changed. Long computations,
builds and tests used `/tmp/umvl-cpu/run` (one thread, nice 10).

## Files changed
- New `cppunivariate/include/cppunivariate/distributions/genbeta/`:
  README.md, mle.hpp, dpqr.hpp, adapter.cpp, test.cpp, test.R, golden.json,
  escape-probe.R.
- Shared `cppunivariate/include/cppunivariate/cppunivariate.hpp`: one include.
- Shared `univariateML/CANONICAL_NAMES.md`: one model row.
- Shared `univariateML/R/code_helpers.R`: four checked actuar repairs.
- Shared `univariateML/tests/testthat/test-audit-input.R`: required scale argument.
- Shared `univariateML/tests/testthat/test_input_checks.R`: held scale and tau
  for the successful-fit contract (avoids small-sample escapes).
- `codex-report.md`: this report.

## Mathematics — proved algebraically
- With theta held, constant data are unbounded in the unrestricted shapes:
  take tau=1, u=x/theta, alpha=s*u, beta=s*(1-u). Density grows as sqrt(s).
- For free theta, fix alpha,tau>0 and 0<beta<1. At k tied sample maxima,
  theta=max(x)+epsilon gives L ~ C*epsilon^(k*(beta-1)) -> infinity.
  Missing scale explains this and names `mlgenbeta(x, scale = 1)`.
- Y=-log(X/theta) has density tau*exp(-alpha*tau*y)*
  (1-exp(-tau*y))^(beta-1)/B(alpha,beta), y>0.
- For held tau, the beta-shape likelihood is strictly concave, with sufficient
  statistics tau*mean(Y) and -mean(log(1-exp(-tau*Y))). Held subsets restrict
  this problem; alpha=1 also gives beta=-1/mean(log(1-exp(-tau*Y))) exactly.
- beta=1 identifies only alpha*tau. Canonical tau=1 is marked fixed when
  both factors are free, giving one likelihood degree of freedom. Away from
  this ridge, the exponential tail and its first correction identify shapes.
  Beta (tau=1), Kumaraswamy (alpha=1), and power (beta=1) are attained.
- alpha*tau ->lambda, tau ->0, alpha ->infinity gives gamma(beta,rate=lambda)
  for Y. `mllgamma(theta/X)` uses shapelog=beta, ratelog=lambda. Convert its
  log likelihood to X with sum(log(theta)-2*log(x)).
- tau ->infinity, alpha*tau ->lambda, log(beta)/tau ->c>=0 gives
  Y=c+Exp(lambda): power with endpoint theta*exp(-c). Fit endpoint=max(x)
  and lambda=1/(log(max(x))-mean(log(x))). Holding beta forces c=0.
- alpha,beta ->0, tau ->infinity, alpha*tau ->lambda, alpha/beta ->r>0
  gives mass r/(1+r) at Y=0 plus 1/(1+r) Exp(lambda). The unobserved endpoint
  atom only reduces the sample likelihood; zero weight dominates.
- Profiled gamma-face local correction is n*tau^2*(beta-1)*
  (mean(Y^2)-mean(Y)^2*(beta+1)/beta)/24. A positive coefficient excludes
  a local face maximum; a negative coefficient is not a global criterion.

## Numerically checked / supported constraints
- Installed `help("dgenbeta",package="actuar")` confirms all coefficient names
  and the stated transformation. Boundary/tail probes include both support
  endpoints, zero shapes, both probability tails, and log probabilities.
- All eight nonconstant held-shape subsets checked against independent
  actuar-density optim searches. Scale equivariance spans 1e-200 to 1e200.
- Exact gamma/power face fits use the 1e-8*n log-likelihood tie rule.
  Held beta retains its gamma shape; mllgamma cannot hold that shape, which
  the error explicitly states. Held-endpoint power is available through
  `mlgenbeta(x, scale = theta, shape2 = 1)`; mlpower frees its endpoint.
- 200 profile scans, seed 20261029, n=3,5,10,50, lognormal Y with several
  spreads and mixed clusters: no multiple maxima found (maximum root count 1).
  Scratch source/results: /tmp/umvl-codex/genbeta/profile-probe.{cpp,log}.
- Nonconstant observations must be strictly inside (0,scale). Constant data
  with any free beta shape are explicitly rejected; constant constrained
  subsets are not all classified. Both beta shapes held are supported.

## Exact verification counts
- `UMVL_GOLDEN_IDS=genbeta just goldens`: one new n=500 golden;
  logLik=548.75168869752292. Existing goldens untouched.
- C++ final build + `ctest -V`: 5/5 targets, 390/390 cases, 522491/522491
  assertions. Distribution target: 372 cases / 511591 assertions;
  genbeta itself: 3 cases / 22 assertions, including independent score checks.
- `just quality genbeta`: pass (symbolic likelihood, score, information, oracle).
- `just test`: 7990 passes, 0 failures, 0 warnings, 1 skip (optional sn package).
  The returned testthat object counts 6773 expectations in 473 blocks;
  the inclusive just-test reporter additionally counts 1217 standalone assertions.
- After the final overflow guards, rebuilt the adapter and reran genbeta:
  9 blocks / 117 expectations pass, with 0 failures/warnings/skips; two
  additional public overflow probes correctly stop with search unresolved.
- C++ regression cases cover overflowing beta normalizers and the power ridge;
  the held-shape gamma face uses a cancellation-resistant likelihood.


## Escape catalogue (20 per cell; seed 20261029)
`escape-probe.R`; no unresolved searches. These are numerical classifications.

| Generator | n=50 | n=500 |
|---|---|---|
| Beta(.4,.3) | interior 17, unit-gamma 3 | interior 20 |
| Kumaraswamy(2,.3) | interior 9, unit-gamma 11 | interior 14, unit-gamma 6 |
| Power(theta=1,lambda=.5) | interior 4, power 15, unit-gamma 1 | interior 6, power 13, unit-gamma 1 |
| exp(-Gamma(2,rate=3)) | interior 4, power 5, unit-gamma 11 | interior 7, unit-gamma 13 |
| GB1(.4,.3,2,theta=1) | interior 17, unit-gamma 3 | interior 20 |

## Ready-to-paste KNOWN_ISSUES.md rows
| actuar 3.3-7 | `dgenbeta(log=TRUE)` loses precision near the unit-gamma face | `actuar::dgenbeta(.5,1e12,2,1e-12,log=TRUE)` is -0.3665633, should be about -0.3665129 | repaired at runtime using stable log powers and lbeta |
| actuar 3.3-7 | `pgenbeta(log.p=TRUE)` underflows its intermediate power | `actuar::pgenbeta(1e-300,.01,2,100,log.p=TRUE)` is -Inf, should be about -690.76558 | repaired at runtime in `univariateML_reference_fixes` |
| actuar 3.3-7 | `qgenbeta` saturates its intermediate beta quantile near DBL_MIN | `actuar::qgenbeta(.5,.0001,2,100)` is about .00082690, should be 7.81012e-31 | repaired at runtime with logarithmic small-beta-tail inversion |
| actuar 3.3-7 | `rgenbeta` saturates its intermediate beta draws | `set.seed(81); actuar::rgenbeta(10,.0001,2,100)` gives nine identical .0007541415 values | repaired by stable quantile inversion; broken() preserves RNG state |

## Assumptions / open questions
- The mesh nominates all visible profile-score roots; it is not certified
  root isolation. Arbitrarily narrow basins and maxima outside the searched
  range are not formally excluded. Unresolved fits stop instead of returning
  a fit without the profile-score gate.
- The continuous closure catalogue has not been proved exhaustive for every
  joint parameter path. No additional nondegenerate continuous face was found.
- No necessary-and-sufficient elementary sample inequality for the interior
  MLE, or complete constrained constant-sample classification, is claimed.
- Seeds remain reproducible; repaired rgenbeta uses uniform quantile inversion,
  so its draws differ from actuar's seeded sequence while the defect persists.
