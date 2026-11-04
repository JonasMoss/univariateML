---
id: pareto4
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0, shape2 > 0, scale > 0 and min; min held; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Pareto type IV
support: {type: real, lower: "min", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "if (shape2 < 1) Inf else if (shape2 > 1) -Inf else log(shape1) - log(scale)"}}
params:
- name: shape1
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape2
  symbol: '\gamma'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: min
  symbol: '\mu'
  role: lower endpoint
  domain: (-inf, inf)
  simulate: 0
  advice: 'With all coefficients free, shape2 below 1 makes the density singular as min approaches the sample minimum. The fully free likelihood is unbounded. Hold min, for example min = 0, to fit the shifted burr family. Estimating min with other coefficients held is not supported.'
  kind: threshold
r:
  density: dpareto4
  sampler: rpareto4
  control:
    min: Known finite lower endpoint.
    reltol: Delegate fitting tolerance.
    iterlim: Maximum delegate iterations.
    shape1: Known positive finite coefficient; NULL estimates it.
    shape2: Known positive finite coefficient; NULL estimates it.
log_density: log(shape1) + log(shape2) - log((x-min)) - shape2*log(scale/(x-min)) - (shape1+1)*log(1+((x-min)/scale)**shape2)
limits:
- name: unbounded likelihood
  firmness: proved
  when: A free power $\gamma$ tends to infinity while the other free coefficients concentrate the excess distribution at its common observed value.
  detect: With threshold held, the excesses are constant and the held coefficients permit concentration. In particular, this occurs with both shapes and scale free.
  status: Unbounded
  message: unbounded
- when: The shape $\alpha$ tends to infinity with $\gamma$ fixed and $\sigma\alpha^{-1/\gamma}$ fixed at a positive scale.
  firmness: proved
  detect: The held coefficients permit this path, and the numerical comparison selects this limiting model within $10^{-8}n$ log-likelihood units of the best interior candidate found.
  status: WeibullBoundary
  name: shifted weibull; subtract min before fitting
  numerical: true
- when: The power $\gamma$ tends to infinity with $\alpha\gamma$ fixed at a positive tail exponent and $\sigma$ fixed.
  firmness: proved
  detect: The held coefficients permit this path, and the numerical comparison selects this limiting model within $10^{-8}n$ log-likelihood units of the best interior candidate found.
  status: ParetoBoundary
  name: shifted pareto; subtract min before fitting
  numerical: true
- when: The power $\gamma$ tends to infinity with $\alpha\gamma$ fixed at a positive tail exponent and $\sigma$ fixed.
  firmness: proved
  detect: Scale and threshold are held, both shapes are free, and every excess is at least $\sigma$. If all excesses equal $\sigma$, the likelihood is instead unbounded.
  status: AllGeOne
  fixed: [scale]
  name: shifted Pareto with its endpoint held at min + scale
holds:
- held: []
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape1
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape2
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape1
  - shape2
  status: not-implemented
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - scale
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape1
  - scale
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape2
  - scale
  status: not-implemented
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape1
  - shape2
  - scale
  status: not-implemented
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
fit_args:
  min: 0
examples: |
  set.seed(1)
  x <- rml(200, mlpareto4())
  mlpareto4(x, min = 0L)

group: Pareto and Lomax
construction:
- base
- generalisation
relations:
  special_case_of:
  - family: fpareto
    at:
      shape1: shape1
      shape2: shape2
      shape3: 1
      scale: scale
      min: min
  transform_of:
  - family: burr
    transform: shift
    at:
      shape1: shape1
      shape2: shape2
      scale: scale
    shift: min
fit:
  method: delegation
  certified: partial
  note: 'pareto4/README.md, Details; pareto4/mle.hpp (public fitter and held overloads); pareto4/test.cpp: Held
    threshold transfers to Lomax, log-logistic or Burr; conditional monotone subsets are certified but joint excess
    profiles inherit numerical-search qualifications. Free thresholds stop. An explicit certified C++ joint candidate is available, but the default switch is blocked by unchanged golden tolerances (TASK-116).'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model thresholded income and losses with a Burr excess distribution.
summary: Pareto type IV provides a model for thresholded income and losses with a Burr excess distribution.
aliases: []
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, every nonempty sample has unbounded likelihood.
    Holding $\mu$ gives the Burr likelihood on the excesses. Constant excesses
    have unbounded joint likelihood, while nonconstant samples may favour
    Weibull or Pareto limits. A complete joint existence criterion and joint
    uniqueness are not established. Holding $\alpha$ gives a unique finite
    fit on nonconstant excesses. Holding $\gamma$ reduces existence to Lomax
    on powered excesses. Holding both shapes gives a unique scale estimate.
    Holding scale alone gives a finite maximum for nonconstant excesses if
    some excess is below it; uniqueness is not established. Other conditional
    cases are described below. Free-threshold fits are not supported.
    Holding all coefficients leaves likelihood evaluation.
  reason: >-
    A free threshold approaching the sample minimum with $\gamma<1$ creates
    a density singularity of order $(x-\mu)^{\gamma-1}$.
    With $\mu$ held, subtracting it gives Burr exactly. With $\alpha$ held,
    the log-excess likelihood is strictly concave in $\gamma$ and
    $\gamma\log\sigma$ and has coercive profiles for nonconstant data.
    With $\gamma$ held, the transformation $(x_i-\mu)^\gamma$ gives Lomax.
    The single-coordinate scores decrease strictly in suitable coordinates.
    These arguments do not certify the joint numerical search.
---

Pareto type IV models sizes above a threshold $\mu$ with Burr-distributed excesses. The scale $\sigma$ sets the size unit. The shapes $\alpha$ and $\gamma$ give upper-tail exponent $\alpha\gamma$ and lower-end power $\gamma$.

## Details

Fitting requires finite observations strictly above a held threshold. Subtracting that threshold gives [Burr](../burr/README.md) with unchanged shapes and scale. The joint fit uses several numerical starts and compares candidate maxima with Weibull and Pareto limits. Global optimality is not certified.

With only scale held, nonconstant excesses all at least $\sigma$ have a supremum on the endpoint-$\sigma$ Pareto path. Its log-likelihood is the held-endpoint Pareto log-likelihood minus $Z\log2$, where $Z$ counts excesses equal to $\sigma$. The endpoint remains $\mu+\sigma$, so estimating it anew would change the constrained model. For nonconstant data with an excess below $\sigma$, a finite maximum exists. Constant excesses at or below $\sigma$ instead have unbounded likelihood with both shapes free. Constant excesses above it have a finite unattained Pareto path limit.

Holding $\alpha$ permits a unique joint scale/power fit for nonconstant excesses, but constant data remain unbounded when power and scale are free. Holding $\gamma$ reduces to Lomax on powered excesses, whose exponential limit becomes a Weibull limit with that power held. Holding both shapes gives a unique scale maximum, even for constant data. Holding $\alpha$ and scale gives a unique power maximum unless every excess equals scale. Holding $\gamma$ and scale gives an explicit unique estimate of $\alpha$.

For the fully free model, choose $\gamma<1$ and let the threshold approach the sample minimum. The $k$ minimum factors diverge as $\varepsilon^{k(\gamma-1)}$ while all other density factors have positive limits. Holding some coefficients can remove this path. For example, $\gamma=1$ with both other coefficients held gives a finite endpoint maximum. The interface nevertheless requires a held threshold for every subset.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md) gives the boundary arguments and their held-parameter conditions.

## R notes

With `min = 0`, the fit agrees with `mlburr(x)` for the same holds. Numerical comparisons prefer limit models within $10^{-8}n$ log-likelihood units, so a tied or slightly better finite maximum may be rejected. Detected faces stop with a recommendation to use `mlweibull(x - min)` or `mlpareto(x - min)`, as applicable. Held-scale endpoint faces preserve `min + scale`. Unresolved searches stop.

## Decision history

2026-10-31 (golden comparison precision):


The double-only baseline and TASK-132 headers return bit-identical estimates;
the largest relative coefficient difference from the extended-precision golden
is 4.18e-7. The double-only comparison uses 5e-7 (also used for wasm), retaining
the 1e-8 native tolerance. An independent analytic total score in log(shape1),
log(shape2), log(scale) has norm 2.02e-5 and independently summed likelihood is
within 1.1e-12 of the golden. These are numerical stationarity/regression checks,
not a global optimality proof. The appended precision case retains a 1e-3 score
gate and 1e-8 absolute likelihood checks. Reproducer:
[pareto4-precision.cpp](../../../../../dev/validation/pareto4-precision.cpp).
The fitter and golden are unchanged.

## Certified excess candidate and compatibility gate (2026-11-04, TASK-116)

The explicit C++ `pareto4_mle_certified(x, min, tol, maxit)` entrypoint
transports the Burr certificate to excesses. For $y_i=x_i-\mu$ with
$\mu$ held, both shapes and scale are unchanged and the Jacobian is one:
$\ell_x=\ell_y$. Thus every global exclusion, unique finite root and
Weibull/Pareto face certificate transfers unchanged. The two faces are
shifted Weibull and shifted Pareto; subtract `min` to fit those models.
Ties, budget exhaustion and numerical failures preserve the Burr statuses
and NaN estimates. This entrypoint never fits a free threshold or alters
held-subset algorithms.

The exact-arithmetic guarantee is conditional on computed comparisons,
with the practical floating-point margin and libm assumptions of the
[Burr dossier](../burr/README.md) and
[production certificate report](../../../../../dev/research/evaluations/2026-11-04-burr-port-report.md).
It is not an outward-rounded interval guarantee. The transformed observations
must be representable in binary64. A universal joint uniqueness theorem is
not claimed; an accepted finite outcome has a sample-specific proof.

**The public default and R fit remain on the legacy engine.** Switching to
the certified root fails the unchanged native and R golden tolerances.
The [dependency report](../../../../../dev/research/evaluations/2026-11-04-burr-deps-report.md)
records the high-precision root comparison and the exact compatibility gap.
The `fit` metadata and limit detection above therefore still describe the
production default. Resolving the golden contract requires an author decision;
a certificate is not bypassed by returning a less accurate legacy candidate.

The certified transformation rejects arithmetic overflow and any merging of
distinct observations into equal transformed values with `NumericalFailure`.
This prevents rounded transformations from supplying artificial constant-sample
or tied-endpoint certificates. Genuine data ties remain allowed.
