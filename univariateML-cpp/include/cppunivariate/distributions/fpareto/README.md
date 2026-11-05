---
id: fpareto
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0, shape2 > 0, shape3 > 0 and scale > 0; finite min held; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Feller-Pareto
support: {type: real, lower: "min", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "if (shape2*shape3 < 1) Inf else if (shape2*shape3 > 1) -Inf else log(shape2) - log(scale) - lbeta(shape1, shape3)"}}
params:
- name: shape1
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
- name: shape2
  symbol: '\gamma'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape3
  symbol: '\tau'
  role: shape
  domain: (0, inf)
  simulate: 2
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
  advice: 'With all coefficients free, shape2 * shape3 below 1 makes the density singular as min approaches the sample minimum. The fully free likelihood is unbounded. Hold min, for example min = 0, to fit the shifted gb2 family. Estimating min with other coefficients held is not supported.'
  kind: threshold
r:
  density: dfpareto
  sampler: rfpareto
  control:
    min: Known finite lower endpoint.
    reltol: Delegate fitting tolerance.
    iterlim: Maximum delegate iterations.
log_density: log(shape2)-log((x-min))-loggamma(shape3)-loggamma(shape1)+loggamma(shape3+shape1)+shape3*shape2*log((x-min)/scale)-(shape3+shape1)*log(1+((x-min)/scale)**shape2)
limits:
- {firmness: proved, model: weibull, when: 'With shape3 held at one, the Burr submodel tends to its Weibull face.', status: WeibullBoundary, fixed: [shape3], numerical: true}
- {firmness: proved, model: invweibull, when: 'With shape1 held at one, the inverse Burr submodel tends to its inverse-Weibull face.', status: InvWeibullBoundary, fixed: [shape1], numerical: true}
- name: unbounded likelihood
  firmness: proved
  when: With $\alpha$ and $\tau$ positive and fixed, the power $\gamma$ tends to infinity and scale places the concentrating distribution at the common excess.
  detect: The threshold is held and the excesses are constant, with both power and scale free. Other held subsets require their own concentration check.
  status: InvalidData
  message: unbounded
- when: The upper beta shape $\alpha$ tends to infinity with $\gamma$ and $\tau$ fixed and $\sigma\alpha^{-1/\gamma}$ fixed at a positive scale.
  firmness: proved
  detect: The held coefficients permit this path, and the numerical comparison selects this limiting model within $10^{-8}n$ log-likelihood units of the best interior candidate found.
  status: TrGammaBoundary
  name: shifted trgamma; subtract min before fitting
  numerical: true
- when: The lower beta shape $\tau$ tends to infinity with $\alpha$ and $\gamma$ fixed and $\sigma\tau^{1/\gamma}$ fixed at a positive scale.
  firmness: proved
  detect: The held coefficients permit this path, and the numerical comparison selects this limiting model within $10^{-8}n$ log-likelihood units of the best interior candidate found.
  status: InvTrGammaBoundary
  name: shifted invtrgamma; subtract min before fitting
  numerical: true
- when: Both beta shapes equal $K\to\infty$, the power is $\gamma=\sqrt{2\psi_1(K)}/s$, and scale is $\sigma=e^m$, with lognormal log-scale $s>0$ and log-location $m$ fixed. Here $\psi_1$ is the trigamma function.
  firmness: proved
  detect: The held coefficients permit this path, and the numerical comparison selects this limiting model within $10^{-8}n$ log-likelihood units of the best interior candidate found.
  status: LognormalBoundary
  name: shifted lnorm; subtract min before fitting
  numerical: true
- when: The power $\gamma$ tends to infinity with $\alpha\gamma$ positive and fixed, while $\tau$ and $\sigma$ stay fixed.
  firmness: proved
  detect: The held coefficients permit this path, and the numerical comparison selects this limiting model within $10^{-8}n$ log-likelihood units of the best interior candidate found.
  status: ParetoBoundary
  name: shifted pareto; subtract min before fitting
  numerical: true
- when: The power $\gamma$ tends to infinity with $\tau\gamma$ positive and fixed, while $\alpha$ and $\sigma$ stay fixed.
  firmness: proved
  detect: The held coefficients permit this path, and the numerical comparison selects this limiting model within $10^{-8}n$ log-likelihood units of the best interior candidate found.
  status: PowerBoundary
  name: shifted power; subtract min before fitting
  numerical: true
- name: shifted double Pareto; subtract min before fitting
  firmness: proved
  when: The power $\gamma$ tends to infinity with both $\alpha\gamma$ and $\tau\gamma$ positive and fixed, while $\sigma$ stays fixed.
  detect: The held coefficients permit this path, and the numerical comparison selects this limiting model within $10^{-8}n$ log-likelihood units of the best interior candidate found.
  status: DoubleParetoBoundary
  message: GB2 selects the double Pareto limit
  numerical: true
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
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape3
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape1
  - shape3
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape2
  - shape3
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape1
  - shape2
  - shape3
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
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape1
  - shape2
  - scale
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape3
  - scale
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape1
  - shape3
  - scale
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape2
  - shape3
  - scale
  status: not-implemented
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape1
  - shape2
  - shape3
  - scale
  status: not-implemented
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
fit_args:
  min: 0
  shape1: 0.3
  shape3: 0.4
examples: |
  set.seed(1)
  x <- rml(200, mlfpareto())
  mlfpareto(x, min = 0L, shape1 = 0.3, shape3 = 0.4)

group: Pareto and Lomax
construction:
- base
- generalisation
relations:
  transform_of:
  - family: gb2
    transform: shift
    at:
      shape1: shape1
      shape2: shape2
      shape3: shape3
      scale: scale
    shift: min
fit:
  method: delegation
  certified: partial
  note: 'fpareto/README.md, Details; fpareto/mle.hpp (public fitter and held overloads); fpareto/test.cpp: With min held, GB2 on excesses transfers certified Burr and inverse Burr submodels when shape3=1 or shape1=1 is held. The parent held-scale two-shape profiles remain numerical. Holding both beta shapes gives a concave power/location likelihood, and holding power and scale gives a concave beta-shape likelihood. Outside unit-shape reductions, free-beta-shape profiles with power or scale free retain numerical searches and face comparisons. Face coefficients refer to excess data.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model insurance loss severity with independently adjustable tails above a supplied
  threshold.
summary: Feller-Pareto provides a model for insurance loss severity with independently adjustable tails above a
  supplied threshold.
aliases: []
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
- 'Dutang, C., Goulet, V. and Langevin, N. (2022). Feller-Pareto and Related Distributions: Numerical Implementation
  and Actuarial Applications. Journal of Statistical Software, 103(6), 1–22. doi:10.18637/jss.v103.i06.'
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, every nonempty sample has unbounded likelihood.
    Holding $\mu$ gives the generalized beta of the second kind (GB2) on
    excesses. Its joint likelihood can escape to several proper limit models;
    a complete joint existence criterion and joint uniqueness are not established.
    Holding both beta shapes $\alpha$ and $\tau$ gives a unique finite
    power/scale fit for nonconstant excesses, while constant data remain unbounded
    if both power and scale are free. With power and scale held, the free
    beta shapes have a unique finite fit for nonconstant excesses, but both
    free beta shapes can concentrate on constant data. Further holds can
    remove these concentration paths. Free-threshold fits are not supported.
    Holding all coefficients leaves likelihood evaluation.
  reason: >-
    With $\gamma\tau<1$, moving a free threshold to the sample minimum makes
    the density factors there diverge as $(x-\mu)^{\gamma\tau-1}$.
    With the threshold held, put $y_i=x_i-\mu$. These excesses have a GB2
    likelihood. For held beta
    shapes, the log-excess likelihood in $(\gamma,\gamma\log\sigma)$ is
    strictly concave, with coercive profiles for nonconstant data. For held
    power and scale, transforming to $T_i=(y_i/\sigma)^\gamma/
    \{1+(y_i/\sigma)^\gamma\}$ gives a beta likelihood. Its log normalizer
    has a positive definite Hessian, and nonconstant interior observations
    place the mean sufficient statistic inside its convex support.
    These conditional results do not settle the outer joint search.
---

The Feller–Pareto distribution models sizes above a threshold $\mu$ with independently adjustable lower and upper tails. The scale $\sigma$ sets the size unit, the power $\gamma$ controls the transformation of excesses, and the beta shapes $\tau$ and $\alpha$ give lower and upper tail powers $\gamma\tau$ and $\gamma\alpha$.

## Details

Fitting requires finite observations strictly above a held threshold. Subtracting the threshold gives [GB2](../gb2/README.md). In the usual GB2 coordinates $(a,b,p,q)$, the mapping is $(\gamma,\sigma,\tau,\alpha)$. Identification and conditional existence results transfer through this shift.

The fit profiles power and log scale for each pair of beta shapes. The inner problem is strictly concave; the outer search explores and refines several numerical basins. It compares transformed gamma, inverse transformed gamma, lognormal, Pareto, power and double-Pareto faces as allowed by the held coefficients. Neither global optimality nor exhaustion of all possible faces is proved. A small score and an interior location are required for a returned interior candidate; unresolved searches stop.

Holding both beta shapes gives a unique power/scale maximum on nonconstant excesses. Constant excesses allow power to grow and scale to concentrate at the common value. Holding power too gives a unique scale maximum even for constant data. With power and scale held, both beta shapes have a finite unique maximum for nonconstant excesses. If only one beta shape remains free, its strictly decreasing score also permits constant excesses.

With a free threshold, choose $\gamma\tau<1$. As the threshold approaches a minimum tied $k$ times, the minimum factors diverge as $\varepsilon^{k(\gamma\tau-1)}$ and the remaining factors have positive limits. This proves unboundedness for the fully free likelihood. It does not prove non-existence for every held-coefficient submodel. For example, $\gamma=\tau=1$ with the other coefficients held reduces to the monotone Pareto-II endpoint problem. The interface still requires a held threshold.

When scale is held, a selected endpoint face retains $\mu+\sigma$ as its Pareto endpoint or power upper endpoint. Refitting that endpoint freely changes the model. Some held-scale searches remain unresolved, including deterministic Pareto and power samples, and stop without returning a fitted model.

The [family analysis](../../../../../dev/research/evaluations/2026-10-28-gb2.md) gives the limiting paths and boundary comparisons.

## R notes

With `min = 0`, fits agree with `mlgb2(x)` for the same holds. Faces are compared within $10^{-8}n$ log-likelihood units; this numerical tolerance can reject a tied or slightly better finite fit. Errors name the shifted limiting fit where available. No double-Pareto fitter is provided. All-held fits evaluate the supplied density.

## Decision history

The 2026-10-31 GB2 member-seed subsampling for samples larger than 1000
also applies through this delegate when all four GB2 coefficients are free.
The held-threshold shift, validation and full-data final checks are unchanged;
see [GB2](../gb2/README.md) and the
[paired evidence](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).
