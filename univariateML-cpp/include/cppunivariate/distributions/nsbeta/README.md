---
id: nsbeta
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0 and shape2 > 0; finite min < max held; either shape free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Nonstandard beta
support: {type: real, lower: "min", upper: "max", closed: [true, true], endpoint_log_density: {lower: "if (shape1 < 1) Inf else if (shape1 > 1) -Inf else log(shape2) - log(max-min)", upper: "if (shape2 < 1) Inf else if (shape2 > 1) -Inf else log(shape1) - log(max-min)"}}
params:
- name: shape1
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape2
  symbol: '\beta'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: min
  symbol: 'a'
  role: lower endpoint
  domain: "(-inf, max)"
  simulate: 0
  advice: 'With both endpoints and shapes free, the likelihood is unbounded because a shape below one creates an endpoint singularity. Supply min and max, for example mlnsbeta(x, min = 0, max = 1), equivalent to mlbeta(x). Estimating endpoints with shapes held is not supported.'
  kind: threshold
- name: max
  symbol: 'b'
  role: upper endpoint
  domain: "(min, inf)"
  simulate: 1
  advice: 'With both endpoints and shapes free, the likelihood is unbounded because a shape below one creates an endpoint singularity. Supply min and max, for example mlnsbeta(x, min = 0, max = 1), equivalent to mlbeta(x). Estimating endpoints with shapes held is not supported.'
  kind: threshold
r:
  density: dnsbeta
  sampler: rnsbeta
  control:
    min: Known finite lower endpoint.
    max: Known finite upper endpoint larger than min.
    reltol: Delegate fitting tolerance.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
    shape1: Known positive finite coefficient; NULL estimates it.
    shape2: Known positive finite coefficient; NULL estimates it.
limits:
  - name: unbounded likelihood
    firmness: proved
    when: The shapes satisfy $\alpha=tu$ and $\beta=t(1-u)$ with $t\to\infty$, where $u=(x-a)/(b-a)$ is the common transformed observation and the endpoints are held.
    detect: All observations equal one point strictly between the held endpoints, and both shapes are free.
    status: Degenerate
    message: unbounded
log_density: (shape1-1)*log(((x-min)/(max-min))) + (shape2-1)*log(1-((x-min)/(max-min))) - loggamma(shape1) - loggamma(shape2) + loggamma(shape1+shape2)-log(max-min)
holds:
- held: []
  status: no-mle
  reason: 'Estimating min or max is not supported. A free endpoint with its corresponding shape free or below one gives unbounded likelihood. Other held-shape cases may have a finite MLE but are not computed. Supply both min and max.'
- held: [shape1]
  status: no-mle
  reason: 'Estimating min or max is not supported. A free endpoint with its corresponding shape free or below one gives unbounded likelihood. Other held-shape cases may have a finite MLE but are not computed. Supply both min and max.'
- held: [shape2]
  status: no-mle
  reason: 'Estimating min or max is not supported. A free endpoint with its corresponding shape free or below one gives unbounded likelihood. Other held-shape cases may have a finite MLE but are not computed. Supply both min and max.'
- held: [shape1, shape2]
  status: not-implemented
  reason: 'Estimating min or max is not supported. A free endpoint with its corresponding shape free or below one gives unbounded likelihood. Other held-shape cases may have a finite MLE but are not computed. Supply both min and max.'
- held: [min]
  status: no-mle
  reason: 'Estimating min or max is not supported. A free endpoint with its corresponding shape free or below one gives unbounded likelihood. Other held-shape cases may have a finite MLE but are not computed. Supply both min and max.'
- held: [shape1, min]
  status: no-mle
  reason: 'Estimating min or max is not supported. A free endpoint with its corresponding shape free or below one gives unbounded likelihood. Other held-shape cases may have a finite MLE but are not computed. Supply both min and max.'
- held: [shape2, min]
  status: not-implemented
  reason: 'Estimating min or max is not supported. A free endpoint with its corresponding shape free or below one gives unbounded likelihood. Other held-shape cases may have a finite MLE but are not computed. Supply both min and max.'
- held: [shape1, shape2, min]
  status: not-implemented
  reason: 'Estimating min or max is not supported. A free endpoint with its corresponding shape free or below one gives unbounded likelihood. Other held-shape cases may have a finite MLE but are not computed. Supply both min and max.'
- held: [max]
  status: no-mle
  reason: 'Estimating min or max is not supported. A free endpoint with its corresponding shape free or below one gives unbounded likelihood. Other held-shape cases may have a finite MLE but are not computed. Supply both min and max.'
- held: [shape1, max]
  status: not-implemented
  reason: 'Estimating min or max is not supported. A free endpoint with its corresponding shape free or below one gives unbounded likelihood. Other held-shape cases may have a finite MLE but are not computed. Supply both min and max.'
- held: [shape2, max]
  status: no-mle
  reason: 'Estimating min or max is not supported. A free endpoint with its corresponding shape free or below one gives unbounded likelihood. Other held-shape cases may have a finite MLE but are not computed. Supply both min and max.'
- held: [shape1, shape2, max]
  status: not-implemented
  reason: 'Estimating min or max is not supported. A free endpoint with its corresponding shape free or below one gives unbounded likelihood. Other held-shape cases may have a finite MLE but are not computed. Supply both min and max.'
fit_args:
  min: 0
  max: 1
examples: |
  set.seed(1)
  x <- rml(200, mlnsbeta())
  mlnsbeta(x, min = 0L, max = 1L)

group: beta and generalised beta
construction:
- base
relations:
  transform_of:
  - family: genbeta
    at:
      shape1: shape1
      shape2: shape2
      shape3: 1
      scale: max-min
    transform: shift
    shift: min
  - family: beta
    at:
      shape1: shape1
      shape2: shape2
    transform: affine
    shift: min
    scale: max-min
fit:
  method: delegation
  certified: 'yes'
  note: 'nsbeta/README.md, Details; nsbeta/mle.hpp (public fitter and held overloads); nsbeta/test.cpp: Strictly
    concave beta likelihood in its two shapes; held-shape digamma scores decrease strictly. nsbeta uses supplied
    endpoints; unsupported endpoint fits stop.'
areas:
- proportions
classification: specialist
classification_reason: Used to model beta-shaped measurements on a supplied physical interval.
summary: Nonstandard beta provides a model for beta-shaped measurements on a supplied physical interval.
aliases:
- "non-standard beta"
- "four-parameter beta"
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all four coefficients free, the likelihood is unbounded.
    With both endpoints held and observations strictly between them, the
    MLE of the two shapes exists and is unique exactly when the data are
    nonconstant. Holding either shape as well gives a unique finite MLE
    for the other shape, including for constant samples. Estimating an
    endpoint with shapes held is not implemented and need not imply
    non-existence. No joint uniqueness claim is made for free endpoints.
  reason: >-
    Moving a free endpoint toward its sample extreme gives an unbounded
    likelihood if its corresponding shape is below one. With endpoints
    held, $y=(x-a)/(b-a)$ has a beta likelihood. Its log-normalizer has
    positive definite Hessian, the covariance of $(\log y,\log(1-y))$.
    Strict concavity and an interior mean sufficient statistic give a
    unique maximum for nonconstant samples. For a single free shape, the
    digamma score strictly decreases from positive infinity to a negative
    limit. For constant data with both shapes free, concentration at that
    interior point makes the likelihood diverge.
---

The nonstandard beta distribution models measurements on an interval with
supplied lower and upper endpoints. Its two shapes control behaviour near
the lower and upper endpoints, respectively.

## Details

For a constant sample at an interior point $x$, hold the endpoints
fixed and put $u=(x-a)/(b-a)\in(0,1)$. Taking beta shapes
$tu,t(1-u)$, Stirling's formula gives density at $x$ asymptotic to
$\sqrt{t/(2\pi u(1-u))}/(b-a)$. Its $n$th power diverges.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md) describes the limiting paths and the scope of the boundary comparisons.

For at least two finite, nonconstant observations strictly between the held bounds, the MLE is unique and attained: the transformed beta log likelihood is strictly concave in its two shapes. Constant transformed data have unbounded likelihood as both shapes grow.

Observations must be strictly inside the held support. Endpoint estimation with held shapes is not implemented. It is not always a mathematical failure: with both shapes equal to one, the uniform likelihood is maximized by placing free endpoints at the sample extremes, provided the fitted interval has positive width. The fit retains the beta model's sample restrictions and limiting comparisons.

Hold the opposite endpoint beyond the sample. If the shape $s$ associated with the free endpoint satisfies $s<1$, moving that endpoint to within $\varepsilon$ of the corresponding sample extreme gives $k$ factors proportional to $\varepsilon^{s-1}$, where $k$ observations equal that extreme. All other factors approach positive constants, so the likelihood diverges. The held-bounds fit transforms to beta and subtracts $n\log(b-a)$ from the log likelihood.

With both bounds held, either held beta shape leaves a strictly decreasing score with one root for the other shape, even on constant samples strictly inside the bounds.

With one shape held, the free shape uses the same monotone beta score solve as the unit-interval family, with the caller’s iteration budget. This matters near a held endpoint: the free shape may exceed $10^{150}$ while the observations remain finite. The score uses a digamma increment, and the normalizer uses a log-beta difference, so neither is recovered by subtracting nearly equal special-function values. The [endpoint reference cases](../../../../../dev/research/evaluations/2026-11-09-audit-beta-data/reference.json) cover both tails and include a free shape above $10^{150}$.

## R notes

Supply min and max. Passing NULL for either stops with the endpoint
explanation. With min = 0 and max = 1 this is the ordinary beta fit.

## Decision history

### Diagnostic status contract (2026-10-31)

Invalid and empty samples carry non-success statuses, including delegate paths.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp`.

### Translated quantiles and shape extremes

A beta quantile rounded before affine transport can erase a nonzero result when the endpoints straddle zero. Central symmetric inversion retains the offset from one half through the compensated midpoint calculation. The probability normalizer also avoids subtraction of large log-gamma values when one shape diverges.

The concentrating symmetric tail uses t=log((x-min)/(max-x)), with a=shape1=shape2. Rounding its two beta coordinates can erase a resolved displacement. This probability bridge returns NaN for sqrt(a)*E > 2.5e-11 and 0 < |t| < 2E/1e-10, where E is eight binary64 machine epsilons. At t=0 the exact symmetric half-probability remains available; the stated margin is practical floating-point evidence.

The shape2 one/two beta-coordinate reductions retain the cumulative hazard before rescaling the interval. Their closed antiderivatives avoid backend normalizer loss in double-width builds; at shape2 two a positive-deviance sum retains the complement at the upper endpoint.
