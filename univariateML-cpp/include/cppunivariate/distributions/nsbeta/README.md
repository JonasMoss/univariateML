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
  advice: 'With the bounds estimated the likelihood is unbounded: a shape below 1
    makes the density singular at its bound, so let that bound approach the sample
    extreme. Hold both, e.g. `mlnsbeta(x, min = 0, max = 1)`, which is `mlbeta(x)`.
    Fits with bounds free and other parameters held are not supported.'
  kind: threshold
- name: max
  symbol: 'b'
  role: upper endpoint
  domain: "(min, inf)"
  simulate: 1
  advice: 'With the bounds estimated the likelihood is unbounded: a shape below 1
    makes the density singular at its bound, so let that bound approach the sample
    extreme. Hold both, e.g. `mlnsbeta(x, min = 0, max = 1)`, which is `mlbeta(x)`.
    Fits with bounds free and other parameters held are not supported.'
  kind: threshold
r:
  density: dnsbeta
  sampler: rnsbeta
  control:
    min: Known finite lower endpoint.
    max: Known finite upper endpoint larger than min.
    reltol: Delegate fitting tolerance.
    iterlim: Maximum delegate iterations.
    shape1: Known positive finite coefficient; NULL estimates it.
    shape2: Known positive finite coefficient; NULL estimates it.
limits:
  - name: unbounded likelihood
    firmness: argued
    when: A free concentration can collapse on constant observations
    detect: Constant samples; the held coefficients determine whether concentration remains free
    status: Degenerate
    message: unbounded
log_density: (shape1-1)*log(((x-min)/(max-min))) + (shape2-1)*log(1-((x-min)/(max-min))) - loggamma(shape1) - loggamma(shape2) + loggamma(shape1+shape2)-log(max-min)
holds:
- held: []
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape2
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - shape2
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - min
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - min
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape2
  - min
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - shape2
  - min
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - max
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - max
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape2
  - max
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - shape2
  - max
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
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
  method: multistart
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
---

This family has a known support endpoint and delegates fitting to beta.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md) describes the limiting paths and the scope of the boundary comparisons.

For at least two finite, nonconstant observations strictly between the held bounds, the MLE is unique and attained: the transformed beta log likelihood is strictly concave in its two shapes. Constant transformed data have unbounded likelihood as both shapes grow.

Observations must be strictly inside the held support. Free endpoints with other parameters held are not supported; those subsets need separate existence studies. The delegate retains its sample restrictions and limit comparisons.

With the opposite bound held beyond the sample, a shape a < 1 and its bound approaching the corresponding extreme at distance epsilon gives k factors proportional to epsilon^(a-1); all other factors approach positive constants. Thus the likelihood diverges. The held-bounds fit transforms to beta and subtracts n log(max-min) from log likelihood.

With both bounds held, either held beta shape leaves a strictly decreasing score with one root for the other shape, even on constant samples strictly inside the bounds.

### Diagnostic status contract (2026-10-31)

Invalid and empty samples carry non-success statuses, including delegate paths.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp`.
