---
id: nsbeta
name: Nonstandard beta
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: shape1
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
- name: shape2
  role: shape
  domain: (0, inf)
  simulate: 3
  kind: regular
- name: min
  role: lower endpoint
  domain: "(-inf, max)"
  simulate: 0
  advice: 'With the bounds estimated the likelihood is unbounded: a shape below 1
    makes the density singular at its bound, so let that bound approach the sample
    extreme. Hold both, e.g. `mlnsbeta(x, min = 0, max = 1)`, which is `mlbeta(x)`.
    Fits with bounds free and other parameters held are not supported.'
  kind: threshold
- name: max
  role: upper endpoint
  domain: "(min, inf)"
  simulate: 1
  advice: 'With the bounds estimated the likelihood is unbounded: a shape below 1
    makes the density singular at its bound, so let that bound approach the sample
    extreme. Hold both, e.g. `mlnsbeta(x, min = 0, max = 1)`, which is `mlbeta(x)`.
    Fits with bounds free and other parameters held are not supported.'
  kind: threshold
r:
  density: extraDistr::dnsbeta
  sampler: extraDistr::rnsbeta
  control:
    min: Known finite lower endpoint.
    max: Known finite upper endpoint larger than min.
    reltol: Delegate fitting tolerance.
    iterlim: Maximum delegate iterations.
    shape1: Known positive finite coefficient; NULL estimates it.
    shape2: Known positive finite coefficient; NULL estimates it.
limits:
  - name: unbounded likelihood
    when: A free concentration can collapse on constant observations
    detect: Constant samples; the held coefficients determine whether concentration remains free
    status: Degenerate
    message: unbounded
log_density: (shape1-1)*log(((x-min)/(max-min))) + (shape2-1)*log(1-((x-min)/(max-min))) - loggamma(shape1) - loggamma(shape2) + loggamma(shape1+shape2)-log(max-min)
examples: |
  set.seed(3)
  mlnsbeta(simulate_default("mlnsbeta", 100), min = 0, max = 1)
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
---

This family has a known support endpoint and delegates fitting to beta.

## Details

For at least two finite, nonconstant observations strictly between the held bounds, the MLE is unique and attained: the transformed beta log likelihood is strictly concave in its two shapes. Constant transformed data have unbounded likelihood as both shapes grow.

Observations must be strictly inside the held support. Free endpoints with other parameters held are not supported; those subsets need separate existence studies. The delegate retains its sample restrictions and limit comparisons.

With the opposite bound held beyond the sample, a shape a < 1 and its bound approaching the corresponding extreme at distance epsilon gives k factors proportional to epsilon^(a-1); all other factors approach positive constants. Thus the likelihood diverges. The held-bounds fit transforms to beta and subtracts n log(max-min) from log likelihood.

With both bounds held, either held beta shape leaves a strictly decreasing score with one root for the other shape, even on constant samples strictly inside the bounds.
