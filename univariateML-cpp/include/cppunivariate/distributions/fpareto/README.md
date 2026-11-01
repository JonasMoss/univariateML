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
  advice: 'With min estimated the likelihood is unbounded: the density is singular
    at min when shape2 * shape3 < 1, so let min rise to the sample minimum. Hold it,
    e.g. `mlfpareto(x, min = 0)`, which is `mlgb2(x)`. Fits with min free and other
    parameters held are not supported.'
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
- name: unbounded likelihood
  firmness: proved
  when: A free concentration can collapse on constant observations
  detect: Constant samples; the held coefficients determine whether concentration remains free
  status: InvalidData
  message: unbounded
- when: shape1 tends to infinity with shape2 and shape3 fixed and scale divided by shape1^(1/shape2) fixed
  firmness: proved
  status: TrGammaBoundary
  name: shifted trgamma; subtract min before fitting
- when: shape3 tends to infinity with shape1 and shape2 fixed and scale times shape3^(1/shape2) fixed
  firmness: proved
  status: InvTrGammaBoundary
  name: shifted invtrgamma; subtract min before fitting
- when: shape1 = shape3 = K tends to infinity, shape2 = sqrt(2*trigamma(K))/sigma, and scale = exp(mu), with sigma > 0 and mu fixed
  firmness: proved
  status: LognormalBoundary
  name: shifted lnorm; subtract min before fitting
- when: shape2 tends to infinity with shape1*shape2 positive and fixed, and shape3 and scale fixed
  firmness: proved
  status: ParetoBoundary
  name: shifted pareto; subtract min before fitting
- when: shape2 tends to infinity with shape3*shape2 positive and fixed, and shape1 and scale fixed
  firmness: proved
  status: PowerBoundary
  name: shifted power; subtract min before fitting
- name: shifted double Pareto; subtract min before fitting
  firmness: proved
  when: shape2 tends to infinity with shape1*shape2 and shape3*shape2 positive and fixed, and scale fixed
  status: DoubleParetoBoundary
  message: GB2 selects the double Pareto limit
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
  - shape3
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - shape3
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape2
  - shape3
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - shape2
  - shape3
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - scale
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - scale
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape2
  - scale
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - shape2
  - scale
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape3
  - scale
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - shape3
  - scale
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape2
  - shape3
  - scale
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - shape2
  - shape3
  - scale
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
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
  note: 'fpareto/README.md, Details; fpareto/mle.hpp (public fitter and held overloads); fpareto/test.cpp: With
    threshold and beta shapes held, the GB2 delegate has a strictly concave inner power/location problem; joint
    beta-shape search and face enumeration remain numerical.'
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
---

This family has a known support endpoint and delegates fitting to gb2.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-28-gb2.md) describes the limiting paths and the scope of the boundary comparisons.

The 2026-10-31 GB2 member-seed subsampling for samples larger than 1000
also applies through this delegate when all four GB2 coefficients are free.
The held-threshold shift, validation and full-data final checks are unchanged;
see [GB2](../gb2/README.md) and the
[paired evidence](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

Identification transfers from GB2 on x-min. Interior optimization and enumeration of competing faces are numerical, not a proof of global concavity. The delegate compares faces within 1e-8*n and reports unresolved searches explicitly. All-held fits evaluate the density rather than estimate free coefficients.

The GB2 coefficients are unchanged: shape2 is its power, shape3 its lower beta shape, and shape1 its upper beta shape.

Observations must be strictly inside the held support. Free endpoints with other parameters held are not supported; those subsets need separate existence studies. The delegate retains its sample restrictions and limit comparisons.

At distance y above the endpoint the density is asymptotic to C y^(a-1), where a is shape2*shape3. Choose 0<a<1 and hold other parameters positive. As min approaches the sample minimum from below, k minimum factors diverge as epsilon^(k*(a-1)); all remaining factors approach positive constants. Thus unrestricted likelihood is unbounded, including constant samples.

When the delegate selects a fixed-scale endpoint face, that face retains
the endpoint min + scale; a free-endpoint refit would be a different model.
Some held-scale searches remain unresolved, including deterministic Pareto
and power samples; these stop without returning a fitted model.
