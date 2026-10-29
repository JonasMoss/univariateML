---
id: fpareto
name: Feller-Pareto
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: shape1
  role: shape
  domain: (0, inf)
  simulate: 0.3
  kind: regular
- name: shape2
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
- name: shape3
  role: shape
  domain: (0, inf)
  simulate: 0.4
  kind: regular
- name: scale
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: min
  role: lower endpoint
  domain: (-inf, inf)
  simulate: 0
  advice: 'With min estimated the likelihood is unbounded: the density is singular
    at min when shape2 * shape3 < 1, so let min rise to the sample minimum. Hold it,
    e.g. `mlfpareto(x, min = 0)`, which is `mlgb2(x)`. Fits with min free and other
    parameters held are not supported.'
  kind: threshold
r:
  density: actuar::dfpareto
  sampler: actuar::rfpareto
  control:
    min: Known finite lower endpoint.
    reltol: Delegate fitting tolerance.
    iterlim: Maximum delegate iterations.
log_density: log(shape2)-log((x-min))-loggamma(shape3)-loggamma(shape1)+loggamma(shape3+shape1)+shape3*shape2*log((x-min)/scale)-(shape3+shape1)*log(1+((x-min)/scale)**shape2)
limits:
- name: unbounded likelihood
  when: A free concentration can collapse on constant observations
  detect: Constant samples; the held coefficients determine whether concentration remains free
  status: InvalidData
  message: unbounded
- when: shape1 tends to infinity with rescaled scale
  status: TrGammaBoundary
  name: shifted trgamma; subtract min before fitting
- when: shape3 tends to infinity with rescaled scale
  status: InvTrGammaBoundary
  name: shifted invtrgamma; subtract min before fitting
- when: both beta shapes tend to infinity and shape2 tends to zero
  status: LognormalBoundary
  name: shifted lnorm; subtract min before fitting
- when: shape2 tends to infinity and shape1 tends to zero
  status: ParetoBoundary
  name: shifted pareto; subtract min before fitting
- when: shape2 tends to infinity and shape3 tends to zero
  status: PowerBoundary
  name: shifted power; subtract min before fitting
- name: shifted double Pareto; subtract min before fitting
  when: shape2 tends to infinity and both beta shapes tend to zero
  status: DoubleParetoBoundary
  message: GB2 selects the double Pareto limit
examples: |
  set.seed(1)
  mlfpareto(simulate_default("mlfpareto", 100), min = 0)
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
---

This family has a known support endpoint and delegates fitting to gb2.

## Details

Identification transfers from GB2 on x-min. Interior optimization and enumeration of competing faces are numerical, not a proof of global concavity. The delegate compares faces within 1e-8*n and reports unresolved searches explicitly. All-held fits evaluate the density rather than estimate free coefficients.

The GB2 coefficients are unchanged: shape2 is its power, shape3 its lower beta shape, and shape1 its upper beta shape.

Observations must be strictly inside the held support. Free endpoints with other parameters held are not supported; those subsets need separate existence studies. The delegate retains its sample restrictions and limit comparisons.

At distance y above the endpoint the density is asymptotic to C y^(a-1), where a is shape2*shape3. Choose 0<a<1 and hold other parameters positive. As min approaches the sample minimum from below, k minimum factors diverge as epsilon^(k*(a-1)); all remaining factors approach positive constants. Thus unrestricted likelihood is unbounded, including constant samples.

When the delegate selects a fixed-scale endpoint face, that face retains
the endpoint min + scale; a free-endpoint refit would be a different model.
Some held-scale searches remain unresolved, including deterministic Pareto
and power samples; these stop without returning a fitted model.
