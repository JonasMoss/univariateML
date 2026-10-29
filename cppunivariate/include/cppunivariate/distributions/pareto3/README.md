---
id: pareto3
name: Pareto type III
support: {type: real, lower: "min", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "if (shape < 1) Inf else if (shape > 1) -Inf else -log(scale)"}}
params:
- name: shape
  role: shape
  domain: (0, inf)
  simulate: 2
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
    at min when shape < 1, so let min rise to the sample minimum. Hold it, e.g. `mlpareto3(x,
    min = 0)`, which is `mlllogis(x)`. Fits with min free and other parameters held
    are not supported.'
  kind: threshold
r:
  density: actuar::dpareto3
  sampler: actuar::rpareto3
  control:
    min: Known finite lower endpoint.
    reltol: Delegate fitting tolerance.
    iterlim: Maximum delegate iterations.
    shape: Known positive finite coefficient; NULL estimates it.
    scale: Known positive finite coefficient; NULL estimates it.
limits:
  - name: unbounded likelihood
    when: A free concentration can collapse on constant observations
    detect: Constant samples; the held coefficients determine whether concentration remains free
    status: Unbounded
    message: unbounded
log_density: log(shape) + log((1/scale)) + (shape - 1)*log((1/scale)*(x-min)) - 2*log(1 + ((1/scale)*(x-min))**shape)
examples: |
  set.seed(3)
  mlpareto3(simulate_default("mlpareto3", 100), min = 0)
holds:
- held: []
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - scale
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape
  - scale
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
fit_args:
  min: 0
---

This family has a known support endpoint and delegates fitting to llogis.

## Details

For at least two finite, nonconstant observations strictly above min, the held-threshold MLE is unique and attained. On log(x-min), the logistic likelihood is strictly concave in inverse scale and standardized location. Constant samples have unbounded likelihood as log-logistic shape grows.

The delegate coefficients map as shape=shape and scale=1/rate.

Observations must be strictly inside the held support. Free endpoints with other parameters held are not supported; those subsets need separate existence studies. The delegate retains its sample restrictions and limit comparisons.

At distance y above the endpoint the density is asymptotic to C y^(a-1), where a is shape. Choose 0<a<1 and hold other parameters positive. As min approaches the sample minimum from below, k minimum factors diverge as epsilon^(k*(a-1)); all remaining factors approach positive constants. Thus unrestricted likelihood is unbounded, including constant samples.

With min and shape held the log-scale score has one root, including on constant samples. With min and scale held a free shape is unbounded only when all observations equal min + scale.
