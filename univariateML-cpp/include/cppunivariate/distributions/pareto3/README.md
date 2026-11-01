---
id: pareto3
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape > 0, scale > 0 and min; min held; shape and/or scale free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Pareto type III
support: {type: real, lower: "min", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "if (shape < 1) Inf else if (shape > 1) -Inf else -log(scale)"}}
params:
- name: shape
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
  advice: 'With min estimated the likelihood is unbounded: the density is singular
    at min when shape < 1, so let min rise to the sample minimum. Hold it, e.g. `mlpareto3(x,
    min = 0)`, which is `mlllogis(x)`. Fits with min free and other parameters held
    are not supported.'
  kind: threshold
r:
  density: dpareto3
  sampler: rpareto3
  control:
    min: Known finite lower endpoint.
    reltol: Delegate fitting tolerance.
    iterlim: Maximum delegate iterations.
    shape: Known positive finite coefficient; NULL estimates it.
    scale: Known positive finite coefficient; NULL estimates it.
limits:
  - name: unbounded likelihood
    firmness: proved
    when: A free concentration can collapse on constant observations
    detect: Constant samples; the held coefficients determine whether concentration remains free
    status: Unbounded
    message: unbounded
log_density: log(shape) + log((1/scale)) + (shape - 1)*log((1/scale)*(x-min)) - 2*log(1 + ((1/scale)*(x-min))**shape)
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
examples: |
  set.seed(1)
  x <- rml(200, mlpareto3())
  mlpareto3(x, min = 0L)

group: Pareto and Lomax
construction:
- base
- generalisation
relations:
  special_case_of:
  - family: pareto4
    at:
      shape1: 1
      shape2: shape
      scale: scale
      min: min
  transform_of:
  - family: llogis
    transform: shift
    at:
      shape: shape
      scale: scale
    shift: min
fit:
  method: multistart
  certified: partial
  note: 'pareto3/README.md, Details; pareto3/mle.hpp (public fitter and held overloads); pareto3/test.cpp: Default
    fit transfers to the concave log-logistic likelihood; held shape/scale routes delegate to Burr numerical machinery,
    so certification of every held subset is not established.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model thresholded sizes with a log-logistic excess distribution.
summary: Pareto type III provides a model for thresholded sizes with a log-logistic excess distribution.
aliases: []
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
---

This family has a known support endpoint and delegates fitting to llogis.

## Details

For a constant sample at $x>\min$, choose scale $x-\min$ and let
shape $a\to\infty$. The density at every observation is
$a/[4(x-\min)]$, so the likelihood diverges. This constructs a path
when shape and scale can take those values; holding a coefficient may
remove it.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md) describes the limiting paths and the scope of the boundary comparisons.

For at least two finite, nonconstant observations strictly above min, the held-threshold MLE is unique and attained. On log(x-min), the logistic likelihood is strictly concave in inverse scale and standardized location. Constant samples have unbounded likelihood as log-logistic shape grows.

The delegate coefficients map as shape=shape and scale=1/rate.

Observations must be strictly inside the held support. Free endpoints with other parameters held are not supported; those subsets need separate existence studies. The delegate retains its sample restrictions and limit comparisons.

At distance y above the endpoint the density is asymptotic to C y^(a-1), where a is shape. Choose 0<a<1 and hold other parameters positive. As min approaches the sample minimum from below, k minimum factors diverge as epsilon^(k*(a-1)); all remaining factors approach positive constants. Thus unrestricted likelihood is unbounded, including constant samples.

With min and shape held the log-scale score has one root, including on constant samples. With min and scale held a free shape is unbounded only when all observations equal min + scale.
