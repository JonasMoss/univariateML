---
id: pareto2
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape > 0, scale > 0 and min; min held; shape and/or scale free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support"
name: Pareto type II
support: {type: real, lower: "min", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "log(shape) - log(scale)"}}
params:
- name: shape
  symbol: '\alpha'
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
  advice: 'With min estimated the likelihood is unbounded: let min rise to the sample
    minimum with scale equal to the gap and shape below k/(n - k), k the multiplicity
    of the minimum. Hold it, e.g. `mlpareto2(x, min = 0)`, which is `mllomax(x)`.
    Fits with min free and other parameters held are not supported.'
  kind: threshold
r:
  density: dpareto2
  sampler: rpareto2
  control:
    min: Known finite lower endpoint.
    reltol: Delegate fitting tolerance.
    iterlim: Maximum delegate iterations.
    shape: Known positive finite coefficient; NULL estimates it.
    scale: Known positive finite coefficient; NULL estimates it.
log_density: log((1/scale)) + log(shape) - (shape + 1)*log(1 + (1/scale)*(x-min))
limits:
- when: $\kappa \to \infty$ with $\lambda \kappa$ fixed
  firmness: proved
  detect: max over 0 < u < ubar of the profile log-likelihood gap is at most 1e-8*n
  status: ExponentialLimit
  name: shifted exp; subtract min before fitting
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
  x <- rml(200, mlpareto2())
  mlpareto2(x, min = 0L)

group: Pareto and Lomax
construction:
- base
- generalisation
relations:
  special_case_of:
  - family: pareto4
    at:
      shape1: shape
      shape2: 1
      scale: scale
      min: min
  - family: gpd
    at:
      mu: min
      sigma: scale/shape
      xi: 1/shape
  transform_of:
  - family: lomax
    transform: shift
    at:
      kappa: shape
      lambda: 1/scale
    shift: min
fit:
  method: multistart
  certified: partial
  note: 'pareto2/README.md, Details; pareto2/mle.hpp (public fitter and held overloads); pareto2/test.cpp: Held
    threshold transfers to Lomax, log-logistic or Burr; conditional monotone subsets are certified but joint excess
    profiles inherit numerical-search qualifications. Free thresholds stop.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model thresholded income or loss sizes with a Lomax excess distribution.
summary: Pareto type II provides a model for thresholded income or loss sizes with a Lomax excess distribution.
aliases: []
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
---

This family has a known support endpoint and delegates fitting to lomax.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md) describes the limiting paths and the scope of the boundary comparisons.

With min held, identification and existence transfer exactly from Lomax on x-min. Its global profile search, exponential-face comparison and 1e-8*n tie rule are retained; the sample criterion is given in the mllomax documentation.

The delegate coefficients map as shape=kappa and scale=1/lambda.

Observations must be strictly inside the held support. Free endpoints with other parameters held are not supported; those subsets need separate existence studies. The delegate retains its sample restrictions and limit comparisons.

Let the minimum m occur k times. Along min = m-epsilon and scale = epsilon, each minimum density is proportional to epsilon^(-1), and each other density to epsilon^shape. Hence L is proportional to epsilon^(shape*(n-k)-k), which diverges for shape < k/(n-k), or for every shape when k=n.

With min held, holding scale gives the explicit shape estimate n / sum(log1p((x-min)/scale)). Holding shape gives a unique scale-score root.
