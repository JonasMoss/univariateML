---
id: invparalogis
name: Inverse paralogistic
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: '\theta'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dinvparalogis
  sampler: rinvparalogis
  control:
    shape: Known positive finite coefficient; NULL estimates it.
    shape0: Optional positive finite starting value; NULL uses the automatic start.
    gradtol: Gradient tolerance for the underlying paralogistic fit on median-normalized reciprocals; with fixed scale, tolerance on the mean log-shape score (at most 1e-10).
    iterlim: Maximum number of iterations.
limits:
  - name: unbounded likelihood
    when: A free concentration can collapse on constant observations
    detect: Constant samples; the held coefficients determine whether concentration remains free
    status: InvalidStart
    message: unbounded
log_density: 2*log(shape) + shape*log(scale/x) - log(x) - (shape+1)*log(1+(scale/x)**shape)
references:
  - "Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley."
  - "Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37."
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlinvparalogis())
  mlinvparalogis(x)

group: beta and generalised beta
construction:
- base
relations:
  special_case_of:
  - family: invburr
    at:
      shape1: shape
      shape2: shape
      scale: scale
fit:
  method: delegation
  certified: unknown
  note: Primary route from the 2026-10-30 fitter inventory; guarantees across held subsets are not classified.
areas:
- actuarial
- economics and income
classification: uncommon
classification_reason: A less routinely used extension within this catalog; prevalence has not been independently
  surveyed.
---

The inverse paralogistic distribution models positive heavy-tailed data.

## Details

The fit is the reciprocal of the paralogistic fit on `1/x`, with a held scale mapped to its reciprocal.
