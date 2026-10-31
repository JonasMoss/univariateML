---
id: gumbel
name: Gumbel
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: regular
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dgumbel
  sampler: rgumbel
  control:
    sigma: Optional held sigma; NULL estimates it.
    mu: Optional held mu; NULL estimates it.
    reltol: Relative step tolerance in log(sigma) of the underlying Gumbel fit.
    iterlim: Maximum number of iterations.
log_density: -log(sigma) - (x-mu)/sigma - exp(-(x-mu)/sigma)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 22. Wiley, New York."
holds: []
limits:
  - name: scale collapse
    when: Scale tends to zero at the fitted or held location.
    detect: Every observation equals the fitted or held location.
    status: Unbounded
    message: "unbounded"
examples: |
  set.seed(1)
  x <- rml(200, mlgumbel())
  mlgumbel(x)

group: extreme value
construction:
- base
relations:
  special_case_of:
  - family: gev
    at:
      mu: mu
      sigma: sigma
      xi: 0
fit:
  method: profile-search
  certified: unknown
  note: Primary route from the 2026-10-30 fitter inventory; guarantees across held subsets are not classified.
areas:
- extremes and environment
classification: classical
classification_reason: A standard distribution in introductory probability or applied statistical modelling.
---

The Gumbel distribution models maxima and extreme measurements.

## Details

This is the Gumbel distribution for maxima; `sigma` is positive.


Holding mu gives a unique inverse-scale maximum unless every observation equals that mu. Holding sigma gives an exact location profile and permits constant samples.
