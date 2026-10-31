---
id: rayleigh
name: Rayleigh
support: {type: real, lower: 0.0, upper: inf, closed: [true, false]}
params:
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: drayleigh
  sampler: rrayleigh
  control:
    sigma: Optional held sigma; NULL estimates it.
log_density: log(x) - 2*log(sigma) - x**2/(2*sigma**2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 18. Wiley, New York."
examples: |
  set.seed(1)
  x <- rml(200, mlrayleigh())
  mlrayleigh(x)

group: gamma and Weibull
construction:
- base
relations:
  special_case_of:
  - family: weibull
    at:
      shape: 2
      scale: sqrt(2)*sigma
fit:
  method: closed-form
  certified: unknown
  note: Primary route from the 2026-10-30 fitter inventory; guarantees across held subsets are not classified.
areas:
- reliability and survival
- waiting times and queues
classification: classical
classification_reason: A standard distribution in introductory probability or applied statistical modelling.
---

The Rayleigh distribution describes the magnitude of a pair of independent centred normal variables with a common scale.

## Details

An observation at zero makes the likelihood identically zero for every scale, so the scale is not identifiable.

The scale estimate is the square root of half the empirical second moment.

A held positive sigma evaluates the likelihood without estimation. Any zero observation makes the likelihood identically zero for every scale and stops.
