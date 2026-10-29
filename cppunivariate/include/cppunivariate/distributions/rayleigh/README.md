---
id: rayleigh
name: Rayleigh
support: {type: real, lower: 0.0, upper: inf, closed: [true, false]}
params:
  - {name: sigma, role: scale, domain: "(0, inf)", default: 2.0}
r:
  density: extraDistr::drayleigh
  sampler: extraDistr::rrayleigh
  control: {}
log_density: log(x) - 2*log(sigma) - x**2/(2*sigma**2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 18. Wiley, New York."
examples: |
  mlrayleigh(precip)
---

The Rayleigh distribution describes the magnitude of a pair of independent centred normal variables with a common scale.

## Details

An observation at zero makes the likelihood identically zero for every scale, so the scale is not identifiable.

The scale estimate is the square root of half the empirical second moment.
