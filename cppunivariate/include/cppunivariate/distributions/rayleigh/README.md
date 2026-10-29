---
id: rayleigh
name: Rayleigh
support: {type: real, lower: 0.0, upper: inf, closed: [true, false]}
params:
- name: sigma
  role: scale
  domain: (0, inf)
  simulate: 2.0
  kind: regular
r:
  density: extraDistr::drayleigh
  sampler: extraDistr::rrayleigh
  control: {}
log_density: log(x) - 2*log(sigma) - x**2/(2*sigma**2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 18. Wiley, New York."
examples: |
  mlrayleigh(precip)
holds:
- held:
  - sigma
  status: not-implemented
  reason: The adapter does not accept `sigma`; this hold is not implemented (P2).
---

The Rayleigh distribution describes the magnitude of a pair of independent centred normal variables with a common scale.

## Details

The scale estimate is the square root of half the empirical second moment.
