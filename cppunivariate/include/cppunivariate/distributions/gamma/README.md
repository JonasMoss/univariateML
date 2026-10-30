---
id: gamma
name: Gamma
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  role: shape
  domain: (0, inf)
  simulate: 2.0
  kind: regular
- name: scale
  role: scale
  domain: (0, inf)
  simulate: 0.5
  kind: regular
r:
  density: stats::dgamma
  sampler: stats::rgamma
  control:
    shape: Optional held shape; NULL estimates it.
    scale: Optional held scale; NULL estimates it.
    abstol: Absolute step tolerance in shape.
    iterlim: Maximum number of iterations.
log_density: -shape*log(scale) - loggamma(shape) + (shape - 1)*log(x) - x/scale
references:
  - "Choi, S. C, and R. Wette. \"Maximum likelihood estimation of the parameters of the gamma distribution and their bias.\" Technometrics 11.4 (1969): 683-690."
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 17. Wiley, New York."
examples: |
  mlgamma(precip)
---

The gamma distribution models positive measurements and waiting times.

## Details

Holding either shape or scale gives a unique finite fit for any positive sample, including constant data. With both free, constant data have unbounded likelihood.
