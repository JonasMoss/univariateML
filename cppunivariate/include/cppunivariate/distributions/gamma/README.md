---
id: gamma
name: Gamma
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
  - {name: shape, role: shape, domain: "(0, inf)", default: 2.0}
  - {name: rate, role: rate, domain: "(0, inf)", default: 2.0}
r:
  density: stats::dgamma
  sampler: stats::rgamma
  control:
    abstol: Absolute step tolerance in shape.
    iterlim: Maximum number of iterations.
log_density: shape*log(rate) - loggamma(shape) + (shape - 1)*log(x) - rate*x
references:
  - "Choi, S. C, and R. Wette. \"Maximum likelihood estimation of the parameters of the gamma distribution and their bias.\" Technometrics 11.4 (1969): 683-690."
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 17. Wiley, New York."
examples: |
  mlgamma(precip)
---

The gamma distribution models positive measurements and waiting times.

## Details

The parameterisation uses `rate`, the reciprocal of the gamma scale.
