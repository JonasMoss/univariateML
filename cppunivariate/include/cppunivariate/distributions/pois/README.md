---
id: pois
name: Poisson
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
  - {name: lambda, role: rate, domain: "[0, inf)", default: 2.0}
r:
  density: stats::dpois
  sampler: stats::rpois
  control: {}
log_density: "x*log(lambda) - lambda - loggamma(x + 1)"
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  mlpois(corbet)
---

The Poisson distribution models nonnegative counts with a single rate parameter.

## Details

All-zero data give the boundary estimate `lambda = 0`, a point mass at zero.
