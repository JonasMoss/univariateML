---
id: zip
name: Zero-inflated Poisson
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: lambda
  role: rate
  domain: '[0, inf)'
  simulate: 1.0
  kind: regular
- name: pi
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: extraDistr::dzip
  sampler: extraDistr::rzip
  control: {}
log_density: "Piecewise((log(pi + (1 - pi)*exp(-lambda)), Eq(x, 0)), (log(1 - pi) - lambda + x*log(lambda) - loggamma(x + 1), True))"
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  mlzip(corbet)
holds:
- held:
  - lambda
  status: not-implemented
  reason: The adapter does not accept `lambda`; this hold is not implemented (P2).
- held:
  - pi
  status: not-implemented
  reason: The adapter does not accept `pi`; this hold is not implemented (P2).
---

The zero-inflated Poisson distribution mixes a Poisson count with an additional point mass at zero.

## Details

For zero-deflated data the estimate is `pi = 0`, the Poisson fit. All-zero data give a nonunique point-mass fit: the fitter warns and returns `lambda = 1`, `pi = 1`. These are boundary fits within the family.
