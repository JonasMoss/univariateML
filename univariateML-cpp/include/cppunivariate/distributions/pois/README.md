---
id: pois
name: Poisson
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: stats::dpois
  sampler: stats::rpois
  control:
    lambda: "Optional held lambda; NULL estimates it."
log_density: "x*log(lambda) - lambda - loggamma(x + 1)"
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  x <- datasets::discoveries
  mlpois(x)

group: Poisson, binomial and negative binomial
construction:
- base
relations: {}
fit:
  method: closed-form
  certified: unknown
  note: Primary route from the 2026-10-30 fitter inventory; guarantees across held subsets are not classified.
areas:
- counts and ecology
classification: classical
classification_reason: A standard distribution in introductory probability or applied statistical modelling.
---

The Poisson distribution models nonnegative counts with a single rate parameter.

## Details

All-zero data give the boundary estimate `lambda = 0`, a point mass at zero.

Supplying `lambda` evaluates the likelihood at that value. Held values outside the parameter domain or incompatible with the observed counts stop.
