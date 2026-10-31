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
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
examples: |
  x <- datasets::discoveries
  mlpois(x)

group: Poisson, binomial and negative binomial
construction:
- base
relations: {}
fit:
  method: closed-form
  certified: 'yes'
  note: 'pois/README.md, Details; pois/mle.hpp (public fitter and held overloads); pois/test.cpp: Closed-form maximum
    for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- counts and ecology
classification: classical
classification_reason: A standard model for independent event counts at constant intensity.
summary: Poisson provides a model for independent event counts at constant intensity.
aliases: []
key_references:
- 1
---

The Poisson distribution models nonnegative counts with a single rate parameter.

## Details

All-zero data give the boundary estimate `lambda = 0`, a point mass at zero.

Supplying `lambda` evaluates the likelihood at that value. Held values outside the parameter domain or incompatible with the observed counts stop.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
