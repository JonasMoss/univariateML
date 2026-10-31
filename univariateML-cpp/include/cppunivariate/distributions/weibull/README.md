---
id: weibull
name: Weibull
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  symbol: 'k'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: '\lambda'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: stats::dweibull
  sampler: stats::rweibull
  control:
    shape: Optional held shape; NULL estimates it.
    scale: Optional held scale; NULL estimates it.
    shape0: Optional positive finite starting shape; `NULL` uses the automatic start.
    reltol: Relative step tolerance in log(sigma) of the underlying Gumbel fit.
    iterlim: Maximum number of iterations.
log_density: log(shape) - log(scale) + (shape - 1)*log(x/scale) - (x/scale)**shape
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 21.
  Wiley, New York.
limits:
- name: unbounded likelihood
  fixed: [scale]
  when: Every observation equals held scale.
  status: Unbounded
  message: unbounded
examples: |
  x <- datasets::precip
  mlweibull(x)

group: gamma and Weibull
construction:
- base
relations:
  special_case_of:
  - family: trgamma
    at:
      shape1: 1
      shape2: shape
      scale: scale
fit:
  method: delegation
  certified: 'yes'
  note: 'weibull/README.md, Details; weibull/mle.hpp (public fitter and held overloads); weibull/test.cpp: Gumbel
    location is profiled exactly and its scale equation has a unique root; held location has a strictly concave
    inverse-scale likelihood. Log/reciprocal transformations preserve this structure.'
areas:
- reliability and survival
- waiting times and queues
classification: classical
classification_reason: A standard model for reliability lifetimes with a power-shaped hazard.
summary: Weibull provides a model for reliability lifetimes with a power-shaped hazard.
aliases: []
key_references:
- 1
---

The Weibull distribution is used for lifetimes and reliability data.

## Details

The optional `shape0` start is functional here; it was documented but ignored in univariateML 1.x.

With scale held, the likelihood is unbounded exactly when every observation equals that scale. Holding shape permits constant data.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
