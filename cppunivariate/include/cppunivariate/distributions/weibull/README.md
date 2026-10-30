---
id: weibull
name: Weibull
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
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
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 21. Wiley, New York."
limits:
- name: unbounded likelihood
  fixed: [scale]
  when: Every observation equals held scale.
  status: Unbounded
  message: unbounded
examples: |
  x <- datasets::precip
  mlweibull(x)
---

The Weibull distribution is used for lifetimes and reliability data.

## Details

The optional `shape0` start is functional here; it was documented but ignored in univariateML 1.x.

With scale held, the likelihood is unbounded exactly when every observation equals that scale. Holding shape permits constant data.
