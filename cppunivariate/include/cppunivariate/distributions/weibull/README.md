---
id: weibull
name: Weibull
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
  - {name: shape, role: shape, domain: "(0, inf)", default: 3.0}
  - {name: scale, role: scale, domain: "(0, inf)", default: 4.0}
r:
  density: stats::dweibull
  sampler: stats::rweibull
  control:
    shape0: Optional positive finite starting shape; `NULL` uses the automatic start.
    reltol: Relative step tolerance in log(sigma) of the underlying Gumbel fit.
    iterlim: Maximum number of iterations.
log_density: log(shape) - log(scale) + (shape - 1)*log(x/scale) - (x/scale)**shape
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 21. Wiley, New York."
examples: |
  mlweibull(precip)
---

The Weibull distribution is used for lifetimes and reliability data.

## Details

The optional `shape0` start is functional here; it was documented but ignored in univariateML 1.x.
