---
id: gumbel
name: Gumbel
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mu, role: location, domain: "(-inf, inf)", default: 3.0}
  - {name: sigma, role: scale, domain: "(0, inf)", default: 3.0}
r:
  density: extraDistr::dgumbel
  sampler: extraDistr::rgumbel
  control:
    reltol: Relative step tolerance in log(sigma) of the underlying Gumbel fit.
    iterlim: Maximum number of iterations.
log_density: -log(sigma) - (x-mu)/sigma - exp(-(x-mu)/sigma)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 22. Wiley, New York."
examples: |
  mlgumbel(precip)
---

The Gumbel distribution models maxima and extreme measurements.

## Details

This is the Gumbel distribution for maxima; `sigma` is positive.
