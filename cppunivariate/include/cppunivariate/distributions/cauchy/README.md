---
id: cauchy
name: Cauchy
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: location, role: location, domain: "(-inf, inf)", default: 0.0}
  - {name: scale, role: scale, domain: "(0, inf)", default: 1.0}
r:
  density: stats::dcauchy
  sampler: stats::rcauchy
  control:
    abstol: Absolute step tolerance in (location, log(scale)).
    iterlim: Maximum number of iterations.
log_density: -log(pi) - log(scale) - log(1 + ((x-location)/scale)**2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 16. Wiley, New York."
examples: |
  mlcauchy(airquality$Temp)
---

The Cauchy distribution is a symmetric location-scale distribution with heavy tails.

## Details

No maximum likelihood estimator exists when a value has at least half the sample mass, except for two distinct values with equal multiplicities. In that exception the maximum is attained and nonunique; the fitter returns the midpoint and half the distance as location and scale. Other such samples report `NoMLE`.
