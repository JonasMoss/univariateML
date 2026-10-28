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

No maximum likelihood estimator exists for $n < 3$ or when a value is repeated at least $n/2$ times. The fitter reports `NoMLE` and stops with an error in these cases.
