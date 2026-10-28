---
id: logis
name: Logistic
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: location, role: location, domain: "(-inf, inf)", default: 0.0}
  - {name: scale, role: scale, domain: "(0, inf)", default: 1.0}
r:
  density: stats::dlogis
  sampler: stats::rlogis
  control:
    abstol: Absolute step tolerance in (location, log(scale)).
    iterlim: Maximum number of iterations.
log_density: -(x-location)/scale - log(scale) - 2*log(1 + exp(-(x-location)/scale))
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 23. Wiley, New York."
examples: |
  mllogis(precip)
---

The logistic distribution is a symmetric location-scale distribution with heavier tails than the normal distribution.

## Details

The location is unrestricted and the scale is positive.
