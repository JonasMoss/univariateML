---
id: laplace
name: Laplace
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mu, role: location, domain: "(-inf, inf)", default: 0.0}
  - {name: sigma, role: scale, domain: "(0, inf)", default: 1.0}
r:
  density: extraDistr::dlaplace
  sampler: extraDistr::rlaplace
  control: {}
log_density: -log(2*sigma) - Abs(x-mu)/sigma
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 24. Wiley, New York."
examples: |
  mllaplace(precip)
---

The Laplace (double exponential) distribution is a symmetric location-scale model with exponential tails.

## Details

The location estimate is a sample median and the scale estimate is the mean absolute deviation from it. For even sample size, every point between the two central order statistics maximises the location likelihood; this fitter returns the upper endpoint of that interval.
