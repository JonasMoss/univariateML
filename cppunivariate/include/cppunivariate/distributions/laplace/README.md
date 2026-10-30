---
id: laplace
name: Laplace
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mu
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sigma
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: dlaplace
  sampler: rlaplace
  control:
    sigma: Optional held sigma; NULL estimates it.
    mu: Optional held mu; NULL estimates it.
log_density: -log(2*sigma) - Abs(x-mu)/sigma
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 24. Wiley, New York."
examples: |
  mllaplace(precip)
holds: []
limits:
  - name: scale collapse
    when: Scale tends to zero at the fitted or held location.
    detect: Every observation equals the fitted or held location.
    status: Unbounded
    message: "does not exist"
---

The Laplace (double exponential) distribution is a symmetric location-scale model with exponential tails.

## Details

The location estimate is a sample median and the scale estimate is the mean absolute deviation from it. For even sample size, every point between the two central order statistics maximises the location likelihood; this fitter returns the upper endpoint of that interval.


With mu held, sigma is the mean absolute residual about that value. Scale collapses only when every observation equals the held mu. Holding sigma permits constant samples.
