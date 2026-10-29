---
id: cauchy
name: Cauchy
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: location
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: scale
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: stats::dcauchy
  sampler: stats::rcauchy
  control:
    scale: Optional held scale; NULL estimates it.
    location: Optional held location; NULL estimates it.
    abstol: Absolute step tolerance in (location, log(scale)).
    iterlim: Maximum number of iterations.
log_density: -log(pi) - log(scale) - log(1 + ((x-location)/scale)**2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 16. Wiley, New York."
examples: |
  mlcauchy(airquality$Temp)
holds: []
limits:
  - name: scale collapse
    when: Scale tends to zero at the fitted or held location.
    detect: Excessive mass at that location or vanishing residual spread.
    status: NoMLE
    message: "does not exist"
---

The Cauchy distribution is a symmetric location-scale distribution with heavy tails.

## Details

No maximum likelihood estimator exists when a value has at least half the sample mass, except for two distinct values with equal multiplicities. In that exception the maximum is attained and nonunique; the fitter returns the midpoint and half the distance as location and scale. Other such samples report `NoMLE`.


With location held and scale free, let k count observations equal to that location. A finite scale maximum exists precisely when 2k < n; at equality the collapse supremum is not attained. Holding scale removes collapse. The conditional location likelihood can be multimodal and is searched across the observation gaps.
