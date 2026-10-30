---
id: logis
name: Logistic
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: location
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: scale
  symbol: 's'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: stats::dlogis
  sampler: stats::rlogis
  control:
    scale: Optional held scale; NULL estimates it.
    location: Optional held location; NULL estimates it.
    abstol: Absolute step tolerance in (location, log(scale)).
    iterlim: Maximum number of iterations.
log_density: -(x-location)/scale - log(scale) - 2*log(1 + exp(-(x-location)/scale))
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 23. Wiley, New York."
holds: []
limits:
  - name: scale collapse
    when: Scale tends to zero at the fitted or held location.
    detect: Every observation equals the fitted or held location.
    status: InvalidInput
    message: "unbounded"
examples: |
  set.seed(1)
  x <- rml(200, mllogis())
  mllogis(x)
---

The logistic distribution is a symmetric location-scale distribution with heavier tails than the normal distribution.

## Details

The location is unrestricted and the scale is positive.


Holding location gives a unique scale maximum unless every observation equals that location. Holding scale gives an attained location maximum, including for constant samples. Both conditional scores are solved by bisection.
