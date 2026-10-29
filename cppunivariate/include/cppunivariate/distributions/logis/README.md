---
id: logis
name: Logistic
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
holds:
- held:
  - location
  status: not-implemented
  reason: The adapter does not accept `location`; this hold is not implemented (P2).
- held:
  - scale
  status: not-implemented
  reason: The adapter does not accept `scale`; this hold is not implemented (P2).
---

The logistic distribution is a symmetric location-scale distribution with heavier tails than the normal distribution.

## Details

The location is unrestricted and the scale is positive.
