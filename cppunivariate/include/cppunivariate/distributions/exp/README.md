---
id: exp
name: Exponential
support: {type: real, lower: 0.0, upper: inf, closed: [true, false]}
params:
- name: rate
  role: rate
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: stats::dexp
  sampler: stats::rexp
  control: {}
log_density: log(rate) - rate*x
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 19. Wiley, New York."
examples: |
  mlexp(precip)
holds:
- held:
  - rate
  status: not-implemented
  reason: The adapter does not accept `rate`; this hold is not implemented (P2).
---

The exponential distribution models nonnegative waiting times with a constant hazard rate.

## Details

The rate estimate is the inverse sample mean.
