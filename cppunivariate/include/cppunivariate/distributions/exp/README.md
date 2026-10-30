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
  control:
    rate: Optional held rate; NULL estimates it.
log_density: log(rate) - rate*x
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 19. Wiley, New York."
examples: |
  set.seed(1)
  x <- rml(200, mlexp())
  mlexp(x)
---

The exponential distribution models nonnegative waiting times with a constant hazard rate.

## Details

The rate estimate is the inverse sample mean.

A held positive rate gives a finite likelihood even for all-zero data. A free rate has unbounded likelihood for all-zero data.
