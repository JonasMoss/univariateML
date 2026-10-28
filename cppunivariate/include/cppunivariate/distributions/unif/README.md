---
id: unif
name: Uniform
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: min, role: lower, domain: "(-inf, max)", default: 0.0}
  - {name: max, role: upper, domain: "(min, inf)", default: 1.0}
r:
  density: stats::dunif
  sampler: stats::runif
  control: {}
log_density: -log(max-min)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 26. Wiley, New York."
examples: |
  mlunif(precip)
---

The uniform distribution assigns constant density to a bounded interval.

## Details

The endpoint estimates are `min(x)` and `max(x)`, so the MLE lies on the boundary determined by the sample order statistics. The fitted support is the closed interval between these endpoints, which must be distinct.
