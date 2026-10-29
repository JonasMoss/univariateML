---
id: unif
name: Uniform
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: min
  role: lower
  domain: (-inf, max)
  simulate: 0.0
  bound: lower
  kind: endpoint
- name: max
  role: upper
  domain: (min, inf)
  simulate: 1.0
  bound: upper
  kind: endpoint
r:
  density: stats::dunif
  sampler: stats::runif
  control: {}
log_density: -log(max-min)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 26. Wiley, New York."
examples: |
  mlunif(precip)
holds:
- held:
  - min
  status: not-implemented
  reason: The adapter does not accept `min`; this hold is not implemented (P2).
- held:
  - max
  status: not-implemented
  reason: The adapter does not accept `max`; this hold is not implemented (P2).
---

The uniform distribution assigns constant density to a bounded interval.

## Details

The endpoint estimates are `min(x)` and `max(x)`, so the MLE lies on the boundary determined by the sample order statistics. The fitted support is the closed interval between these endpoints, which must be distinct.
