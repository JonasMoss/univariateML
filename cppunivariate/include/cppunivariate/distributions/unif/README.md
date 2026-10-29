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
  control:
    min: Optional held min; `NULL` estimates it.
    max: Optional held max; `NULL` estimates it.
log_density: -log(max-min)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 26. Wiley, New York."
examples: |
  mlunif(precip)
holds: []
limits:
- name: zero-width collapse
  when: Free interval width tends to zero.
  detect: Constant data and no held endpoint strictly outside the data.
  status: Unbounded
  message: does not exist
- name: incompatible held endpoints
  when: A held endpoint excludes observations or min is not below max.
  detect: Held min exceeds the sample minimum or held max is below the sample maximum, or min >= max.
  status: Incompatible
  message: held uniform
---

The uniform distribution assigns constant density to a bounded interval.

## Details

The endpoint estimates are `min(x)` and `max(x)`, so the MLE lies on the boundary determined by the sample order statistics. The fitted support is the closed interval between these endpoints, which must be distinct.

A free endpoint is its sample extreme; a held endpoint must contain every observation. A constant sample has a finite maximum exactly when a held endpoint lies strictly outside the observation. Both-held intervals must have positive width and contain the sample; otherwise fitting stops.
