---
id: ztpois
name: Zero-truncated Poisson
support:
  type: integer
  lower: 1
  upper: inf
  closed:
  - true
  - false
params:
- name: lambda
  role: rate
  domain: '[0, inf)'
  simulate: 2.0
  kind: regular
r:
  density: actuar::dztpois
  sampler: actuar::rztpois
  control: {}
log_density: -lambda+x*log(lambda)-loggamma(x+1)-log(1-exp(-lambda))
holds:
- held:
  - lambda
  status: not-implemented
  reason: The adapter does not accept `lambda`; this hold is not implemented (P2).
---

Zero-truncated Poisson distributions model positive counts.

## Details

Actuar defines the point mass at one at `lambda = 0`. This boundary is returned as an attained member.
