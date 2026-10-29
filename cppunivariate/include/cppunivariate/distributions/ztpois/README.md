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
  default: 2.0
r:
  density: actuar::dztpois
  sampler: actuar::rztpois
  control: {}
log_density: -lambda+x*log(lambda)-loggamma(x+1)-log(1-exp(-lambda))
---

Zero-truncated Poisson distributions model positive counts.

## Details

Actuar defines the point mass at one at `lambda = 0`. This boundary is returned as an attained member.
