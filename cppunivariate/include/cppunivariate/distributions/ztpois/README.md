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
  simulate: 1
  kind: regular
r:
  density: dztpois
  sampler: rztpois
  control:
    lambda: "Optional held lambda; NULL estimates it."
log_density: -lambda+x*log(lambda)-loggamma(x+1)-log(1-exp(-lambda))
examples: |
  x <- corbet
  mlztpois(x)
---

Zero-truncated Poisson distributions model positive counts.

## Details

Actuar defines the point mass at one at `lambda = 0`. This boundary is returned as an attained member.

Supplying `lambda` evaluates the likelihood at that value. Held values outside the parameter domain or incompatible with the observed counts stop.
