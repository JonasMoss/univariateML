---
id: ztbinom
name: Zero-truncated binomial
support:
  type: integer
  lower: 1
  upper: inf
  closed:
  - true
  - false
params:
- name: size
  role: trials
  domain: '[1, inf)'
  default: 8.0
  fixable: true
  integer: true
- name: prob
  role: probability
  domain: '[0, 1]'
  default: 0.4
r:
  density: actuar::dztbinom
  sampler: actuar::rztbinom
  control:
    size: Optional fixed size; NULL estimates size.
    reltol: Tolerance for scalar profile search.
    iterlim: Maximum scalar search iterations.
log_density: loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)+x*log(prob)+(size-x)*log(1-prob)-log(1-(1-prob)**size)
limits:
- model: ztpois
  when: $size \to \infty$ with the positive conditional mean fixed
  detect: the maximised face likelihood wins within 1e-8 times the sample size
  status: PoissonLimit
---

Zero-truncated binomial distributions model positive counts.

## Details

Actuar defines the point mass at one at `prob = 0`. This boundary is returned as an attained member.
Holding size fixed removes the infinite-size escape. The point mass at one remains attained for every fixed size.
