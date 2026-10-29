---
id: ztnbinom
name: Zero-truncated negative binomial
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
  domain: '[0, inf)'
  default: 2.0
  fixable: true
- name: prob
  role: probability
  domain: (0, 1]
  default: 0.4
r:
  density: actuar::dztnbinom
  sampler: actuar::rztnbinom
  control:
    size: Optional fixed size; NULL estimates size.
    reltol: Tolerance for scalar profile search.
    iterlim: Maximum scalar search iterations.
log_density: loggamma(size+x)-loggamma(size)-loggamma(x+1)+size*log(prob)+x*log(1-prob)-log(1-prob**size)
limits:
- model: ztpois
  when: $size \to \infty$ with the positive conditional mean fixed
  detect: the maximised face likelihood wins within 1e-8 times the sample size
  status: PoissonLimit
---

Zero-truncated negative binomial distributions model positive counts.

## Details

Actuar defines the point mass at one at `prob = 1`. This boundary is returned as an attained member.
Actuar defines `size = 0` as the logarithmic series with `theta = 1 - prob`; this attained member is returned. Fixed size may be zero.
Holding size fixed removes the infinite-size escape. The point mass at one remains attained for every fixed size.
