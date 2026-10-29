---
id: zib
name: Zero-inflated binomial
support:
  type: integer
  lower: 0
  upper: inf
  closed:
  - true
  - false
params:
- name: size
  role: trials
  domain: '[0, inf)'
  default: 8.0
  fixable: true
  integer: true
- name: prob
  role: probability
  domain: '[0, 1]'
  default: 0.4
- name: pi
  role: probability
  domain: '[0, 1]'
  default: 0.4
r:
  density: extraDistr::dzib
  sampler: extraDistr::rzib
  control:
    size: Optional fixed size; NULL estimates size.
    reltol: Tolerance for scalar profile search.
    iterlim: Maximum scalar search iterations.
log_density: Piecewise((log(pi+(1-pi)*(1-prob)**size), Eq(x,0)), (log(1-pi)+loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)+x*log(prob)+(size-x)*log(1-prob), True))
limits:
- model: zip
  when: $size \to \infty$ with the positive conditional mean fixed
  detect: the maximised face likelihood wins within 1e-8 times the sample size
  status: ZipLimit
---

Zero-inflated binomial distributions model counts with a separate zero component.

## Details

Holding size fixed removes the infinite-size escape. The binomial face remains attained; all-zero data return `prob = pi = 0`.
Fixed `size = 0` is the attained point mass at zero and requires all-zero data.
