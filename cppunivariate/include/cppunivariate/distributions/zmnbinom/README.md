---
id: zmnbinom
name: Zero-modified negative binomial
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
  default: 2.0
  fixable: true
- name: prob
  role: probability
  domain: (0, 1]
  default: 0.4
- name: p0
  role: probability
  domain: '[0, 1]'
  default: 0.3
r:
  density: actuar::dzmnbinom
  sampler: actuar::rzmnbinom
  control:
    size: Optional fixed size; NULL estimates size.
    reltol: Tolerance for scalar profile search.
    iterlim: Maximum scalar search iterations.
log_density: Piecewise((log(p0), Eq(x, 0)), (log(1-p0)+(loggamma(size+x)-loggamma(size)-loggamma(x+1)+size*log(prob)+x*log(1-prob)-log(1-prob**size)), True))
limits:
- model: zmpois
  when: $size \to \infty$ with the positive conditional mean fixed
  detect: the maximised face likelihood wins within 1e-8 times the sample size
  status: PoissonLimit
---

Zero-modified negative binomial distributions model counts with a separate zero component.

## Details

Actuar defines `size = 0` as the logarithmic series with `theta = 1 - prob`; this attained member is returned. Fixed size may be zero.
With no positives the fitter returns `p0 = 1`, keeps a supplied size and picks conventional base parameters with a warning. With no zeros it returns `p0 = 0`. Positive observations all equal to one give an attained two-point law; the fitter warns that `mlbinom(x, size = 1)` fits it exactly.
Holding size fixed removes the infinite-size escape. The point mass at one remains attained for every fixed size.
