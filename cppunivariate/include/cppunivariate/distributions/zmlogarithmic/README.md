---
id: zmlogarithmic
name: Zero-modified logarithmic
support:
  type: integer
  lower: 0
  upper: inf
  closed:
  - true
  - false
params:
- name: prob
  role: probability
  domain: '[0, 1)'
  default: 0.4
- name: p0
  role: probability
  domain: '[0, 1]'
  default: 0.3
r:
  density: actuar::dzmlogarithmic
  sampler: actuar::rzmlogarithmic
  control: {}
log_density: Piecewise((log(p0), Eq(x, 0)), (log(1-p0)+x*log(prob)-log(x)-log(-log(1-prob)), True))
---

Zero-modified logarithmic distributions model counts with a separate zero component.

## Details

With no positives the fitter returns `p0 = 1` and conventional base parameters with a warning. With no zeros it returns `p0 = 0`. Positive observations all equal to one give an attained two-point law; the fitter warns that `mlbinom(x, size = 1)` fits it exactly.
Actuar defines `prob = 0` as the positive point mass at one.
