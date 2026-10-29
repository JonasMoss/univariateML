---
id: zmpois
name: Zero-modified Poisson
support:
  type: integer
  lower: 0
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
- name: p0
  role: probability
  domain: '[0, 1]'
  simulate: 0.3
  kind: regular
r:
  density: actuar::dzmpois
  sampler: actuar::rzmpois
  control: {}
log_density: Piecewise((log(p0), Eq(x, 0)), (log(1-p0)+(-lambda+x*log(lambda)-loggamma(x+1)-log(1-exp(-lambda))), True))
holds:
- held:
  - lambda
  status: not-implemented
  reason: The adapter does not accept `lambda`; this hold is not implemented (P2).
- held:
  - p0
  status: not-implemented
  reason: The adapter does not accept `p0`; this hold is not implemented (P2).
---

Zero-modified Poisson distributions model counts with a separate zero component.

## Details

With no positives the fitter returns `p0 = 1` and conventional base parameters with a warning. With no zeros it returns `p0 = 0`. Positive observations all equal to one give an attained two-point law; the fitter warns that `mlbinom(x, size = 1)` fits it exactly.
