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
  simulate: 0.4
  kind: regular
- name: p0
  role: probability
  domain: '[0, 1]'
  simulate: 0.3
  kind: regular
r:
  density: dzmlogarithmic
  sampler: rzmlogarithmic
  control:
    prob: "Optional held prob; NULL estimates it."
    p0: "Optional held p0; NULL estimates it."
log_density: Piecewise((log(p0), Eq(x, 0)), (log(1-p0)+x*log(prob)-log(x)-log(-log(1-prob)), True))
---

Zero-modified logarithmic distributions model counts with a separate zero component.

## Details

With no positives the fitter returns `p0 = 1` and conventional base parameters with a warning. With no zeros it returns `p0 = 0`. Positive observations all equal to one give an attained two-point law; the fitter warns that `mlbinom(x, size = 1)` fits it exactly.
Actuar defines `prob = 0` as the positive point mass at one.

The zero-indicator likelihood and the positive conditional likelihood factorize. Holding `p0`, including its empirical optimum, leaves the positive conditional fit unchanged. Held `p0 = 0` with zeros or `p0 = 1` with positives is incompatible and stops. With no positives, base parameters are unidentified; supplied values are preserved and free base parameters use conventional representatives.
