---
id: tpois
name: Truncated Poisson
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: lambda
  role: rate
  domain: '[0, inf)'
  simulate: 3
  kind: regular
- name: a
  role: known design quantity
  domain: (-inf, inf)
  simulate: 0
  kind: design
- name: b
  role: known design quantity
  domain: (-inf, inf)
  simulate: 10
  kind: design
r:
  density: extraDistr::dtpois
  sampler: extraDistr::rtpois
  control:
    lambda: Optional fixed parameter; NULL estimates it.
    a: Known strict lower truncation bound.
    b: Known inclusive upper truncation bound.

log_density: "x*log(lambda)-loggamma(x+1)-log(Sum(exp(k*log(lambda)-loggamma(k+1)), (k, Max(0, floor(a)+1), floor(b))))"
limits:
- name: point mass at lowest support point
  when: '$\lambda \to 0$'
  detect: all observations at a strictly positive lowest support point
  status: LowerPointMass
- name: point mass at highest support point
  when: '$\lambda \to \infty$'
  detect: all observations at finite highest support point
  status: UpperPointMass
examples: |
  set.seed(1)
  mltpois(extraDistr::rtpois(100, 3, 0, 10), a = 0, b = 10)

holds:
- held: []
  status: not-implemented
  reason: Estimating `a` with NULL is not implemented for this design.
- held:
  - lambda
  status: not-implemented
  reason: Estimating `a` with NULL is not implemented for this design.
- held:
  - a
  status: not-implemented
  reason: Estimating `b` with NULL is not implemented for this design.
- held:
  - lambda
  - a
  status: not-implemented
  reason: Estimating `b` with NULL is not implemented for this design.
- held:
  - b
  status: not-implemented
  reason: Estimating `a` with NULL is not implemented for this design.
- held:
  - lambda
  - b
  status: not-implemented
  reason: Estimating `a` with NULL is not implemented for this design.
fit_args:
  a: 0
  b: 10
---

The Poisson law conditioned on known bounds `a < x <= b` models truncated counts.

## Details

Bounds must be supplied explicitly to avoid silently ignoring the sampling design.
Untruncated bounds `a = -Inf, b = Inf` recover the ordinary Poisson interior fit.
The conditional mean is strictly increasing in the natural parameter, since its
derivative is the positive conditional variance. Thus every sample mean strictly
between the support edges has one global maximum. Edge-only data escape to a
point mass; a singleton support does not identify the parameter.
The normalizer uses mode-centred recurrences with a two-million-term budget per tail.

When the conditioning event includes zero, the point mass at zero is attained
at `lambda = 0`.
Otherwise the corresponding zero-probability conditioning limit is reported as an escape.

For `a = 0, b = Inf`, interior fits agree with `mlztpois`. Its actuar
reference additionally defines the point mass at one at zero rate; extraDistr
does not define that zero-probability conditioning event. Consequently
`mlztpois` retains its attained boundary while `mltpois` reports the point-mass escape.
