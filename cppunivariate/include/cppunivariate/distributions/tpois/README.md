---
id: tpois
name: Truncated Poisson
support: {type: integer, lower: "max(0, floor(a)+1)", upper: "floor(b)", closed: [true, true]}
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
  density: dtpois
  sampler: rtpois
  control:
    lambda: Optional fixed parameter; NULL estimates it.
    a: "Strict lower bound; NULL estimates it as min(x) - 1."
    b: "Inclusive upper bound; NULL estimates it as max(x)."

log_density: "x*log(lambda)-loggamma(x+1)-log(Sum(exp(k*log(lambda)-loggamma(k+1)), (k, Max(0, floor(a)+1), floor(b))))"
limits:
- name: point mass at lowest support point
  when: '$\lambda \to 0$'
  detect: all observations at a strictly positive lowest support point
  status: LowerPointMass
  fixed: [b]
- name: point mass at highest support point
  when: '$\lambda \to \infty$'
  detect: all observations at finite highest support point
  status: UpperPointMass
  fixed: [a]
examples: |
  set.seed(1)
  mltpois(univariateML:::rtpois(n = 100, lambda = 3, a = 0, b = 10), a = 0, b = 10)
holds: []
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

For `a = 0, b = Inf`, interior fits agree with `mlztpois`. The zero-rate
conditioning limit is defined as an attained member only for `mlztpois`. Thus
`mlztpois` retains its attained boundary while `mltpois` reports the point-mass escape.

With a free bound, the conditioning support tightens to the corresponding sample extreme. The strict lower bound is nonunique in `[min(x) - 1, min(x))`; the returned representative is `min(x) - 1`. On singleton support every positive rate maximizes the likelihood; a free rate is represented by one. For nonsingleton support the empirical mean must lie strictly between the effective integer bounds, except that a zero mean attains rate zero when zero is in the support. Otherwise the fitter stops at the lower or upper point-mass limit.
