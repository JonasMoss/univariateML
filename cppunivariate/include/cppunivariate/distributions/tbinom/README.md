---
id: tbinom
name: Truncated binomial
support: {type: integer, lower: "max(0, floor(a)+1)", upper: "min(size, floor(b))", closed: [true, true]}
params:
- name: prob
  role: probability
  domain: '[0, 1]'
  simulate: 0.4
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
- name: size
  role: known design quantity
  domain: '[0, inf)'
  integer: yes
  simulate: 12
  kind: design
r:
  density: dtbinom
  sampler: rtbinom
  control:
    prob: "Optional fixed parameter; NULL estimates it."
    a: "Required lower bound; NULL estimates it as min(x) - 1 (strict truncation)."
    b: "Required upper bound; NULL estimates it as max(x)."
    size: "Required number of trials; NULL estimates size when probability is held."
log_density: "x*log(prob)+(size-x)*log(1-prob)-loggamma(x+1)-loggamma(size-x+1)-log(Sum(exp(k*log(prob)+(size-k)*log(1-prob)-loggamma(k+1)-loggamma(size-k+1)), (k, Max(0, floor(a)+1), Min(size, floor(b)))))"
limits:
- name: point mass at lowest support point
  when: $prob \to 0$
  detect: all observations at a strictly positive lowest support point
  status: LowerPointMass
- name: point mass at highest support point
  when: $prob \to 1$
  detect: all observations at finite highest support point below size
  status: UpperPointMass
- name: point mass at highest support point
  when: $size \to \infty$ at a fixed interior probability
  detect: constant data at a finite upper support edge with nonsingleton support
  fixed:
  - prob
  status: UpperPointMass
examples: |
  set.seed(1)
  mltbinom(univariateML:::rtbinom(n = 100, prob = 0.4, a = 0, b = 10, size = 12), a = 0, b = 10, size = 12)
holds:
- held: []
  status: not-implemented
  reason: Free size with free probability awaits the P0 truncated-binomial existence proof.
- held:
  - a
  status: not-implemented
  reason: Free size with free probability awaits the P0 truncated-binomial existence proof.
- held:
  - b
  status: not-implemented
  reason: Free size with free probability awaits the P0 truncated-binomial existence proof.
- held:
  - a
  - b
  status: not-implemented
  reason: Free size with free probability awaits the P0 truncated-binomial existence proof.
fit_args:
  a: 0
  b: 10
  size: 12
---

The binomial law conditioned on known bounds `a < x <= b` models truncated counts.

## Details

Bounds must be supplied explicitly to avoid silently ignoring the sampling design.
Size is also required; `NULL` estimates it when probability is held, using a discrete profile with explicit tail bounds.
The conditional mean is strictly increasing in the natural parameter, since its
derivative is the positive conditional variance. Thus every sample mean strictly
between the support edges has one global maximum. Edge-only data escape to a
point mass; a singleton support does not identify the parameter.
The normalizer uses mode-centred recurrences with a two-million-term budget per tail.

When the conditioning event includes zero, the point mass at zero is attained
at `prob = 0`. When it includes `size`, the point mass there is attained at `prob = 1`.
Otherwise the corresponding zero-probability conditioning limit is reported as an escape.

At fixed size, free truncation bounds select the smallest conditioning support containing the sample: `a = min(x) - 1`, `b = max(x)`. The likelihood cannot decrease when unobserved support is removed. Singleton support attains likelihood one with any interior probability; an unspecified probability is returned as 0.5. A held boundary probability requires the conditioning event to have positive probability.

With probability held in (0,1), integer-size profiling bounds the entire tail by geometric ratios below a finite upper bound, or binomial stochastic ordering without an upper bound. A constant sample at a nonsingleton finite upper edge escapes as size increases. A million-candidate budget stops instead of returning a capped estimate. Free probability with free size remains pending P0.
