---
id: tbinom
name: Truncated binomial
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- {name: prob, role: probability, domain: "[0, 1]", default: 0.4, fixable: true}
- {name: a, role: known design quantity, domain: "(-inf, inf)", default: 0, required: true}
- {name: b, role: known design quantity, domain: "(-inf, inf)", default: 10, required: true}
- {name: size, role: known design quantity, domain: "[0, inf)", default: 12, required: true, integer: true}
r:
  density: extraDistr::dtbinom
  sampler: extraDistr::rtbinom
  control:
    prob: Optional fixed parameter; NULL estimates it.
    a: Known strict lower truncation bound.
    b: Known inclusive upper truncation bound.
    size: Known nonnegative integer number of trials.
fit_args: {a: 0, b: 10, size: 12}
log_density: "x*log(prob)+(size-x)*log(1-prob)-loggamma(x+1)-loggamma(size-x+1)-log(Sum(exp(k*log(prob)+(size-k)*log(1-prob)-loggamma(k+1)-loggamma(size-k+1)), (k, Max(0, floor(a)+1), Min(size, floor(b)))))"
limits:
- name: point mass at lowest support point
  when: '$prob \to 0$'
  detect: all observations at a strictly positive lowest support point
  status: LowerPointMass
- name: point mass at highest support point
  when: '$prob \to 1$'
  detect: all observations at finite highest support point below size
  status: UpperPointMass
examples: |
  set.seed(1)
  mltbinom(extraDistr::rtbinom(100, 12, .4, 0, 10), a = 0, b = 10, size = 12)

---

The binomial law conditioned on known bounds `a < x <= b` models truncated counts.

## Details

Bounds must be supplied explicitly to avoid silently ignoring the sampling design.
Size is also required; estimating it introduces a separate discrete profile and infinite-size limits.
The conditional mean is strictly increasing in the natural parameter, since its
derivative is the positive conditional variance. Thus every sample mean strictly
between the support edges has one global maximum. Edge-only data escape to a
point mass; a singleton support does not identify the parameter.
The normalizer uses mode-centred recurrences with a two-million-term budget per tail.

When the conditioning event includes zero, the point mass at zero is attained
at `prob = 0`. When it includes `size`, the point mass there is attained at `prob = 1`.
Otherwise the corresponding zero-probability conditioning limit is reported as an escape.
