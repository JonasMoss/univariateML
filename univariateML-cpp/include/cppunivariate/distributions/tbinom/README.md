---
id: tbinom
inference_numeric:
  - regime: "singleton retained support"
    available: true
    condition: "known design and truncation bounds with exactly one retained integer; probability free in (0,1); the conditional law is a point mass, so information is zero"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: primary interior
    available: true
    condition: "known integer size, known a and b held; 0 < prob < 1 free; retained support has at least two integers"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Truncated binomial
support: {type: integer, lower: "max(0, floor(a)+1)", upper: "min(size, floor(b))", closed: [true, true]}
params:
- name: prob
  symbol: 'p'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
- name: a
  symbol: 'a'
  role: known design quantity
  domain: (-inf, inf)
  simulate: 0
  kind: design
- name: b
  symbol: 'b'
  role: known design quantity
  domain: (-inf, inf)
  simulate: 10
  kind: design
- name: size
  symbol: 'n'
  role: known design quantity
  domain: '[0, inf)'
  integer: yes
  simulate: 10
  kind: design
r:
  density: dtbinom
  sampler: rtbinom
  control:
    prob: "Optional fixed parameter; NULL estimates it."
    a: "Required lower bound; NULL estimates it as min(x) - 1 (strict truncation)."
    b: "Required upper bound; NULL estimates it as max(x)."
    size: "Required number of trials; NULL estimates integer size, with probability held or free."
log_density: "x*log(prob)+(size-x)*log(1-prob)-loggamma(x+1)-loggamma(size-x+1)-log(Sum(exp(k*log(prob)+(size-k)*log(1-prob)-loggamma(k+1)-loggamma(size-k+1)), (k, Max(0, floor(a)+1), Min(size, floor(b)))))"
limits:
- name: point mass at lowest support point
  firmness: proved
  when: $prob \to 0$
  detect: all observations at a strictly positive lowest support point
  status: LowerPointMass
- name: point mass at highest support point
  firmness: proved
  when: $prob \to 1$
  detect: all observations at finite highest support point below size
  status: UpperPointMass
- name: point mass at highest support point
  firmness: proved
  when: $size \to \infty$ at a fixed interior probability
  detect: constant data at a finite upper support edge with nonsingleton support
  fixed:
  - prob
  status: UpperPointMass
- model: tpois
  firmness: proved
  when: '$size \to \infty$, $prob \to 0$ with $size \cdot prob$ finite'
  detect: free size and probability; numerical profile approaches the truncated-Poisson face without exceeding it by 1e-9 times the sample size
  fixed: []
  status: TpoisLimit
- model: tpois
  firmness: proved
  when: '$size \to \infty$, $prob \to 0$ with $size \cdot prob$ finite'
  detect: same numerical comparison, with the lower bound held
  fixed: [a]
  status: TpoisLimit
- model: tpois
  firmness: proved
  when: '$size \to \infty$, $prob \to 0$ with $size \cdot prob$ finite'
  detect: same numerical comparison, with the upper bound held
  fixed: [b]
  status: TpoisLimit
- model: tpois
  firmness: proved
  when: '$size \to \infty$, $prob \to 0$ with $size \cdot prob$ finite'
  detect: same numerical comparison, with both bounds held
  fixed: [a, b]
  status: TpoisLimit
fit_args:
  a: 0
  b: 10
  size: 12
examples: |
  set.seed(1)
  x <- rml(200, mltbinom())
  mltbinom(x, a = 0L, b = 10L, size = 10L)

group: modified counts
construction:
- truncation
relations:
  truncation_of:
  - family: binom
    at:
      size: size
      prob: prob
fit:
  method: multistart
  certified: partial
  note: 'tbinom/README.md, Details; tbinom/mle.hpp (public fitter and held overloads); tbinom/test.cpp: Fixed size
    has exact conditional probability profiling; held probability has integer tail bounds. Free-size/free-probability
    grids lack a global unimodality certificate.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model success counts observed only within a specified conditioning window.
summary: Truncated binomial provides a model for success counts observed only within a specified conditioning window.
aliases: []
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
key_references:
- 1
---

The binomial law conditioned on known bounds `a < x <= b` models truncated counts.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-binomial.md) derives the stated limits and held-parameter conditions.

Bounds must be supplied explicitly to avoid silently ignoring the sampling design.
Size is also required; `NULL` estimates it by an integer profile. With probability held the search uses explicit tail bounds.
The conditional mean is strictly increasing in the natural parameter, since its
derivative is the positive conditional variance. Thus every sample mean strictly
between the support edges has one global maximum. Edge-only data escape to a
point mass; a singleton support does not identify the parameter.
The normalizer uses mode-centred recurrences with a two-million-term budget per tail.

When the conditioning event includes zero, the point mass at zero is attained
at `prob = 0`. When it includes `size`, the point mass there is attained at `prob = 1`.
Otherwise the corresponding zero-probability conditioning limit is reported as an escape.

At fixed size, free truncation bounds select the smallest conditioning support containing the sample: `a = min(x) - 1`, `b = max(x)`. The likelihood cannot decrease when unobserved support is removed. Singleton support attains likelihood one with any interior probability; an unspecified probability is returned as 0.5. A held boundary probability requires the conditioning event to have positive probability.

With probability held in (0,1), integer-size profiling bounds the entire tail by geometric ratios below a finite upper bound, or binomial stochastic ordering without an upper bound. A constant sample at a nonsingleton finite upper edge escapes as size increases. A million-candidate budget stops instead of returning a capped estimate. With probability free, the search scans 128 consecutive sizes, expands a bracket geometrically and refines every observed peak over integers. It compares the profile with the exact truncated-Poisson face on the same bounds; this is a numerical search, without a proof of global unimodality. Four consecutive expansions within `1e-9 * length(x)` of the face stop the search. A finite fit must beat that face by this tolerance; otherwise fitting stops with `use mltpois`. Two-point conditioning windows attain their saturated likelihood at finite size and are returned directly. The budget is 512 expansions, with no size magnitude cap; unresolved searches stop with numerical failure. Poisson-scale coordinates and `log1p` corrections avoid cancellation at large sizes. Constant samples attain likelihood one at `size = max(x), prob = 1`.

For a finite upper conditioning bound and an `Ok` candidate exceeding the Poisson face by `1e-9 * length(x)`, the fitter computes a tail cutoff. If every previously unvisited feasible integer through the cutoff fits within 40 million estimated operation units, it profiles all of them, including sizes below the upper bound; fixed-size profiling is concave in logit probability. The estimate charges at least 400 units per profile, plus a sample- and window-dependent estimate. When the cutoff or extra profiles cannot be resolved or exceed the budget, the original numerical search result is returned unchanged. The completed pass is a numerical certificate using the stated margin, not interval arithmetic or a directed-rounding proof. The face-win decision and all other results remain numerical. Infinite upper windows retain the earlier numerical search. See the [size-search investigation](../../../../../dev/research/evaluations/2026-10-31-size-search-report.md) for the proof and limitations.

Scalar d/p/q/r use the same positive-probability conditioning contract. At
`prob = 0` or `prob = 1`, the window must contain zero or size, respectively;
otherwise they return NaN. The atom at the corresponding extreme allowed
count exists as a weak limit but is not an attained member here. The ztbinom
family separately includes its positive-truncation probability-zero limit.
