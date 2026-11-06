---
id: tbinom
inference_numeric:
  - regime: "singleton retained support"
    available: false
    condition: "known design and truncation bounds with exactly one retained integer; probability free in (0,1); the conditional law is a point mass, so information is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "primary interior"
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
  when: 'With size fixed, $p\to0$ on a nonsingleton window whose lowest retained count is positive.'
  detect: 'Size is held, probability is free, and every observation equals the strictly positive lowest count of a nonsingleton retained window.'
  status: LowerPointMass
- name: point mass at highest support point
  firmness: proved
  when: 'With size fixed, $p\to1$ on a nonsingleton window whose highest retained count is below $n$.'
  detect: 'Size is held, probability is free, and every observation equals the highest count of a nonsingleton retained window below $n$.'
  status: UpperPointMass
- name: point mass at highest support point
  firmness: proved
  when: 'The size satisfies $n\to\infty$ with $0<p<1$ held and a finite upper truncation bound.'
  detect: 'Size is free, probability is held strictly between zero and one, and constant data equal the finite upper truncation edge and no feasible size can make the retained support singleton.'
  fixed:
  - prob
  status: UpperPointMass
- model: tpois
  firmness: proved
  when: 'The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same truncation window.'
  detect: 'Size and probability are free. The numerical size profile approaches the truncated-Poisson limit without exceeding its log likelihood by $10^{-9}$ times the sample size. This comparison does not prove non-existence of a finite maximizer.'
  fixed: []
  status: TpoisLimit
  numerical: true
- model: tpois
  firmness: proved
  when: 'The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same truncation window.'
  detect: 'Size and probability are free and the lower bound is held. The numerical profile does not exceed the truncated-Poisson log likelihood by $10^{-9}$ times the sample size. This is a numerical comparison rather than a non-existence proof.'
  fixed: [a]
  status: TpoisLimit
  numerical: true
- model: tpois
  firmness: proved
  when: 'The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same truncation window.'
  detect: 'Size and probability are free and the upper bound is held. The numerical profile does not exceed the truncated-Poisson log likelihood by $10^{-9}$ times the sample size. This is a numerical comparison rather than a non-existence proof.'
  fixed: [b]
  status: TpoisLimit
  numerical: true
- model: tpois
  firmness: proved
  when: 'The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same truncation window.'
  detect: 'Size and probability are free and both bounds are held. The numerical profile does not exceed the truncated-Poisson log likelihood by $10^{-9}$ times the sample size. This is a numerical comparison rather than a non-existence proof.'
  fixed: [a, b]
  status: TpoisLimit
  numerical: true
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
  method: integer-search
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
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, existence and uniqueness are not classified
    for general nonconstant samples. Held bounds and sizes must permit every
    observation. Constant samples attain likelihood one
    at $n=\max_i x_i$ and $p=1$. Free bounds can be tightened to the sample
    extremes, but their real values are not unique. With size held and
    nonsingleton retained support, a unique interior probability MLE exists
    exactly when the sample mean lies strictly between the retained edges.
    Edge-only samples attain $p=0$ or $p=1$ only when the window contains
    zero or $n$, respectively. Otherwise they have an unattained point-mass
    supremum. Singleton support leaves an interior probability unidentified.
    With an interior probability held and size free, a finite integer
    maximum exists unless constant data equal the finite upper truncation edge and no
    feasible size can make the retained support singleton. Integer maxima
    need not be unique. With both size and probability
    held, free bounds tighten to the sample extremes whenever the observations
    and conditioning event have positive probability. A held $p=0$ permits only
    zeros and leaves a free size unidentified among feasible sizes. A held $p=1$ permits only constant data and fixes a free size to
    that count. Holding every coefficient
    leaves only likelihood evaluation.
  reason: >-
    Tightening a window removes only unobserved mass. For fixed size the
    log likelihood is strictly concave in $\log(p/(1-p))$ on nonsingleton
    support, with curvature minus the sample size times the conditional
    variance. With an interior probability held, increasing size concentrates
    a finitely upper-truncated law at its upper edge. If an observation lies
    below that edge the likelihood tends to zero. Without an upper bound,
    the probability of any fixed finite sample tends to zero. These tail
    arguments ensure attainment outside the stated escape. With both size
    and probability free, the truncated-Poisson limit competes with finite
    profiles. The general search and its face comparisons are numerical,
    and do not prove global uniqueness or a general existence criterion.
---

The truncated binomial distribution models success counts observed only within a conditioning window $a<X\le b$. The trial count $n$ describes the underlying binomial experiment and $p$ is its success probability. The bounds describe which counts can be recorded.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-binomial.md) derives the stated limits and held-parameter conditions.

Bounds must be supplied explicitly to avoid silently ignoring the sampling design.
Estimating size requires an integer profile. With probability held strictly between zero and one, the search uses explicit tail bounds.
The conditional mean is strictly increasing in the natural parameter, since its
derivative is the positive conditional variance. With size held, every sample mean strictly
between the support edges therefore gives one interior probability maximum.
Edge-only data give either an attained boundary mass or an unattained limit,
as described below. Singleton support does not identify the probability.

When the conditioning event includes zero, the point mass at zero is attained
at $p=0$. When it includes $n$, the point mass there is attained at $p=1$.
Otherwise the corresponding zero-probability conditioning limit is reported as an escape.

At fixed size, free truncation bounds select the smallest conditioning support containing the sample: $a=\min_i x_i-1$, $b=\max_i x_i$. The likelihood cannot decrease when unobserved support is removed. Singleton support attains likelihood one with any interior probability. Bounds giving the same integer window are equivalent, so their real values are not unique. A held boundary probability requires the conditioning event to have positive probability.

## R notes

Supply `a`, `b` and `size` explicitly. A numeric value holds the corresponding coefficient. Use `a = NULL` or `b = NULL` to estimate a bound at `min(x) - 1` or `max(x)`. Use `size = NULL` to estimate the trial count, and supply `prob` to hold the probability. Singleton support represents a free probability by 0.5. The truncated-Poisson comparison stops with advice to use `mltpois` on the same bounds.

Scalar d/p/q/r use the same positive-probability conditioning contract. At
`prob = 0` or `prob = 1`, the window must contain zero or size, respectively;
otherwise they return NaN. The atom at the corresponding extreme allowed
count exists as a weak limit but is not an attained member here. The ztbinom
family separately includes its positive-truncation probability-zero limit.

## Decision history

The normalizer uses mode-centred recurrences with a two-million-term budget per tail.

With probability held in $(0,1)$, integer-size profiling bounds the entire tail by geometric ratios below a finite upper bound, or binomial stochastic ordering without an upper bound. A constant sample at the finite upper truncation edge escapes as size increases unless some feasible size gives singleton support. An exhausted million-candidate budget stops instead of returning a capped estimate. With probability free, the search scans 128 consecutive sizes, expands a bracket geometrically and refines every observed peak over integers. It compares the profile with the exact truncated-Poisson face on the same bounds; this is a numerical search, without a proof of global unimodality. Four consecutive expansions within $10^{-9}$ times the sample size of the face stop the search. A finite fit must beat that face by this tolerance; otherwise the fitter reports the truncated-Poisson limit. This decision is numerical, not a proof that a finite maximizer cannot exist. Two-point conditioning windows attain their saturated likelihood at finite size and are returned directly. The budget is 512 expansions, with no size magnitude cap; unresolved searches stop with numerical failure. Poisson-scale coordinates and `log1p` corrections avoid cancellation at large sizes. Constant samples attain likelihood one at $n=\max_i x_i$, $p=1$.

For a finite upper conditioning bound and a successful candidate exceeding the Poisson face by $10^{-9}$ times the sample size, the fitter computes a tail cutoff. If every previously unvisited feasible integer through the cutoff fits within 40 million estimated operation units, it profiles all of them, including sizes below the upper bound; fixed-size profiling is concave in logit probability. The estimate charges at least 400 units per profile, plus a sample- and window-dependent estimate. When the cutoff or extra profiles cannot be resolved or exceed the budget, the original numerical search result is returned unchanged. The completed pass is a numerical certificate using the stated margin, not interval arithmetic or a directed-rounding proof. The face-win decision and all other results remain numerical. Infinite upper windows retain the earlier numerical search. See the [size-search investigation](../../../../../dev/research/evaluations/2026-10-31-size-search-report.md) for the proof and limitations.
