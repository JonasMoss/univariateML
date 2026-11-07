---
id: zmbinom
inference_numeric:
  - regime: "known boundary binomial probability"
    available: true
    condition: "known integer size >= 1; prob = 0 or prob = 1 held; 0 < p0 < 1 free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "positive-count member with zero mass known"
    available: true
    condition: "known integer size >= 2 held, 0 < prob < 1 free, p0 = 0 held"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "known point-mass member"
    available: false
    condition: "known integer size >= 1 held, 0 < prob < 1 free, p0 = 1 held; information is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "size one with probability known"
    available: true
    condition: "known size = 1; prob held in (0,1), p0 free in (0,1); the remaining zero-mass parameter is identifiable"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "size one with probability free"
    available: false
    condition: "known size = 1; prob free; the conditional positive distribution is a point mass and the probability score vanishes"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "primary interior"
    available: true
    condition: "known integer size >= 2 held; 0 < prob < 1 and 0 < p0 < 1; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-modified binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  symbol: 'n'
  role: trials
  domain: '[1, inf)'
  integer: yes
  simulate: 10
  kind: design
- name: prob
  symbol: 'p'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
- name: p0
  symbol: 'p_0'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzmbinom
  sampler: rzmbinom
  control:
    size: "Optional fixed size; NULL estimates size."
    reltol: "Tolerance for scalar profile search."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per global certificate: 128 * min(iterlim, 100000) evaluations; per integer scan: 100000 * min(iterlim, 100000) enumeration cells."
    prob: "Optional fixed probability in `[0, 1]`; NULL estimates it."
    p0: "Optional fixed p0 in `[0, 1]`; NULL estimates it."
log_density: Piecewise((log(p0), Eq(x, 0)), (log(1-p0)+(loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)+x*log(prob)+(size-x)*log(1-prob)-log(1-(1-prob)**size)), True))
limits:
- model: zmpois
  firmness: proved
  when: As $n\to\infty$ and $p\to0$ with $np\to\lambda>0$, the positive-count law tends to a zero-truncated Poisson law. The zero probability $p_0$ remains fixed, giving a zero-modified Poisson law.
  detect: With both base coefficients free and at least one count above one, the fitted zero-modified Poisson log likelihood is at least the best searched finite-size value minus $10^{-8}$ times the number of positive observations.
  status: PoissonLimit
  numerical: true
- model: zmpois
  firmness: proved
  when: As $n\to\infty$ and $p\to0$ with $np\to\lambda>0$, the positive-count law tends to a zero-truncated Poisson law. The zero probability $p_0$ remains fixed, giving a zero-modified Poisson law.
  detect: With $p_0$ held, at least one count above one and both base coefficients free, the matching zero-modified Poisson log likelihood is at least the best searched finite-size value minus $10^{-8}$ times the number of positive observations.
  status: PoissonLimit
  fixed:
  - p0
  numerical: true
fit_args:
  size: ~
examples: |
  set.seed(1)
  x <- rml(200, mlzmbinom())
  mlzmbinom(x, size = 10)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: binom
    at:
      size: size
      prob: prob
    type: zero-modified
    mass: p0
fit:
  method: delegation
  certified: partial
  note: 'zmbinom/README.md, Details; zmbinom/mle.hpp; zmbinom/test.cpp: The certified positive-count ztbinom routes transfer with the Bernoulli zero-indicator factor, including stop witnesses. Winning-face comparisons remain unresolved.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model bounded counts with an independently adjustable zero probability.
summary: Zero-modified binomial provides a model for bounded counts with an independently adjustable zero probability.
aliases:
- "hurdle binomial"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, the zero probability is the empirical zero fraction and the remaining problem is zero-truncated binomial fitting on the positive observations. A general finite-MLE criterion and uniqueness result for that free-size problem are not established here. All-zero samples attain $p_0=1$ and leave the base coefficients unidentified. Positive counts all equal to one attain a maximum with nonunique base coefficients. Holding $p_0$ leaves the positive-count problem unchanged when compatible. With $n>1$ held and at least one positive count, the free probability has a unique MLE. At $n=1$, it is unidentified. With interior $p$ held and positive counts present, a finite integer maximum exists. Held $p=0$ permits only ones among positives and leaves size unidentified. Held $p=1$ requires all positive counts to equal $n$. With no positives, all compatible base coefficients remain unidentified. All-held fits only evaluate likelihood.
  reason: >-
    The likelihood factors into a Bernoulli zero-indicator likelihood and a zero-truncated binomial likelihood. The first has the unique empirical-fraction maximizer when free. At fixed $n>1$, the positive-count likelihood is strictly concave in logit probability. With interior probability held, its observed masses vanish as size grows, ensuring finite integer attainment. Factorization transfers the finite-winner certificate but supplies no general uniqueness theorem or winning-face criterion for free size and probability.
---

The zero-modified binomial distribution describes bounded counts with an independently adjustable zero probability $p_0$. Conditional on a positive count, the distribution is binomial with trial count $n$ and success probability $p$, conditioned to be positive.

## Details

The likelihood separates into a zero-indicator part and a positive-count part. Holding the zero probability changes only the first part. The fit for the positive counts is the zero-truncated binomial fit, with the same held trial count or probability.

With no positives the base coefficients are unidentified. With no zeros the free zero probability is zero. Positive counts all equal to one give an attained two-point distribution. Holding the trial count removes the infinite-size escape, and the point mass at one remains attained at every trial count. At trial count one the positive-count probability is unidentified.

A held interior probability is preserved and the integer search bounds the remaining tail before returning. Exhausting its work budget is an error. Boundary probabilities must give positive likelihood to the observations. A held zero probability of zero is incompatible with observed zeros, and a held zero probability of one is incompatible with positives.

The free-size positive-count certificate and its unresolved face comparisons transfer from [ztbinom](../ztbinom/README.md). The zero-indicator log likelihood is added to every candidate and bound, with `p0` appended to its coefficients. A positive-Poisson face becomes a zero-modified Poisson face with the same `p0`. This transfer applies with `p0` held or fitted; it supplies no general uniqueness theorem for the integer size.

## R notes

Unresolved Poisson comparisons retain diagnostic witnesses and do not assert non-existence.

Supply `size` explicitly. A numeric value holds it, while `size = NULL` estimates it. Supplying `prob` or `p0` holds that coefficient. With no positives the fitter preserves supplied base coefficients and chooses conventional representatives for free ones. With both `prob` and `p0` free it warns about unidentified base coefficients on all-zero data. With both free and positive counts all equal to one it warns that `mlbinom(x, size = 1)` fits the attained law exactly. A held `size` is preserved at these boundaries. Unresolved infinite-size comparisons retain the fitted zero-modified Poisson witness with the same `p0`, and stop with kind `unresolved`. Incompatible held values stop.
