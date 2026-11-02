---
id: ztnbinom
inference_numeric:
  - regime: "known zero negative-binomial size"
    available: true
    condition: "size = 0 held; 0 < prob < 1 free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: primary interior
    available: true
    condition: "finite size > 0 and 0 < prob < 1; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-truncated negative binomial
support:
  type: integer
  lower: 1
  upper: inf
  closed:
  - true
  - false
params:
- name: size
  symbol: 'r'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: prob
  symbol: 'p'
  role: probability
  domain: (0, 1]
  simulate: 0.5
  kind: regular
r:
  density: dztnbinom
  sampler: rztnbinom
  control:
    size: Optional fixed size; NULL estimates size.
    reltol: Tolerance for scalar profile search.
    iterlim: Maximum scalar search iterations.
    prob: "Optional held prob; NULL estimates it."
log_density: loggamma(size+x)-loggamma(size)-loggamma(x+1)+size*log(prob)+x*log(1-prob)-log(1-prob**size)
limits:
- model: ztpois
  firmness: proved
  when: As $r\to\infty$ and $p\to1$ with $r(1-p)/p\to\lambda>0$, the law tends to a zero-truncated Poisson law.
  detect: With both coefficients free and at least one count above one, the fitted zero-truncated Poisson log likelihood is at least the best searched finite-size value minus $10^{-8}$ times sample size.
  status: PoissonLimit
  numerical: true
examples: |
  set.seed(1)
  x <- rml(200, mlztnbinom())
  mlztnbinom(x)

group: modified counts
construction:
- truncation
relations:
  modification_of:
  - family: nbinom
    at:
      size: size
      prob: prob
    type: zero-truncated
  special_case_of:
  - family: zmnbinom
    at:
      size: size
      prob: prob
      p0: 0
fit:
  method: profile-search
  certified: 'no'
  note: "ztnbinom/mle.hpp: Log-size grids and local refinements profile the positive conditional likelihood; fixed size has a monotone probability solve, but joint and held-probability searches have no global certificate."
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model negative-binomial counts sampled only when positive.
summary: Zero-truncated negative binomial provides a model for negative-binomial counts sampled only when positive.
aliases:
- "positive negative binomial"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With both coefficients free, a general finite-MLE criterion and uniqueness result are not established here. The supremum can occur at the zero-truncated Poisson limit. All-one samples attain $p=1$ and leave $r$ unidentified. With finite $r>0$ held, the probability MLE exists and is unique, including $p=1$ for all-one data. Held $r=0$ gives a logarithmic-series model with a unique probability estimate. With $0<p<1$ held, a finite size maximum exists, possibly at $r=0$, but uniqueness is not established here. Held $p=1$ requires only ones and leaves free size unidentified. Holding both coefficients leaves only likelihood evaluation.
  reason: >-
    For fixed positive size, the positive-count law is an exponential family in $\log(1-p)$. Its conditional mean ranges strictly from one to infinity, and its log likelihood is strictly concave in that natural parameter. At size zero the same argument applies to the logarithmic-series member. With interior probability held, likelihood is continuous at size zero and tends to zero as size tends to infinity, so a maximum is attained on $[0,\infty)$. These facts do not certify the two-free-coefficient profile search.
---

The zero-truncated negative binomial distribution models negative-binomial counts observed only when positive. The size $r$ is a nonnegative shape coefficient that can be fractional, and $p$ is the underlying negative-binomial probability.

## Details

Holding a positive size leaves a unique probability fit obtained by conditional-mean matching. Size zero is an attained logarithmic-series member with logarithmic parameter $1-p$. Probability one is the attained point mass at one. All-one samples therefore fail to identify a free size. Holding any finite size removes the infinite-size Poisson escape.

Holding an interior probability also removes that escape. The likelihood then vanishes at infinite size, so a finite maximum exists. The fit compares size zero with peaks bracketed on a grid in log size. This numerical search does not certify that every peak has been found.

With both coefficients free and some count above one, the fit profiles probability by conditional-mean matching. It scans log sizes from -24 to 24, refines bracketed peaks, and compares the best value with the attained logarithmic member and the zero-truncated Poisson limit. A tolerance of $10^{-8}$ times sample size is used in these comparisons. Neither finite attainment nor global optimality is certified by this grid. Reaching the upper size endpoint with a successful candidate is treated as a numerical failure.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-counts.md) discusses the stated limits and held-parameter conditions. Held probability one is compatible only with ones.

## R notes

The numerical limit comparison can reject a finite mathematical maximum that ties or improves on the limit by less than the comparison tolerance. A limit diagnostic is therefore not a proof of non-existence for the supplied sample.

Supplying `size` or `prob` holds that coefficient, while `NULL` estimates it. A held size may be zero. The attained members at `size = 0` and `prob = 1` follow actuar's conventions. Detected Poisson escapes stop with advice to use `mlztpois`. Invalid controls, incompatible held values and reported iteration failures stop. Increasing `iterlim` does not extend the fixed free-size grid or prove global optimality.
