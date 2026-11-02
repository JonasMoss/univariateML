---
id: ztbinom
inference_numeric:
  - regime: "size one"
    available: true
    condition: "known size = 1; 0 < prob < 1 free; the zero-truncated law is a point mass, so information is zero"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: primary interior
    available: true
    condition: "known integer size >= 2 held; 0 < prob < 1 free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-truncated binomial
support: {type: integer, lower: "1", upper: "size", closed: [true, true]}
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
r:
  density: dztbinom
  sampler: rztbinom
  control:
    size: "Optional fixed size; NULL estimates size."
    reltol: "Tolerance for scalar profile search."
    iterlim: "Maximum scalar search iterations."
    prob: "Optional fixed probability in `[0, 1]`; NULL estimates it."
log_density: loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)+x*log(prob)+(size-x)*log(1-prob)-log(1-(1-prob)**size)
limits:
- model: ztpois
  firmness: proved
  when: As $n\to\infty$ and $p\to0$ with $np\to\lambda>0$, the positive-count law tends to a zero-truncated Poisson law.
  detect: With both coefficients free and at least one count above one, the fitted zero-truncated Poisson log likelihood is at least the best searched finite-size value minus $10^{-8}$ times sample size.
  status: PoissonLimit
fit_args:
  size: ~
examples: |
  set.seed(1)
  x <- rml(200, mlztbinom())
  mlztbinom(x, size = 10)

group: modified counts
construction:
- truncation
relations:
  modification_of:
  - family: binom
    at:
      size: size
      prob: prob
    type: zero-truncated
  special_case_of:
  - family: zmbinom
    at:
      size: size
      prob: prob
      p0: 0
fit:
  method: multistart
  certified: partial
  note: 'ztbinom/README.md, Details; ztbinom/mle.hpp (public fitter and held overloads); ztbinom/test.cpp: Fixed
    size has exact conditional probability profiling; held probability has integer tail bounds. Free-size/free-probability
    grids lack a global unimodality certificate.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model binomial success counts sampled only when positive.
summary: Zero-truncated binomial provides a model for binomial success counts sampled only when positive.
aliases:
- "positive binomial"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With both trial count and probability free, a general finite-MLE criterion and uniqueness result are not established here. The supremum can lie at the zero-truncated Poisson limit as $n$ tends to infinity. All-one data attain a maximum with nonunique coefficients. With integer $n\ge\max_i x_i$ held and $n>1$, the probability MLE exists and is unique, including its endpoints. At $n=1$, only ones are possible and $p$ is unidentified. With $0<p<1$ held, a finite integer maximum exists but uniqueness is not asserted. Held $p=0$ requires only ones and leaves $n$ unidentified. Held $p=1$ requires constant counts and sets $n$ to that count. Holding both coefficients requires compatible observations.
  reason: >-
    At fixed $n>1$, the log likelihood is strictly concave in logit probability, with curvature minus sample size times the conditional variance. The conditional mean ranges from one to $n$, giving a unique mean-matching solution or endpoint. At held interior probability, each observed mass tends to zero as $n$ tends to infinity, so a finite integer maximum exists. These arguments do not establish unimodality or uniqueness of the free-size profile.
---

The zero-truncated binomial distribution describes binomial success counts observed only when positive. The integer trial count $n$ bounds the counts, and $p$ is the success probability before conditioning.

## Details

Holding the trial count removes the infinite-size escape. For a trial count greater than one, the fit matches the conditional mean to the sample mean. All-one data give the attained endpoint $p=0$, and counts all equal to the held trial count give $p=1$. At trial count one every probability gives the same point mass, and the probability is unidentified.

With both coefficients free, probability is profiled by conditional-mean matching at each candidate size. The search scans an exponential grid and refines local peaks before comparing neighboring integer sizes. The grid extends beyond its initial range until more than ten consecutive cells give no improvement, subject to the iteration budget. This is a numerical stopping rule. Neither the grid nor its comparison with the Poisson limit certifies a global maximum.

With an interior probability held, the integer search bounds the remaining tail before returning. Exhausting its work budget is an error. Probability zero permits only ones. Probability one permits only counts equal to the trial count. These are attained members, not escapes.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-binomial.md) discusses the boundaries. The [search checks](../../../../../dev/research/evaluations/2026-10-29-tail85-report.md) compare the fit with independent reference-density multistart oracles, including tied and near-Poisson samples. Such comparisons do not prove global optimality.

## R notes

The numerical limit comparison can reject a finite mathematical maximum that ties or improves on the limit by less than the comparison tolerance. A limit diagnostic is therefore not a proof of non-existence for the supplied sample.

Supply `size` explicitly. A numeric value holds it, while `size = NULL` estimates it. Supplying `prob` holds the probability exactly. The boundary convention at `prob = 0` agrees with actuar. Detected Poisson escapes stop with advice to use `mlztpois`. Iteration exhaustion stops instead of returning a capped size. Increasing `iterlim` permits more work but does not supply a global certificate.

## Decision history

2026-10-29: the free-size grid's fixed magnitude cap was removed. The current search grows subject to its iteration budget and numerical stopping rule. See the search checks linked above.

2026-10-31: the [size-search investigation](../../../../../dev/research/evaluations/2026-10-31-size-search-report.md) records the remaining certificate gap. DeRiggi, D. F. (1983), “Unimodality of Likelihood Functions for the Binomial Distribution”, Journal of the American Statistical Association, 78(381), 181–183, doi:10.1080/01621459.1983.10477949, concerns the untruncated law. DeRiggi, D. (1994), “Sufficient conditions for unimodality of the positive binomial likelihood function”, Statistics & Probability Letters, 19(1), 1–4, doi:10.1016/0167-7152(94)90061-2, reports a continuous-profile counterexample. As recorded in that investigation, the full paper and correction remain unchecked, and this does not establish an integer-size counterexample.
