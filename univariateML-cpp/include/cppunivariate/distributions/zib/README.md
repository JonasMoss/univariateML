---
id: zib
inference_numeric:
  - regime: size zero
    available: true
    condition: "known integer size = 0 held; the law is a point mass and every regular score is zero"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "known unit binomial probability"
    available: true
    condition: "known integer size >= 1, prob = 1 held, 0 < pi < 1 free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "binomial member with inflation known"
    available: true
    condition: "integer size >= 1 held, 0 < prob < 1 free, pi = 0 held"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "known point-mass member"
    available: true
    condition: "known integer size >= 1, 0 < prob < 1 free, pi = 1 held; information is zero"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "size one with one coefficient known"
    available: true
    condition: "known size = 1; exactly one of prob and pi free, the other held in (0,1); the remaining scalar Bernoulli parameter is identifiable"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "size one with both coefficients free"
    available: true
    condition: "known size = 1; prob and pi both free in (0,1); DQM and pointwise C2 hold but the joint information has rank one"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: primary interior
    available: true
    condition: "known integer size >= 2 held; 0 < prob < 1 and 0 < pi < 1; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-inflated binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  symbol: 'n'
  role: trials
  domain: '[0, inf)'
  integer: yes
  simulate: 10
  kind: design
- name: prob
  symbol: 'p'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
- name: pi
  symbol: '\pi'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzib
  sampler: rzib
  control:
    size: "Optional fixed size; NULL estimates size."
    reltol: "Tolerance for scalar profile search."
    iterlim: "Maximum scalar search iterations."
    prob: "Optional fixed probability in `[0, 1]`; NULL estimates it."
    pi: "Optional fixed pi in `[0, 1]`; NULL estimates it."
log_density: Piecewise((log(pi+(1-pi)*(1-prob)**size), Eq(x,0)), (log(1-pi)+loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)+x*log(prob)+(size-x)*log(1-prob), True))
limits:
- model: zip
  firmness: proved
  when: 'The trial count $n$ tends to infinity and $p$ tends to zero with $np$ tending to a finite positive rate.'
  detect: 'Size and probability are free, and the fitted zero-inflated Poisson boundary has log likelihood at least the best finite value found minus $10^{-8}$ times sample size. This numerical comparison does not certify non-existence.'
  status: ZipLimit
- model: zip
  firmness: proved
  when: 'The trial count $n$ tends to infinity and $p$ tends to zero with $np$ tending to a finite positive rate.'
  detect: 'Inflation is held, size and probability are free, and the zero-inflated Poisson boundary with that same inflation has log likelihood at least the best finite value found minus $10^{-8}$ times sample size. This numerical comparison does not certify non-existence.'
  status: ZipLimit
  fixed:
  - pi
fit_args:
  size: ~
examples: |
  set.seed(1)
  x <- rml(200, mlzib())
  mlzib(x, size = 10)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: binom
    at:
      size: size
      prob: prob
    type: zero-inflated
    mass: pi
fit:
  method: multistart
  certified: partial
  note: 'zib/README.md, Details; zib/mle.hpp (public fitter and held overloads); zib/test.cpp: Fixed size has exact
    conditional probability profiling; held probability has integer tail bounds. Free-size/free-probability grids
    lack a global unimodality certificate.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model bounded success counts with an additional structural-zero component.
summary: Zero-inflated binomial provides a model for bounded success counts with an additional structural-zero component.
aliases:
- "ZIB"
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, all-zero data attain a nonunique maximum. For
    positive data, finite-size attainment must be distinguished from a supremum on
    the infinite-size zero-inflated Poisson boundary; the numerical search does
    not settle this globally for every sample. With size held at an integer at
    least the largest count, a maximum always exists for compatible data, but it
    need not be unique. With $0<p<1$ held, a finite integer maximum exists.
    Holding only inflation retains the infinite-size comparison with that same
    inflation. Held $p=0$ or $\pi=1$ permits only zeros. Held $p=1$ requires every
    positive count to equal the size. Size zero permits only zeros and leaves both
    probabilities unidentified. Holding all coefficients leaves likelihood
    evaluation.
  reason: >-
    At a fixed size the probabilities range over a compact square and the
    likelihood is continuous, so a maximum is attained. For size one the law
    depends only on $(1-\pi)p$, giving nonunique pairs when both probabilities are
    free. With $0<p<1$ held and positive data, every observed positive-count
    probability tends to zero as size increases, so the integer tail cannot
    prevent attainment. With both probabilities held and all counts zero, the
    smallest compatible size attains the maximum. When size and probability are
    free, a finite-mean path to infinite size has the zero-inflated Poisson
    boundary. The current grid comparison is not a proof of global attainment or
    uniqueness.
---

The zero-inflated binomial distribution mixes a binomial count with a structural-zero component of probability $\pi$. The integer size $n$ is the number of trials and $p$ is the base success probability.

## Details

The [binomial-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-binomial.md) records the boundary and held-subset arguments.

Holding size removes the infinite-size escape. The binomial member at $\pi=0$ remains attained. Size zero gives the point mass at zero and requires all-zero data. At size one, only the product $(1-\pi)p$ is identified when both probabilities are free.

A held probability is preserved. With $0<p<1$ and at least one positive count, the integer search bounds the remaining likelihood tail before returning. Exhausting its work budget is a numerical failure. Boundary probabilities must assign positive likelihood to the observations.

Holding inflation leaves a scalar probability profile at each integer size. Its stationary points are isolated using a quadratic turning-point equation. The infinite-size comparison uses the zero-inflated Poisson boundary with the same held inflation.

When size and probability are free, an exponential grid of sizes is scanned and local peaks are refined. The free inflation profile is analytic. No unimodality proof or global tail certificate is known for this size profile. The boundary comparison uses a log-likelihood margin of $10^{-8}$ times the sample size and can reject a nearly tied finite maximum. This is a numerical decision rather than a necessary-and-sufficient data criterion for non-existence.

## R notes

Supply size explicitly: a number holds it and `size = NULL` estimates it. Supply prob or pi to hold either probability. With both probabilities free, all-zero data return `prob = pi = 0`, preserving a held size or choosing size one when size is free.

For all-zero data with both probabilities held, supply `size = 0` explicitly. The current free-size path can choose size one and miss the maximum in this case.

The size grid has no fixed magnitude cap. After its initial grid depth it continues until more than ten exponential steps pass without improvement, subject to the iteration budget `iterlim`. Budget or floating-point resolution failures stop. The infinite-size report recommends `mlzip`.

## Decision history

The [2026-10-29 tail investigation](../../../../../dev/research/evaluations/2026-10-29-tail85-report.md) records comparisons with independent reference-density multistart oracles, including tied and near-Poisson samples. The [2026-10-31 size-search investigation](../../../../../dev/research/evaluations/2026-10-31-size-search-report.md) reduces the free profile to positive-binomial and binomial branches and identifies the remaining certification gap. These comparisons support the implementation on tested samples rather than proving globality.
