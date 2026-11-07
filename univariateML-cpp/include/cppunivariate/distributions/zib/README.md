---
id: zib
inference_numeric:
  - regime: "size zero"
    available: false
    condition: "known integer size = 0 held; the law is a point mass and every regular score is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
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
    available: false
    condition: "known integer size >= 1, 0 < prob < 1 free, pi = 1 held; information is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "size one with one coefficient known"
    available: true
    condition: "known size = 1; exactly one of prob and pi free, the other held in (0,1); the remaining scalar Bernoulli parameter is identifiable"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "size one with both coefficients free"
    available: false
    condition: "known size = 1; prob and pi both free in (0,1); DQM and pointwise C2 hold but the joint information has rank one"
    reason: "The two free probability coefficients have rank-one information; covariance is singular"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "primary interior"
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
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per global certificate: 128 * min(iterlim, 100000) evaluations; per integer scan: 100000 * min(iterlim, 100000) enumeration cells."
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
  note: 'zib/README.md, Details; zib/mle.hpp; zib/test.cpp: Finite free-size winners require constrained retained-support tail exclusion and complete integer-prefix coverage; held-inflation ZIP witnesses use the ZIP certificate. Winning-face comparisons remain unresolved.'
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
    the infinite-size zero-inflated Poisson boundary. Retained-support certificates
    settle strict finite winners within their work budgets; winning-face decisions
    remain unresolved. With size held at an integer at
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
    boundary. A retained-support bound and complete integer-prefix coverage certify
    returned finite winners; they do not establish a general face-win criterion or
    uniqueness.
---

The zero-inflated binomial distribution mixes a binomial count with a structural-zero component of probability $\pi$. The integer size $n$ is the number of trials and $p$ is the base success probability.

## Details

The [binomial-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-binomial.md) records the boundary and held-subset arguments.

Holding size removes the infinite-size escape. The binomial member at $\pi=0$ remains attained. Size zero gives the point mass at zero and requires all-zero data. At size one, only the product $(1-\pi)p$ is identified when both probabilities are free.

A held probability is preserved. With $0<p<1$ and at least one positive count, the integer search bounds the remaining likelihood tail before returning. Exhausting its work budget is a numerical failure. Boundary probabilities must assign positive likelihood to the observations.

Holding inflation leaves a scalar probability profile at each integer size. Its stationary points are isolated using a quadratic turning-point equation. Root brackets must resolve relative to the probability: an absolute-width test can accept an unchecked tiny probability at a large held size. An unfinished solve stops numerically and retains its evaluated finite witness. The turning-point polynomial is formed in $u=Np$, with coefficients $n_{\mathrm{obs}}n_+$, $-S(n_{\mathrm{obs}}+n_+)-Sn_0/N$ and $S^2+Sn_0$, where $S$ is the count sum and $n_0,n_+$ are zero and positive sample counts. Differencing polynomial values in probability coordinates loses its linear coefficient at large size. Discriminants within $4096\epsilon(1+b^2+4ac)$ of zero stop numerically. Zero probabilities use `exp(N*log1p(-p))`; forming `1-p` first can round it to one and change the score. With no zeros, inflation is zero when free and contributes only a constant when held; the ordinary-binomial mean fit applies. The infinite-size comparison uses the zero-inflated Poisson boundary with the same held inflation.

When size and probability are free, the grid supplies a proposal. Success requires the retained-support correction $D_{N,K}$ from [ztbinom](../ztbinom/README.md), retaining $0,\ldots,K$, and complete evaluation or bounded block exclusion of all feasible sizes below the resulting tail cutoff. Positive counts have the same correction bound. At zero, the inflated mass is at most $e^{-c_N(K)}[\pi+(1-\pi)/Z_K]$, so the same total correction applies with inflation held or free.

The positive-mean root is checked before deciding whether the unconstrained inflation is feasible; otherwise an inaccurate tiny probability could select the wrong branch.

The retained likelihood factors into the positive conditional likelihood and a Bernoulli zero-indicator likelihood. On a log-rate interval the first has a concave tangent upper bound. The second is maximized over its attainable zero-mass interval: $[\pi+(1-\pi)/Z_K(\eta_+),\pi+(1-\pi)/Z_K(\eta_-)]$ when inflation is held and $[1/Z_K(\eta_+),1]$ when free. Subdivision bounds all branches without assuming unimodality. The base conditional mean brackets every maximum between the full and positive sample means. The work units and floating-point margins are those of the shared retained-support certificate.

The held-inflation ZIP face uses [ZIP's root certificate](../zip/README.md). Neither it nor a nearly equal face value establishes that no finite size wins. Unresolved face comparisons preserve both witnesses and an infinite global upper bound; signed-tail decisions are open. For data on $\{0,1\}$, size one is saturated; with inflation held, choose $p=\min(1,\bar x/(1-\pi))$. This also attains the best constrained zero frequency when held inflation exceeds the observed zero fraction. With both probabilities free only their product is identified.

## R notes

Supply size explicitly: a number holds it and `size = NULL` estimates it. Supply prob or pi to hold either probability. With both probabilities free, all-zero data return `prob = pi = 0`, preserving a held size or choosing size one when size is free.

For all-zero data with free size and either probability held, the fitter returns size zero, preserving held values and choosing zero for a free probability. The maximum log likelihood is exactly zero. No all-zero free-size subset requires an escape or non-identification stop: as with the existing all-free convention, the fitter returns an attained representative with status `Ok`, without claiming unique coefficients.

Indeed, the zero mass is $\pi+(1-\pi)(1-p)^n\leq1$, with equality exactly when $n=0$, $p=0$, or $\pi=1$ (interpreting size zero as the point mass even at $p=1$). With both probabilities held, size zero is unique if $p>0$ and $\pi<1$; otherwise every size maximizes. With only $p>0$ held, the maximizing set is $n=0$ or $\pi=1$; with $p=0$ held, both free coefficients are unidentified. With only $\pi<1$ held, the maximizing set is $n=0$ or $p=0$; with $\pi=1$ held, both free coefficients are unidentified. With neither held, the maximizing set is the union of all three faces, so no individual coefficient has a unique maximizing value. At the returned size-zero representative every free probability is unidentified. These are exact boundary statements, requiring no numerical size search.

The size grid has no fixed magnitude cap. After its initial grid depth it continues until more than ten exponential steps pass without improvement, subject to the iteration budget `iterlim`. Budget or floating-point resolution failures stop. An unresolved infinite-size comparison retains the fitted ZIP witness.

## Decision history

The [2026-10-29 tail investigation](../../../../../dev/research/evaluations/2026-10-29-tail85-report.md) records comparisons with independent reference-density multistart oracles, including tied and near-Poisson samples. The [2026-10-31 size-search investigation](../../../../../dev/research/evaluations/2026-10-31-size-search-report.md) reduces the free profile to positive-binomial and binomial branches and identifies the remaining certification gap. These comparisons support the implementation on tested samples rather than proving globality.

The [2026-11-02 all-zero correction](../../../../../dev/research/evaluations/2026-11-02-zib-zero-report.md) records the size-one non-maximizer with both probabilities held and the exact size-zero replacement.
