---
id: zinb
inference_numeric:
  - regime: "negative-binomial member with inflation known"
    available: true
    condition: "finite size > 0 and 0 < prob < 1; pi = 0 held; size and/or prob free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "known point-mass member"
    available: true
    condition: "finite size > 0 and 0 < prob < 1; pi = 1 held; remaining information is zero"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: primary interior
    available: true
    condition: "finite size > 0, 0 < prob < 1 and 0 < pi < 1; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-inflated negative binomial
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
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
- name: pi
  symbol: '\pi'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzinb
  sampler: rzinb
  control:
    reltol: "Tolerance on the scalar log-size search interval."
    iterlim: "Maximum number of scalar search iterations."
    size: "Optional fixed finite nonnegative size, including fractional values; NULL estimates size."
    prob: "Optional held prob; NULL estimates it."
    pi: "Optional held pi; NULL estimates it."
log_density: "Piecewise((log(pi + (1 - pi)*prob**size), Eq(x, 0)), (log(1 - pi) + loggamma(x + size) - loggamma(size) - loggamma(x + 1) + size*log(prob) + x*log(1 - prob), True))"
limits:
  - model: zip
    firmness: proved
    when: 'The size $r$ tends to infinity and $p$ tends to one with $r(1-p)/p$ fixed and positive inflation.'
    detect: 'Size and probability are free, and the fitted zero-inflated Poisson boundary has positive inflation and log likelihood at least the best finite value found minus $10^{-8}$ times sample size. This numerical comparison does not certify non-existence.'
    status: ZipLimit
  - model: pois
    firmness: proved
    when: 'The size $r$ tends to infinity and $p$ tends to one with $r(1-p)/p$ fixed, while inflation tends to zero.'
    detect: 'Size and probability are free, and the fitted zero-inflated Poisson boundary has zero inflation and log likelihood at least the best finite value found minus $10^{-8}$ times sample size. This numerical comparison does not certify non-existence.'
    status: PoissonLimit
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
examples: |
  set.seed(1)
  x <- rml(200, mlzinb())
  mlzinb(x)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: nbinom
    at:
      size: size
      prob: prob
    type: zero-inflated
    mass: pi
  special_case_of:
  - family: zmnbinom
    at:
      size: size
      prob: prob
      p0: pi+(1-pi)*prob**size
fit:
  method: multistart
  certified: 'no'
  note: 'zinb/README.md, Details; zinb/mle.hpp (public fitter and held overloads); zinb/test.cpp: Default and nontrivial
    conditional fits use multistart, finite grids or local profile refinement without a global certificate; all-held
    density evaluation is not counted as certification.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model overdispersed counts with an additional structural-zero component.
summary: Zero-inflated negative binomial provides a model for overdispersed counts with an additional structural-zero
  component.
aliases:
- "ZINB"
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, all-zero data attain a nonunique point-mass
    maximum. For samples containing positive counts, existence depends on
    comparison with the infinite-size zero-inflated Poisson boundary; no general
    uniqueness claim is made. A finite nonnegative size held fixed, or a
    probability held strictly below one, removes that escape and gives an attained
    maximum for compatible data. Holding only inflation below one retains the
    escape, with the same inflation held on the limiting boundary. Further holds
    retain these conclusions. Held size zero, probability one or inflation one
    permits only zeros. Holding all coefficients leaves likelihood evaluation.
  reason: >-
    For positive data, profile probability and any free inflation at each size
    $r>0$, obtaining $Q(r)$. Its infinite-size boundary is the corresponding
    maximized zero-inflated Poisson log likelihood $Q_\infty$. A finite maximum
    exists if some finite $r$ has $Q(r)\ge Q_\infty$; otherwise the supremum is
    attained only on that boundary. This is a global profile characterization, not
    a certified numerical test. Holding finite positive size makes the probability
    endpoints either attained zero laws or zero-likelihood limits. At held
    $0<p<1$, the likelihood of positive counts vanishes as $r\to\infty$ and the
    zero-size endpoint is attained. These boundary arguments establish attainment
    for those held subsets, but not uniqueness.
---

The zero-inflated negative binomial distribution adds structural zeros with probability $\pi$ to a negative binomial count. The size $r$ controls dispersion and need not be an integer. The base probability $p$, together with size, determines the count scale.

## Details

Inflation zero is an attained negative binomial member. Size zero, probability one and inflation one each give the point mass at zero. Thus all-zero data attain a maximum, although the coefficients are not identified. Endpoint holds that give zero probability to a positive observation are incompatible.

Holding a finite nonnegative size removes the infinite-size escape. Holding a probability strictly below one also removes it, because increasing size at fixed probability sends mass away from the observed positive counts. Holding only inflation retains comparison against the zero-inflated Poisson boundary with that same inflation.

Free inflation profiles out analytically at each base distribution. The remaining scalar profiles are searched numerically. Neither the grid nor local refinement certifies that every peak has been found. In particular, a boundary log likelihood within $10^{-8}$ times the sample size of the best finite value found triggers a limit report. This practical comparison can reject a nearly tied finite maximum and is not an exact existence criterion.

The [count-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-counts.md) gives the global profile characterization and held-subset arguments.

## R notes

A supplied coefficient is held; `NULL` estimates it. Fractional nonnegative size values are allowed. All-zero data with free size return `size = 0`, preserve holds and use `prob = 0.5, pi = 0` for unidentified free coordinates. With positive size held, a free probability is one. If probability is held below one, free inflation is one. These boundary fits are silent.

An infinite-size report stops with advice to use `mlzip`, or `mlpois` when the selected limiting inflation is zero. Numerical search failure also stops.
