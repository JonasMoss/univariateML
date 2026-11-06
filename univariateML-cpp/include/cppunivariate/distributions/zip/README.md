---
id: zip
inference_numeric:
  - regime: "Poisson member with inflation known"
    available: true
    condition: "finite lambda > 0 free; pi = 0 held"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "known point-mass member"
    available: false
    condition: "finite lambda > 0 free; pi = 1 held; information is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "primary interior"
    available: true
    condition: "finite lambda > 0 and 0 < pi < 1; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-inflated Poisson
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: pi
  symbol: '\pi'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzip
  sampler: rzip
  control:
    lambda: "Optional held lambda; NULL estimates it."
    pi: "Optional held pi; NULL estimates it."
log_density: "Piecewise((log(pi + (1 - pi)*exp(-lambda)), Eq(x, 0)), (log(1 - pi) - lambda + x*log(lambda) - loggamma(x + 1), True))"
limits:
  - model: pois
    firmness: proved
    when: 'The rate $\lambda$ tends to zero with inflation $\pi<1$ held.'
    detect: 'All observations are zero, the rate is free and inflation is held below one.'
    fixed: [pi]
    status: ZeroLimit
  - name: undefined zero-rate reference
    firmness: proved
    when: 'The rate $\lambda$ is held at zero, outside the declared domain.'
    fixed: [lambda]
    status: InvalidInput
    message: lambda positive
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
examples: |
  set.seed(1)
  x <- rml(200, mlzip())
  mlzip(x)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: pois
    at:
      lambda: lambda
    type: zero-inflated
    mass: pi
fit:
  method: closed-form
  certified: partial
  note: 'zip/README.md, Details; zip/mle.hpp (public fitter and held overloads); zip/test.cpp: Free inflation is
    an exact clipped profile and the free fit reduces to the zero-truncated rate equation; held inflation uses a
    numerical rate search without a global root certificate.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model Poisson event counts with an additional structural-zero component.
summary: Zero-inflated Poisson provides a model for Poisson event counts with an additional structural-zero component.
aliases:
- "ZIP"
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With both coefficients free, every nonempty count sample has a finite maximum.
    If a count is positive, the maximizing pair is unique. All-zero data are
    maximized by $\pi=1$ with any $\lambda>0$. With $\lambda>0$ held, free
    inflation has a unique maximum. With $\pi<1$ held, a finite rate maximum
    exists whenever a count is positive, but uniqueness is not asserted. For
    all-zero data and held $\pi<1$, the supremum requires $\lambda\downarrow0$ and
    is not attained in the declared domain. Held $\pi=1$ permits only zeros and
    leaves a free rate unidentified. Holding both coefficients leaves likelihood
    evaluation, subject to compatibility.
  reason: >-
    Write $z$ for the empirical zero fraction. At a held rate, the inflation
    maximum is $\max\{0,(z-e^{-\lambda})/(1-e^{-\lambda})\}$, with value one for
    all-zero data. In the positive-inflation branch, the rate solves
    $\lambda/(1-e^{-\lambda})=\bar x/(1-z)$, whose left side is strictly
    increasing. If this branch is infeasible, the unique maximum is the Poisson
    member $\pi=0,\lambda=\bar x$. For held $\pi<1$ and a positive count, the
    likelihood tends to zero at both ends of the rate domain, so a finite maximum
    exists. It need not be concave, and the numerical rate search does not prove
    uniqueness.
---

The zero-inflated Poisson distribution mixes a Poisson count of rate $\lambda$ with a structural-zero component of probability $\pi$. The total zero probability also includes zeros from the Poisson component.

## Details

When the empirical zero fraction is no greater than $e^{-\bar x}$, the free fit is the attained Poisson member $\pi=0$. Otherwise inflation is positive and the rate is determined by the mean of the positive observations. All-zero data identify only the point mass at zero, leaving the rate arbitrary.

Holding the rate estimates only inflation by a clipped zero-fraction adjustment. Holding inflation below one leaves a finite rate maximum for any sample containing a positive count. This conditional likelihood can have separated peaks, which the numerical search compares without a global root certificate. With all observations zero, the rate instead tends to zero, outside the declared domain $\lambda>0$. A held zero rate is rejected. A held unit inflation is compatible only with zeros.

The [count-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-counts.md) records the boundary arguments and held subsets.

## R notes

Supply either coefficient to hold it, or use `NULL` to estimate it. The all-zero fit warns and returns `lambda = 1, pi = 1` when both are free. With unit inflation held, the free rate also uses one as a representative. All-zero data with `pi < 1` held stop with advice to use `mlpois`.
