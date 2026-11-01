---
id: weibull
inference_numeric:
  - regime: interior
    available: true
    condition: "finite shape > 0 and scale > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Weibull
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  symbol: 'k'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: '\lambda'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: stats::dweibull
  sampler: stats::rweibull
  control:
    shape: Optional held shape; NULL estimates it.
    scale: Optional held scale; NULL estimates it.
    shape0: Optional positive finite starting shape; `NULL` uses the automatic start.
    reltol: Relative step tolerance in log(sigma) of the underlying Gumbel fit.
    iterlim: Maximum number of iterations.
log_density: log(shape) - log(scale) + (shape - 1)*log(x/scale) - (x/scale)**shape
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 21.
  Wiley, New York.
limits:
- kind: nonexistence
  name: unbounded likelihood
  when: '$\lambda$ equals the common observation and $k$ tends to infinity'
  detect: 'both coefficients are free and all observations are equal'
  status: Unbounded
  message: unbounded
- name: unbounded likelihood
  fixed: [scale]
  when: '$k$ tends to infinity with scale $\lambda$ held at the common observation value'
  detect: '$k$ is free and every observation equals the held scale $\lambda$'
  status: Unbounded
  message: unbounded
examples: |
  x <- datasets::precip
  mlweibull(x)

group: gamma and Weibull
construction:
- base
relations:
  special_case_of:
  - family: trgamma
    at:
      shape1: 1
      shape2: shape
      scale: scale
  transform_of:
  - family: exp
    at:
      rate: 1/scale**shape
    transform: power
    power: shape
  - family: gev
    at:
      mu: -scale
      sigma: scale/shape
      xi: -1/shape
    transform: negate
fit:
  method: delegation
  certified: 'yes'
  note: 'weibull/README.md, Details; weibull/mle.hpp (public fitter and held overloads); weibull/test.cpp: Gumbel
    location is profiled exactly and its scale equation has a unique root; held location has a strictly concave
    inverse-scale likelihood. Log/reciprocal transformations preserve this structure.'
areas:
- reliability and survival
- waiting times and queues
classification: classical
classification_reason: A standard model for reliability lifetimes with a power-shaped hazard.
summary: Weibull provides a model for reliability lifetimes with a power-shaped hazard.
aliases:
- "Rosin–Rammler"
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a unique finite MLE exists for every
    nonconstant positive sample. Constant data have unbounded likelihood.
    Holding shape gives a unique finite scale estimate even for constant data.
    Holding scale gives a unique finite shape estimate unless every observation
    equals that scale, when the likelihood is unbounded. Holding both leaves
    only likelihood evaluation.
  reason: >-
    Write $z_i=\log x_i$. After profiling scale, the shape score per
    observation is $1/k+\bar z-\sum_i z_i e^{kz_i}/\sum_i e^{kz_i}$.
    Its derivative is $-1/k^2$ minus a weighted variance. For nonconstant
    data it decreases from $+\infty$ to $\bar z-\max_i z_i<0$.
    With scale held, put $r_i=\log(x_i/\lambda)$. The score
    $n/k+\sum_i r_i(1-e^{kr_i})$ has derivative
    $-n/k^2-\sum_i r_i^2e^{kr_i}<0$ and crosses zero unless every $r_i=0$.
    With shape held, $\hat\lambda=(n^{-1}\sum_i x_i^k)^{1/k}$.
---

The Weibull distribution models lifetimes and reliability data. Its scale $\lambda$ sets the time unit. Its shape $k$ gives a decreasing hazard when $k<1$, a constant hazard when $k=1$, and an increasing hazard when $k>1$.

## Details

The joint fit transforms the observations to $-\log x$ and fits a Gumbel distribution. This reduces the problem to a unique scalar root after profiling location. Constant data instead allow the shape to grow without bound while scale remains at their common value, so no finite MLE exists.

Holding shape gives an explicit scale estimate. Holding scale leaves a strictly decreasing shape score, solved by bracketing. This fit is finite unless all observations equal the held scale. Holding both evaluates the likelihood.

## R notes

The optional `shape0` starting value is used and must be positive and finite.

## Decision history

The `shape0` argument was documented but ignored in univariateML 1.x; it is functional in the current fitter.

2026-10-31 (diagnostic contract): Empty or invalid input carries a non-success status. Degeneracy depends on the held coefficients, and failures from the Gumbel fit survive transformation. See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md) and the TASK-132 cases in `test.cpp` for the validation scope.
