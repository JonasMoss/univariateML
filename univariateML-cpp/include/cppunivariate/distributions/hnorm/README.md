---
id: hnorm
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite mu held and sigma > 0 free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support"
name: Translated Half-Normal
support: {type: real, lower: "mu", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "log(2/pi)/2 - log(sigma)"}}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  bound: lower
  kind: threshold
  advice: Supply mu = 0 for the ordinary half-normal, or NULL to estimate the translated endpoint.
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dhnorm
  sampler: rhnorm
  control:
    sigma: Held positive finite coefficient; NULL estimates it.
    mu: Required endpoint; a value holds it and explicit `NULL` estimates the translation.
log_density: log(2/pi)/2 - log(sigma) - (x-mu)**2/(2*sigma**2)
limits:
  - firmness: proved
    name: scale collapse
    when: 'The scale tends to zero at the fitted or held endpoint $\mu$.'
    detect: 'Scale is free and every observation equals the fitted or held endpoint.'
    status: DegenerateData
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994). Continuous Univariate Distributions, Volume 1, second edition.
  Wiley.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
fit_args:
  mu: ~
examples: |
  set.seed(1)
  x <- rml(200, mlhnorm())
  mlhnorm(x, mu = NULL)

group: normal and Student
construction:
- folding
relations:
  transform_of:
  - family: norm
    at:
      mean: 0
      sd: sigma
    transform: abs
    shift: mu
  - family: foldnorm
    at:
      mu: 0
      sigma: sigma
    transform: shift
    shift: mu
  - family: naka
    at:
      shape: 0.5
      scale: sigma**2
    transform: shift
    shift: mu
fit:
  method: delegation
  certified: 'yes'
  note: 'hnorm/README.md, Details; hnorm/mle.hpp (public fitter and held overloads); hnorm/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- measurement and errors
classification: specialist
classification_reason: Used to model one-sided Gaussian deviations from a lower endpoint.
summary: Translated Half-Normal provides a model for one-sided Gaussian deviations from a lower endpoint.
aliases: []
key_references:
- 2
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a unique MLE exists exactly when the sample
    is nonconstant. The endpoint is $\hat\mu=\min_i x_i$. With $\mu$ held,
    a unique scale estimate exists when all observations are at least $\mu$
    and at least one exceeds it. With positive $\sigma$ held, the free
    endpoint is uniquely estimated by the sample minimum, even for constant data.
  reason: >-
    For every positive scale the likelihood increases as the endpoint moves
    towards the sample minimum. At that endpoint, or at a held compatible
    endpoint, maximizing over scale gives
    $\hat\sigma^2=n^{-1}\sum_i(x_i-\mu)^2$ uniquely when this is positive.
    If it is zero, the likelihood diverges as $\sigma\to0$.
---

The translated half-normal distribution models one-sided Gaussian deviations from a lower endpoint, which may be known or estimated.

## Details

The endpoint is $\mu$, and $\sigma$ is the parent normal scale, not the standard deviation of the observations. With both coefficients free, the endpoint estimate is the sample minimum and the scale estimate is the root mean squared distance from it. Constant samples have unbounded likelihood as scale tends to zero.

Holding the endpoint requires every observation to be at least that value. With scale free, at least one observation must exceed the endpoint. Holding positive scale removes collapse and permits singleton and constant samples. With both coefficients held, compatible data only require likelihood evaluation. The density at the endpoint is its limit from inside the support.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-halfthreshold.md) gives the boundary arguments.

## R notes

Supply `mu = 0` for the ordinary half-normal, or `mu = NULL` to estimate the translated endpoint. The internal translated half-normal functions provide evaluation, including the endpoint density `sqrt(2/pi)/sigma`. For data with a sharp maximum, fit `mlhnorm(-x, mu = NULL)` and reflect the fitted distribution.

## Decision history

On 2026-10-30 the adapter began returning the core held-scale log likelihood directly. The removed duplicate used the same sum order and formula. Support and finite-likelihood checks remain in the core fitter.
