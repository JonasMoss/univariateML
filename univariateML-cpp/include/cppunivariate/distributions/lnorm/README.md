---
id: lnorm
inference_numeric:
  - regime: interior
    available: true
    condition: "finite meanlog and sdlog > 0; either or both free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
  - regime: boundary or limit
    available: false
    condition: "sdlog = 0 or nonfinite parameters"
    reason: "zero scale and infinite coefficients are outside the interior model"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
name: Lognormal
support: {type: real, lower: 0.0, upper: inf, closed: [false, false]}
params:
- name: meanlog
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sdlog
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: stats::dlnorm
  sampler: stats::rlnorm
  control:
    meanlog: Optional held meanlog; NULL estimates it.
    sdlog: Optional held sdlog; NULL estimates it.
log_density: -log(x) - log(sdlog) - log(2*pi)/2 - (log(x)-meanlog)**2/(2*sdlog**2)
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 14.
  Wiley, New York.
limits:
- kind: nonexistence
  name: unbounded likelihood
  when: '$\mu$ equals the common log observation and $\sigma$ tends to zero'
  detect: 'both coefficients are free and all observations are equal'
  status: Unbounded
  message: unbounded
- name: unbounded likelihood
  fixed: [meanlog]
  when: 'the free log scale $\sigma$ tends to zero with log mean $\mu$ held'
  detect: 'every $\log x_i$ equals the held $\mu$ and $\sigma$ is free'
  status: Unbounded
  message: unbounded
examples: |
  x <- datasets::precip
  mllnorm(x)

group: normal and Student
construction:
- transformation
relations:
  transform_of:
  - family: norm
    transform: log
    at:
      mean: meanlog
      sd: sdlog
fit:
  method: delegation
  certified: 'yes'
  note: 'lnorm/README.md, Details; lnorm/mle.hpp (public fitter and held overloads); lnorm/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- finance
- measurement and errors
classification: classical
classification_reason: A standard model for positive multiplicative variation represented by Gaussian logarithms.
summary: Lognormal provides a model for positive multiplicative variation represented by Gaussian logarithms.
aliases:
- "log-normal"
- "Galton"
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    For a nonempty positive sample with both coefficients free, the MLE
    exists and is unique exactly when the observations are not all equal.
    With $\mu$ held and $\sigma$ free, it exists and is unique unless every
    $\log x_i$ equals $\mu$. With $\sigma$ held, the free mean has a unique
    MLE even for constant data. Holding both coefficients leaves only
    likelihood evaluation.
  reason: >-
    Put $y_i = \log x_i$. The Jacobian is independent of the coefficients,
    so maximization is the normal likelihood problem. For each positive
    $\sigma$, the unique free mean is $\bar y$. For a fixed or fitted mean,
    write $Q = \sum_i(y_i-\mu)^2$. The scale likelihood is
    $-n\log\sigma-Q/(2\sigma^2)$ plus a constant. If $Q>0$, its unique
    maximum is at $\sigma^2=Q/n$; if $Q=0$, it is unbounded as
    $\sigma\to0$.
---

The lognormal distribution models positive quantities whose logarithms follow a normal distribution.

## Details

The coefficient $\mu$ is the mean of the logarithms and $\sigma$ is their standard deviation. Neither is the mean or standard deviation on the original scale. Fitting takes logarithms and fits a normal distribution, retaining the Jacobian in the reported likelihood. With both coefficients free, the fitted log mean is the sample mean of the logarithms and the fitted log variance uses divisor $n$.

Holding $\mu$ makes the scale estimate the root mean square log residual about that supplied mean. If every log residual is zero, shrinking $\sigma$ makes the likelihood unbounded. Holding a positive $\sigma$ removes this collapse and permits constant data.

## R notes

`meanlog = NULL` and `sdlog = NULL` estimate the corresponding coefficients. Supplied coefficients are held fixed. Both held coefficients evaluate the likelihood without estimation. Invalid or empty data and unbounded likelihoods stop with an error. Numerical failures from the normal fit are preserved.

## Decision history

2026-10-30 (TASK-125): when squared log residuals overflow, divide them by their maximum magnitude before summation, then restore the scale. The held-mean 1e300 regression exercises this path.

2026-10-31 (TASK-132): the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md) records invalid-input, degeneracy and delegate-failure behavior. The appended cases in `test.cpp` establish the tested scope.
