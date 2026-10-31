---
id: rayleigh
name: Rayleigh
support: {type: real, lower: 0.0, upper: inf, closed: [true, false]}
params:
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: drayleigh
  sampler: rrayleigh
  control:
    sigma: Optional held sigma; NULL estimates it.
log_density: log(x) - 2*log(sigma) - x**2/(2*sigma**2)
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 18.
  Wiley, New York.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
examples: |
  set.seed(1)
  x <- rml(200, mlrayleigh())
  mlrayleigh(x)

group: gamma and Weibull
construction:
- base
relations:
  special_case_of:
  - family: weibull
    at:
      shape: 2
      scale: sqrt(2)*sigma
  - family: naka
    at:
      shape: 1
      scale: 2*sigma**2
fit:
  method: closed-form
  certified: 'yes'
  note: 'rayleigh/README.md, Details; rayleigh/mle.hpp (public fitter and held overloads); rayleigh/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- reliability and survival
- waiting times and queues
classification: classical
classification_reason: A standard model for amplitudes formed from two independent centred Gaussian components.
summary: Rayleigh provides a model for amplitudes formed from two independent centred Gaussian components.
aliases: []
key_references:
- 2
existence:
  uniqueness: proved
  statement: >-
    Every nonempty strictly positive sample has a unique MLE for the free
    scale. If any observation is zero, the likelihood is zero for every
    scale, so every scale ties and there is no unique estimate. Such samples
    are rejected. With the scale held, only the likelihood is evaluated,
    and zero observations are still rejected.
  reason: >-
    For positive data put $t=\sigma^2$ and $S=\sum_i x_i^2>0$.
    Apart from a constant the log-likelihood is $-n\log t-S/(2t)$.
    Its derivative $(S/2-nt)/t^2$ is positive below $S/(2n)$ and negative
    above it, proving existence and uniqueness. At zero the density is
    zero for every positive scale, making the entire sample likelihood zero.
---

The Rayleigh distribution describes the magnitude of a pair of independent centred normal variables with a common standard deviation $\sigma$.

## Details

The scale estimate is the square root of half the empirical second moment, with divisor $n$. It exists even for a positive constant sample or a single positive observation.

An observation at zero makes the likelihood identically zero for every scale. The sample therefore provides no likelihood preference among scales, and the fit rejects it. Holding a positive scale leaves only likelihood evaluation, with the same rejection of zero observations.

## R notes

`sigma = NULL` estimates the scale. A supplied positive finite `sigma` is held fixed. Any zero observation stops with an error, including when `sigma` is held.
