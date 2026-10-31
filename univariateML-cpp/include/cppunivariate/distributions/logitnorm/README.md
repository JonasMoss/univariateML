---
id: logitnorm
name: Logit-normal
support: {type: real, lower: 0.0, upper: 1.0, closed: [false, false]}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: regular
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dlogitnorm
  sampler: rlogitnorm
  control:
    mu: Optional held mu; `NULL` estimates it.
    sigma: Optional held sigma; `NULL` estimates it.
log_density: -log(sigma) - log(2*pi)/2 - log(x) - log(1-x) - (log(x/(1-x))-mu)**2/(2*sigma**2)
references:
  - "Aitchison, J., & Shen, S. M. (1980). Logistic-normal distributions: Some properties and uses. Biometrika, 67(2), 261-272. doi:10.1093/biomet/67.2.261."
holds: []
limits:
- name: zero-scale collapse
  when: Free sigma tends to zero
  detect: All logits equal mu (estimated or held) and sigma is free
  status: zero scale
  message: does not exist
examples: |
  set.seed(1)
  x <- rml(200, mllogitnorm())
  mllogitnorm(x)

summary: The logit-normal distribution models proportions whose logits are normally distributed.
classification: specialist
classification_reason: Used for continuous proportions and logistic-normal compositional models.
aliases:
- logistic-normal
key_references:
- 1

group: bounded continuous
construction:
- transformation
relations:
  transform_of:
  - family: norm
    transform: logit
    at:
      mean: mu
      sd: sigma
fit:
  method: delegation
  certified: unknown
  note: Primary route from the 2026-10-30 fitter inventory; guarantees across held subsets are not classified.
areas:
- proportions
---

The logit-normal distribution models proportions whose logits follow a normal distribution.

## Details

2026-10-30 (TASK-125): normal log tails use the shared normal-tail helper, including its Mills expansion before subnormal loss; quantile inversion uses the same log-tail calculation. The quantile checks its monotone bracket and requires adjacent floating-point endpoints, otherwise returning NaN. Truth rows and extreme round trips validate selected tails.

The parameters `mu` and `sigma` describe the normal distribution of the logits, not the mean and standard deviation of the proportions. The scale estimate uses divisor $n$ on the logit scale.

Held coefficients apply on the logit scale. With `mu` held, the scale is the root mean squared distance of the logits from `mu`. With `sigma` held, the location is their mean, including for constant data. A free scale has no MLE exactly when every logit equals the estimated or held location. Both-held fits evaluate the likelihood.

## d/p/q/r

Sampling transforms a normal variate by the logistic function.
