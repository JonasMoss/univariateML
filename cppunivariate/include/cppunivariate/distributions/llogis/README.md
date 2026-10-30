---
id: llogis
name: Loglogistic
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  role: shape
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: rate
  role: rate
  domain: (0, inf)
  simulate: 2.0
  kind: regular
r:
  density: dllogis
  sampler: rllogis
  control:
    shape: Optional held shape; NULL estimates it.
    rate: Optional held rate; NULL estimates it.
    abstol: Absolute step tolerance in (location, log(scale)) of the underlying logistic fit.
    iterlim: Maximum number of iterations.
log_density: log(shape) + log(rate) + (shape - 1)*log(rate*x) - 2*log(1 + (rate*x)**shape)
references:
  - "Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions in Economics and Actuarial Sciences, Wiley."
  - "Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley."
  - "Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37."
examples: |
  mlllogis(precip)
limits:
- name: unbounded likelihood
  fixed: [rate]
  when: Every observation equals the reciprocal held rate.
  status: Unbounded
  message: unbounded
---

The log-logistic distribution describes positive data whose logarithm has a logistic distribution. It is used for size, lifetime and loss data.

## Details

The parameter `rate` is the reciprocal of scale, following actuar. Fitting requires strictly positive observations.

With rate held, the likelihood is unbounded if every observation equals 1/rate. Holding shape permits constant data.
