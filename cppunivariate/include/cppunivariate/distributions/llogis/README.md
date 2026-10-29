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
  density: actuar::dllogis
  sampler: actuar::rllogis
  control:
    abstol: Absolute step tolerance in (location, log(scale)) of the underlying logistic fit.
    iterlim: Maximum number of iterations.
log_density: log(shape) + log(rate) + (shape - 1)*log(rate*x) - 2*log(1 + (rate*x)**shape)
references:
  - "Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions in Economics and Actuarial Sciences, Wiley."
  - "Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley."
  - "Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37."
examples: |
  mlllogis(precip)
holds:
- held:
  - shape
  status: not-implemented
  reason: The adapter does not accept `shape`; this hold is not implemented (P2).
- held:
  - rate
  status: not-implemented
  reason: The adapter does not accept `rate`; this hold is not implemented (P2).
---

The log-logistic distribution describes positive data whose logarithm has a logistic distribution. It is used for size, lifetime and loss data.

## Details

The parameter `rate` is the reciprocal of scale, following actuar. Fitting requires strictly positive observations.
