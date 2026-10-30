---
id: llogis
name: Log-logistic
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  role: shape
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: scale
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dllogis
  sampler: rllogis
  control:
    shape: Optional held shape; NULL estimates it.
    scale: Optional held scale; NULL estimates it.
    abstol: Absolute step tolerance in (location, log(scale)) of the underlying logistic fit.
    iterlim: Maximum number of iterations.
log_density: log(shape) - log(scale) + (shape - 1)*log(x/scale) - 2*log(1 + (x/scale)**shape)
references:
  - "Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions in Economics and Actuarial Sciences, Wiley."
  - "Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley."
  - "Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37."
limits:
- name: unbounded likelihood
  fixed: [scale]
  when: Every observation equals the held scale.
  status: Unbounded
  message: unbounded
examples: |
  set.seed(1)
  x <- rml(200, mlllogis())
  mlllogis(x)
---

The log-logistic distribution describes positive data whose logarithm has a logistic distribution. It is used for size, lifetime and loss data.

## Details

Fitting requires strictly positive observations.

With scale held, the likelihood is unbounded if every observation equals that scale. Holding shape permits constant data.
