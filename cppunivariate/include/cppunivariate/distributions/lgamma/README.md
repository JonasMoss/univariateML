---
id: lgamma
name: Loggamma
support: {type: real, lower: 1, upper: inf, closed: [false, false]}
params:
- name: shapelog
  role: shape
  domain: (0, inf)
  simulate: 2.0
  kind: regular
- name: ratelog
  role: rate
  domain: (0, inf)
  simulate: 3.0
  kind: regular
r:
  density: actuar::dlgamma
  sampler: actuar::rlgamma
  control:
    abstol: Absolute step tolerance in shapelog.
    iterlim: Maximum number of iterations.
log_density: shapelog*log(ratelog) - loggamma(shapelog) + (shapelog - 1)*log(log(x)) - (ratelog + 1)*log(x)
references:
  - "Hogg, R. V. and Klugman, S. A. (1984), Loss Distributions, Wiley."
  - "Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37."
examples: |
  mllgamma(precip)
holds:
- held:
  - shapelog
  status: not-implemented
  reason: The adapter does not accept `shapelog`; this hold is not implemented (P2).
- held:
  - ratelog
  status: not-implemented
  reason: The adapter does not accept `ratelog`; this hold is not implemented (P2).
---

The log-gamma distribution describes positive data whose logarithm follows a gamma distribution.

## Details

The support is above 1: `log(x)` has gamma shape `shapelog` and rate `ratelog`. This is the actuar log-gamma convention, rather than the distribution of the logarithm of a gamma variable.
