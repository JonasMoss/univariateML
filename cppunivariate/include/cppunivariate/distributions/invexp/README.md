---
id: invexp
name: Inverse exponential
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: rate
  role: rate
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: actuar::dinvexp
  sampler: actuar::rinvexp
  control: {}
log_density: -log(rate) - 2*log(x) - 1/(rate*x)
references:
  - "Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley."
  - "Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37."
holds:
- held:
  - rate
  status: not-implemented
  reason: The adapter does not accept `rate`; this hold is not implemented (P2).
---

The inverse exponential distribution models positive heavy-tailed data.

## Details

The fit is the reciprocal of the exponential fit on `1/x`.
