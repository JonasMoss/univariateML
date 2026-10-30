---
id: invexp
name: Inverse exponential
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: scale
  symbol: '\theta'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dinvexp
  sampler: rinvexp
  control:
    scale: Optional held scale; NULL estimates it.
log_density: log(scale) - 2*log(x) - scale/x
references:
  - "Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley."
  - "Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37."
examples: |
  set.seed(1)
  x <- rml(200, mlinvexp())
  mlinvexp(x)
---

The inverse exponential distribution models positive heavy-tailed data.

## Details

The scale equals the exponential rate fitted on `1/x`.

A held positive scale evaluates the likelihood without estimation.
