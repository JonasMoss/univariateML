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
- Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition,
  Wiley.
- 'Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical
  Software, 25(7), 1-37.'
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
examples: |
  set.seed(1)
  x <- rml(200, mlinvexp())
  mlinvexp(x)

group: gamma and Weibull
construction:
- transformation
relations:
  special_case_of:
  - family: invgamma
    at:
      alpha: 1
      beta: scale
  - family: invweibull
    at:
      shape: 1
      scale: scale
  transform_of:
  - family: exp
    transform: inverse
    at:
      rate: scale
fit:
  method: delegation
  certified: 'yes'
  note: 'invexp/README.md, Details; invexp/mle.hpp (public fitter and held overloads); invexp/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- actuarial
- reliability and survival
classification: specialist
classification_reason: Used to model positive reciprocal waiting times with a fixed polynomial tail index.
summary: Inverse exponential provides a model for positive reciprocal waiting times with a fixed polynomial tail
  index.
aliases: []
key_references:
- 3
---

The inverse exponential distribution models positive heavy-tailed data.

## Details

The scale equals the exponential rate fitted on `1/x`.

A held positive scale evaluates the likelihood without estimation.
