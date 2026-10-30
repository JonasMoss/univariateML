---
id: invweibull
name: Inverse Weibull
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  symbol: 'k'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: '\lambda'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dinvweibull
  sampler: rinvweibull
  control:
    shape: Optional held shape; NULL estimates it.
    scale: Optional held scale; NULL estimates it.
    shape0: Optional positive finite starting shape; `NULL` uses the automatic start.
    reltol: Relative step tolerance in log(sigma) of the underlying Gumbel fit.
    iterlim: Maximum number of iterations.
log_density: log(shape) + shape*log(scale) - (shape + 1)*log(x) - (x/scale)**(-shape)
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
  x <- rml(200, mlinvweibull())
  mlinvweibull(x)
---

The inverse Weibull distribution is the distribution of the reciprocal of a Weibull variable, used for heavy-tailed positive data.

## Details

The optional `shape0` start was documented but ignored in univariateML 1.x.

With scale held, the likelihood is unbounded if every reciprocal observation equals `1/scale`. Holding shape permits constant data.
