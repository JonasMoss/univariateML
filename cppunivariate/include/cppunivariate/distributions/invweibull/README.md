---
id: invweibull
name: InverseWeibull
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  role: shape
  domain: (0, inf)
  simulate: 2.0
  kind: regular
- name: rate
  role: rate
  domain: (0, inf)
  simulate: 3.0
  kind: regular
r:
  density: actuar::dinvweibull
  sampler: actuar::rinvweibull
  control:
    shape0: Optional positive finite starting shape; `NULL` uses the automatic start.
    reltol: Relative step tolerance in log(sigma) of the underlying Gumbel fit.
    iterlim: Maximum number of iterations.
log_density: log(shape) - shape*log(rate) - (shape + 1)*log(x) - (rate*x)**(-shape)
references:
  - "Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions in Economics and Actuarial Sciences, Wiley."
  - "Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley."
  - "Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37."
examples: |
  mlinvweibull(precip)
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

The inverse Weibull distribution is the distribution of the reciprocal of a Weibull variable, used for heavy-tailed positive data.

## Details

The parameter `rate` is the reciprocal of the inverse Weibull scale, following actuar. The optional `shape0` start was documented but ignored in univariateML 1.x.
