---
id: expweibull
name: Exponentiated Weibull
stage: dpqr
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- {name: scale, symbol: a, role: scale, domain: '(0, inf)', simulate: 2, kind: regular}
- {name: shape, symbol: b, role: shape, domain: '(0, inf)', simulate: 1.5, kind: regular}
- {name: exponent, symbol: c, role: shape, domain: '(0, inf)', simulate: 2.5, kind: regular}
r:
  density: dexpweibull
  sampler: rexpweibull
log_density: log(shape) + log(exponent) - log(scale) + (shape - 1)*log(x/scale) - (x/scale)**shape + (exponent - 1)*log(1 - exp(-(x/scale)**shape))
---

For $x>0$, the CDF is $[1-\exp\{-(x/a)^b\}]^c$.
`scale` and `shape` retain the Weibull coefficient names; `exponent`
distinguishes the CDF power from the Weibull shape. The scalar C++ order is
`scale`, `shape`, `exponent`. Setting `exponent = 1` gives Weibull.
Only d/p/q/r are available; the fitter is pending TASK-180.
