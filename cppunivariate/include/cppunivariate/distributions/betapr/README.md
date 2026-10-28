---
id: betapr
name: BetaPrime
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
  - {name: shape1, role: shape, domain: "(0, inf)", default: 2.0}
  - {name: shape2, role: shape, domain: "(0, inf)", default: 3.0}
r:
  density: extraDistr::dbetapr
  sampler: extraDistr::rbetapr
  control:
    reltol: Relative step tolerance in `log(shape2)`.
    iterlim: Maximum number of iterations.
log_density: (shape1-1)*log(x) - (shape1+shape2)*log(1+x) - loggamma(shape1) - loggamma(shape2) + loggamma(shape1+shape2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995). Continuous Univariate Distributions, Volume 2, Chapter 25. Wiley, New York."
examples: |
  AIC(mlbetapr(USArrests$Rape))
---

The beta prime distribution models positive ratios.

## Details

Only the two shape parameters are estimated; the scale is fixed at 1. If $Y$ has a beta distribution, then $Y/(1-Y)$ has a beta prime distribution.
