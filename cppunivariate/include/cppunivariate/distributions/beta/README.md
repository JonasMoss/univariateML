---
id: beta
name: Beta
support: {type: real, lower: 0, upper: 1, closed: [false, false]}
params:
- name: shape1
  role: shape
  domain: (0, inf)
  simulate: 2.0
  kind: regular
- name: shape2
  role: shape
  domain: (0, inf)
  simulate: 3.0
  kind: regular
r:
  density: stats::dbeta
  sampler: stats::rbeta
  control:
    reltol: Relative step tolerance in `log(shape2)`.
    iterlim: Maximum number of iterations.
log_density: (shape1-1)*log(x) + (shape2-1)*log(1-x) - loggamma(shape1) - loggamma(shape2) + loggamma(shape1+shape2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995). Continuous Univariate Distributions, Volume 2, Chapter 25. Wiley, New York."
examples: |
  AIC(mlbeta(USArrests$Rape / 100))
holds:
- held:
  - shape1
  status: not-implemented
  reason: The adapter does not accept `shape1`; this hold is not implemented (P2).
- held:
  - shape2
  status: not-implemented
  reason: The adapter does not accept `shape2`; this hold is not implemented (P2).
---

The beta distribution models proportions and other continuous quantities between zero and one.
