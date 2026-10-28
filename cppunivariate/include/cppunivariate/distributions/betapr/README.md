---
id: betapr
name: BetaPrime
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
  - {name: shape1, role: shape, domain: "(0, inf)", default: 2.0}
  - {name: shape2, role: shape, domain: "(0, inf)", default: 3.0}
  - {name: scale, role: scale, domain: "(0, inf)", default: 1.0}
r:
  density: extraDistr::dbetapr
  sampler: extraDistr::rbetapr
  control:
    gradtol: Tolerance on the norm of the total negative log-likelihood gradient in log coordinates.
    reltol: Relative objective tolerance.
    abstol: Step tolerance in log coordinates.
    shape10: Optional positive starting value for shape1 (zero selects automatic starts).
    shape20: Optional positive starting value for shape2 (zero selects automatic starts).
    scale0: Optional positive starting value for scale (zero selects automatic starts).
    iterlim: Maximum number of iterations.
log_density: (shape1-1)*log(x/scale) - log(scale) - (shape1+shape2)*log(1+x/scale) - loggamma(shape1) - loggamma(shape2) + loggamma(shape1+shape2)
limits:
  - {model: gamma, when: 'shape2 tends to infinity with scale/shape2 fixed', status: GammaBoundary}
  - {model: invgamma, when: 'shape1 tends to infinity with scale*shape1 fixed', status: InvGammaBoundary}
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995). Continuous Univariate Distributions, Volume 2, Chapter 25. Wiley, New York."
examples: |
  set.seed(313)
  mlbetapr(extraDistr::rbetapr(1000, 1, 2, scale = 3))
---

The beta prime distribution models positive ratios.

## Details

univariateML 1.x estimated only the shapes, with the scale fixed at 1.
