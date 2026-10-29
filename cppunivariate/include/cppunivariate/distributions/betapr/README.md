---
id: betapr
name: BetaPrime
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
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
- name: scale
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: extraDistr::dbetapr
  sampler: extraDistr::rbetapr
  control:
    gradtol: Tolerance on the total gradient norm in log coordinates after dividing data by their geometric mean; with fixed scale, relative Newton step tolerance on log(shape2).
    reltol: Relative objective tolerance.
    abstol: Step tolerance in log coordinates after dividing data by their geometric mean.
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

The beta prime distribution models positive ratios.

## Details

univariateML 1.x estimated only the shapes, with the scale fixed at 1.

Supply `scale` to hold it at a positive finite value. With scale fixed,
there are no nondegenerate limit-model escapes.
For positive finite nonconstant data the fixed-scale MLE exists uniquely,
by the beta transformation $y=x/(s+x)$. Constant data have an unbounded
likelihood as both shapes grow with their ratio fixed.
With scale held, sending only shape2 to infinity collapses the distribution
at zero; sending only shape1 to infinity sends its mass to infinity.
Sending both shapes to infinity with a finite positive ratio gives a point
mass, rather than a gamma or inverse-gamma distribution.
