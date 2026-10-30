---
id: yulesimon
name: Yule--Simon
support: {type: integer, lower: 1, upper: inf, closed: [true, false]}
params:
- name: rho
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
r:
  density: dyulesimon
  sampler: ryulesimon
  control:
    rho: "Optional held coefficient; NULL estimates it."
    reltol: "Relative tolerance for the bracketed scalar solve."
    iterlim: "Maximum number of bisection iterations."
log_density: "log(rho) + loggamma(x) + loggamma(rho+1) - loggamma(x+rho+1)"
limits:
  - name: point mass at one
    when: '$rho \to +\infty$'
    detect: all observations equal one
    status: PointMass
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlyulesimon())
  mlyulesimon(x)
---

The Yule--Simon distribution describes power-law frequencies on the positive integers.

## Details

This is the beta negative binomial law with `size = 1`, `alpha = rho`, `beta = 1`, shifted upward by one. In log shape the score is strictly decreasing from the sample size to the sample size minus the sum of the observations. A unique finite MLE therefore exists whenever any observation exceeds one. As `rho` decreases to zero the likelihood tends to zero; probability escapes to infinity rather than approaching a proper limiting law.

All-ones samples have an unattained point-mass supremum at one and the fitter stops. Other constant samples have finite fits. Package-defined probability functions evaluate upper tails directly and use bounded-memory integer inversion. Beyond 2^53, quantiles use the representable double grid; adjacent integers cannot be distinguished, and quantiles exceeding the largest finite double are returned as infinity.

With the coefficient held, the likelihood is evaluated directly, including all-ones samples.

## d/p/q/r

Support starts at one with no finite upper endpoint; the double grid above 2^53 may skip integers and overflowed quantiles return Inf.
