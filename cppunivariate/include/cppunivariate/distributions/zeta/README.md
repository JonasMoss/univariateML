---
id: zeta
name: Zeta
support: {type: integer, lower: 1, upper: inf, closed: [true, false]}
params:
- name: s
  role: shape
  domain: (1, inf)
  simulate: 2
  kind: regular
r:
  export:
    params:
      s: Finite shape in (1, inf).
    details: Support starts at one and has no finite upper endpoint. Quantiles beyond 2^53 use
      the representable double grid; a result beyond the largest finite double is Inf. At that
      scale, adjacent integers cannot be distinguished. Random draws use inversion.
  density: dzeta
  sampler: rzeta
  control:
    s: "Optional held coefficient; NULL estimates it."
    reltol: "Relative tolerance for the bracketed scalar solve."
    iterlim: "Maximum number of bisection iterations."
log_density: "-s*log(x) - log(zeta(s))"
limits:
  - name: point mass at one
    when: '$s \to +\infty$'
    detect: all observations equal one
    status: PointMass
examples: |
  mlzeta(c(1, 1, 2, 3, 8))
holds: []
---

The zeta distribution describes power-law frequencies on the positive integers.

## Details

This is the infinite-support face of finite Zipf: for fixed `s > 1`, its probabilities are the limits as `N` tends to infinity. The log-likelihood is strictly concave in `s`: its score is the sample size times the difference between the model and empirical means of `log(x)`. The model mean decreases from infinity to zero. Thus the finite MLE is unique unless all observations are one. As `s` decreases to one the likelihood tends to zero, so that endpoint never maximizes a finite sample likelihood.

All-ones samples have an unattained point-mass supremum at one and the fitter stops. Other constant samples have finite fits. Package-defined probability functions evaluate upper tails directly and use bounded-memory integer inversion. Beyond 2^53, quantiles use the representable double grid; adjacent integers cannot be distinguished, and quantiles exceeding the largest finite double are returned as infinity.

With the coefficient held, the likelihood is evaluated directly, including all-ones samples.
