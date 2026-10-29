---
id: invtrgamma
name: Inverse transformed gamma
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
  - {name: shape1, role: shape, domain: "(0, inf)", default: 3.0, fixable: true}
  - {name: shape2, role: shape, domain: "(0, inf)", default: 1.5, fixable: true}
  - {name: scale, role: scale, domain: "(0, inf)", default: 0.5}
r:
  density: actuar::dinvtrgamma
  sampler: actuar::rinvtrgamma
  control:
    reltol: Tolerance in `log(shape2)` for the one-dimensional profile search (free `shape2`).
    iterlim: Maximum number of iterations per root of the profile score.
log_density: log(shape2) + shape1*shape2*log(scale/x) - (scale/x)**shape2 - log(x) - loggamma(shape1)
limits:
  - model: lnorm
    when: '$\mathrm{shape1} \to \infty$ with $\mathrm{shape2}\sqrt{\mathrm{shape1}}$ and $\log \mathrm{scale} - \log(\mathrm{shape1})/\mathrm{shape2}$ fixed'
    status: LognormalBoundary
  - model: pareto
    when: '$\mathrm{shape2} \to \infty$ with $\mathrm{shape1}\,\mathrm{shape2}$ and scale fixed'
    status: ParetoBoundary
references:
  - "Stacy, E. W. (1962). A generalization of the gamma distribution. The Annals of Mathematical Statistics 33(3), 1187-1192. doi:10.1214/aoms/1177704481."
  - "Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley."
examples: |
  set.seed(1)
  mlinvtrgamma(actuar::rinvtrgamma(500, shape1 = 2, shape2 = 1.5, scale = 0.5))
---

The inverse transformed gamma distribution is the distribution of the
reciprocal of a transformed gamma variable; it models heavy-tailed
positive losses.

## Details

`actuar::dinvtrgamma`, `pinvtrgamma`, `qinvtrgamma` and `rinvtrgamma` are repaired at runtime; each repair switches itself off once the reference is fixed. Fitting and `logLik` do not use them.

The fit is the transformed gamma fit of `1/x` with scale `1/scale`; the
log likelihoods differ by `-2 * sum(log(x))`. The inverse gamma
(`shape2 = 1`) and inverse Weibull (`shape1 = 1`) distributions are
members. For nonconstant data, holding `shape1` or `shape2` fixed gives a maximum
likelihood estimate; the free fit has none with positive probability. The
inward likelihood slope from the lognormal face has the same sign as the
sample skewness of the logarithms. Faces win likelihood ties within `1e-8`
per observation. Fits very close to that limit have a scale above the
largest double; `mlinvtrgamma()` then stops and reports the log scale.

The C++ face helper `trgamma_faces_log` accepts logarithms of observations
and returns the maximised lognormal and power likelihoods and their fitted
parameters. The derivation and numerical search qualifications are in
`notes/evaluations/2026-10-29-trgamma-derivation.md` in the source tree.

actuar's `invtrgamma` functions lose accuracy at extreme shapes (density cancellation, infinite or zero quantiles, underflowing CDFs); see `KNOWN_ISSUES.md`. `logLik` is computed independently in C++.
