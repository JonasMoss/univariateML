---
id: trgamma
name: Transformed gamma
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape1
  role: shape
  domain: (0, inf)
  simulate: 3.0
  kind: regular
- name: shape2
  role: shape
  domain: (0, inf)
  simulate: 1.5
  kind: regular
- name: scale
  role: scale
  domain: (0, inf)
  simulate: 2.0
  kind: regular
r:
  density: dtrgamma
  sampler: rtrgamma
  control:
    scale: Known positive finite coefficient; NULL estimates it.
    shape2: Known positive finite coefficient; NULL estimates it.
    shape1: Known positive finite coefficient; NULL estimates it.
    reltol: Tolerance in `log(shape2)` for the one-dimensional profile search (free `shape2`).
    iterlim: Maximum number of iterations per root of the profile score.
log_density: log(shape2) + shape1*shape2*log(x/scale) - (x/scale)**shape2 - log(x) - loggamma(shape1)
limits:
  - name: fixed-endpoint power face
    fixed: [scale]
    when: shape1 tends to zero and shape2 tends to infinity at fixed product
    status: PowerBoundary
    message: fixed-endpoint power face
  - name: unbounded likelihood
    when: A free concentration can collapse on constant observations
    detect: Constant samples; the held coefficients determine whether concentration remains free
    status: InvalidData
    message: unbounded
  - model: lnorm
    when: '$\mathrm{shape1} \to \infty$ with $\mathrm{shape2}\sqrt{\mathrm{shape1}}$ and $\log \mathrm{scale} + \log(\mathrm{shape1})/\mathrm{shape2}$ fixed'
    status: LognormalBoundary
  - model: power
    when: '$\mathrm{shape2} \to \infty$ with $\mathrm{shape1}\,\mathrm{shape2}$ and scale fixed'
    status: PowerBoundary
references:
  - "Stacy, E. W. (1962). A generalization of the gamma distribution. The Annals of Mathematical Statistics 33(3), 1187-1192. doi:10.1214/aoms/1177704481."
  - "Noufaily, A. and Jones, M. C. (2013). On maximization of the likelihood for the generalized gamma distribution. Computational Statistics 28, 505-517. doi:10.1007/s00180-012-0314-4."
  - "Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley."
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mltrgamma())
  mltrgamma(x)
---

The transformed gamma (Stacy's generalized gamma) distribution is the
distribution of `scale * G^(1/shape2)` for a unit-scale gamma variable `G`
with shape `shape1`; it models positive sizes, losses and lifetimes.

## Details

The gamma (`shape2 = 1`, scale the reciprocal gamma rate) and Weibull
(`shape1 = 1`) distributions are members, not limits. Holding `shape2`
fixed gives the gamma fit of `x^shape2`; holding `shape1` fixed gives a
strictly concave problem (the Weibull fit when `shape1 = 1`). For nonconstant data both have a maximum likelihood estimate.

The free fit has none with positive probability. The inward likelihood slope from the lognormal face has sign opposite
to the sample skewness of the logarithms. Under lognormal sampling the
asymptotic boundary probability is one half. Faces win likelihood ties
within `1e-8` per observation. Fits very close
to the lognormal limit have a scale below the smallest positive normal double;
`mltrgamma()` then stops and reports the log scale.

The C++ face helper `trgamma_faces_log` accepts logarithms of observations
and returns the maximised lognormal and power likelihoods and their fitted
parameters. The derivation and numerical search qualifications are in
`notes/evaluations/2026-10-29-trgamma-derivation.md` in the source tree.

With scale and shape2 held the shape1 score has one root. With scale and shape1 held the shape2 likelihood is strictly concave and coercive unless every observation equals scale. With only scale held the fixed-endpoint power face is compared with the interior profile; the search is numerical.
