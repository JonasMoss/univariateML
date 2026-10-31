---
id: invtrgamma
name: Inverse transformed gamma
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape1
  symbol: 'k'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape2
  symbol: 'p'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: 'a'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dinvtrgamma
  sampler: rinvtrgamma
  control:
    scale: Known positive finite coefficient; NULL estimates it.
    shape2: Known positive finite coefficient; NULL estimates it.
    shape1: Known positive finite coefficient; NULL estimates it.
    reltol: Tolerance in `log(shape2)` for the one-dimensional profile search (free `shape2`).
    iterlim: Maximum number of iterations per root of the profile score.
log_density: log(shape2) + shape1*shape2*log(scale/x) - (scale/x)**shape2 - log(x) - loggamma(shape1)
limits:
  - name: fixed-endpoint Pareto face
    fixed: [scale]
    when: shape1 tends to zero and shape2 tends to infinity at fixed product
    status: ParetoBoundary
    message: fixed-endpoint Pareto face
  - name: unbounded likelihood
    when: A free concentration can collapse on constant observations
    detect: Constant samples; the held coefficients determine whether concentration remains free
    status: InvalidData
    message: unbounded
  - model: lnorm
    when: '$\mathrm{shape1} \to \infty$ with $\mathrm{shape2}\sqrt{\mathrm{shape1}}$ and $\log \mathrm{scale} - \log(\mathrm{shape1})/\mathrm{shape2}$ fixed'
    status: LognormalBoundary
  - model: pareto
    when: '$\mathrm{shape2} \to \infty$ with $\mathrm{shape1}\,\mathrm{shape2}$ and scale fixed'
    status: ParetoBoundary
references:
- Stacy, E. W. (1962). A generalization of the gamma distribution. The Annals of Mathematical Statistics 33(3),
  1187-1192. doi:10.1214/aoms/1177704481.
- Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition,
  Wiley.
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlinvtrgamma())
  mlinvtrgamma(x)

group: gamma and Weibull
construction:
- transformation
- generalisation
relations:
  transform_of:
  - family: trgamma
    transform: inverse
    at:
      shape1: shape1
      shape2: shape2
      scale: 1/scale
fit:
  method: delegation
  certified: partial
  note: 'invtrgamma/README.md, Details; invtrgamma/mle.hpp (public fitter and held overloads); invtrgamma/test.cpp:
    Held power reduces to gamma and held gamma shape to a concave transformed problem; the free-power/shape profile
    and face enumeration are numerical.'
areas:
- actuarial
- reliability and survival
classification: specialist
classification_reason: Used to model insurance severity with reciprocal generalized-gamma tail behaviour.
summary: Inverse transformed gamma provides a model for insurance severity with reciprocal generalized-gamma tail
  behaviour.
aliases: []
key_references:
- 3
---

The inverse transformed gamma distribution is the distribution of the
reciprocal of a transformed gamma variable; it models heavy-tailed
positive losses.

## Details

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
`dev/research/evaluations/2026-10-29-trgamma-derivation.md` in the source tree.

Held-scale fits use the reciprocal transformed gamma profile. With shape2 also held shape1 has one score root; with shape1 held the power likelihood is strictly concave unless every observation equals scale. The held-scale Pareto face is compared numerically.
