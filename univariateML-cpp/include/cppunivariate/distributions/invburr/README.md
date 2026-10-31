---
id: invburr
name: Inverse Burr
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape1
  symbol: 'k'
  role: shape
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: shape2
  symbol: 'c'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: 's'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: dinvburr
  sampler: rinvburr
  control:
    shape2: Known positive finite coefficient; NULL estimates it.
    shape1: Known positive finite coefficient; NULL estimates it.
    abstol: Absolute Newton step tolerance in `log(shape2)` when scale is fixed.
    gradtol: Gradient tolerance for the mean profiled negative log likelihood.
    iterlim: Maximum BFGS iterations per start.
    shape10: Optional positive starting shape1; `NULL` uses automatic starts.
    shape20: Optional positive starting shape2; `NULL` uses automatic starts.
    scale0: Optional positive starting scale; `NULL` uses automatic starts.
    reltol: Relative objective tolerance for BFGS.
log_density: log(shape1) + log(shape2) - log(x) + shape2*log(scale/x) - (shape1+1)*log(1+(scale/x)**shape2)
limits:
  - name: unbounded likelihood
    when: A free concentration can collapse on constant observations
    detect: Constant samples; the held coefficients determine whether concentration remains free
    status: Unbounded
    message: unbounded
  - model: invweibull
    fixed: [shape2]
    when: Fixed-power Lomax profile is maximised on its exponential face
    status: InvWeibullBoundary
  - model: 'invweibull'
    when: '$\mathrm{shape1} \to \infty$ with $\mathrm{scale}\,\mathrm{shape1}^{1/\mathrm{shape2}}$ fixed'
    status: 'InvWeibullBoundary'
  - model: 'power'
    when: '$\mathrm{shape2} \to \infty$ with $\mathrm{shape1}\,\mathrm{shape2}$ and scale fixed'
    status: 'PowerBoundary'
  - model: 'power'
    fixed: [scale]
    when: '$\mathrm{shape2} \to \infty$ with $\mathrm{shape1}\,\mathrm{shape2}$ and scale fixed'
    detect: 'all(x <= scale)'
    status: 'AllLeOne'
references:
- Shao, Q. (2004). Notes on maximum likelihood estimation for the three-parameter Burr XII distribution. Computational
  Statistics & Data Analysis 45, 675-687. doi:10.1016/S0167-9473(02)00367-5.
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994). Continuous Univariate Distributions, Volume 1, Chapter 20.
  Wiley, New York.
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlinvburr())
  mlinvburr(x)

group: beta and generalised beta
construction:
- transformation
- generalisation
relations:
  transform_of:
  - family: burr
    transform: inverse
    at:
      shape1: shape1
      shape2: shape2
      scale: 1/scale
  - family: invpareto
    at:
      shape: shape1
      scale: scale**shape2
    transform: power
    power: shape2
  special_case_of:
  - family: gb2
    at:
      shape1: 1
      shape2: shape2
      shape3: shape1
      scale: scale
fit:
  method: delegation
  certified: partial
  note: 'invburr/README.md, Details; invburr/mle.hpp (public fitter and held overloads); invburr/test.cpp: One-free-coordinate
    and transformed Lomax subsets have monotone scores; free shapes/scale use numerical profiles or BFGS. Reciprocal
    delegation inherits these qualifications.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model insurance loss severity with the reciprocal Burr tail pattern.
summary: Inverse Burr provides a model for insurance loss severity with the reciprocal Burr tail pattern.
aliases:
- "Burr type III"
- "Dagum"
- "Dagum type I"
- "beta-K"
- "three-parameter kappa"
key_references:
- 3
---

The inverse Burr distribution is the distribution of the reciprocal of a Burr type XII variable.

## Details

With scale held at $s$ and nonconstant observations all at most $s$, the inverse Burr supremum is the endpoint-$s$ power log-likelihood minus $Z\log 2$, where $Z$ counts observations equal to $s$; if every observation equals $s$, the likelihood is unbounded.

univariateML 1.x estimated only the shapes, with the scale fixed at 1.

Supply `scale = s` to estimate only the shapes. This fits the historical
scale-1 family to `x / s` and subtracts `length(x) * log(s)` from its
log likelihood. The fixed-scale path profiles shape1 and refines shape2
by Newton iteration, with a profile-likelihood fallback start. Starting
controls `shape10`, `shape20`, `scale0`, and BFGS tolerances apply only
to the free-scale path. The fixed-scale limit has endpoint `s`.

Held-shape existence conditions are transported from Burr by reciprocation. The exponential face on x^(-shape2) is the inverse Weibull with shape2 held. A free shape2 is unbounded when every observation equals the held scale.
