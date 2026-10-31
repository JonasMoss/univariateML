---
id: burr
name: Burr
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
  density: dburr
  sampler: rburr
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
log_density: log(shape1) + log(shape2) - log(x) - shape2*log(scale/x) - (shape1+1)*log(1+(x/scale)**shape2)
limits:
  - name: unbounded likelihood
    when: A free concentration can collapse on constant observations
    detect: Constant samples; the held coefficients determine whether concentration remains free
    status: Unbounded
    message: unbounded
  - model: weibull
    fixed: [shape2]
    when: Fixed-power Lomax profile is maximised on its exponential face
    status: WeibullBoundary
  - model: 'weibull'
    when: '$\mathrm{shape1} \to \infty$ with $\mathrm{scale}\,\mathrm{shape1}^{-1/\mathrm{shape2}}$ fixed'
    status: 'WeibullBoundary'
  - model: 'pareto'
    when: '$\mathrm{shape2} \to \infty$ with $\mathrm{shape1}\,\mathrm{shape2}$ and scale fixed'
    status: 'ParetoBoundary'
  - model: 'pareto'
    fixed: [scale]
    when: '$\mathrm{shape2} \to \infty$ with $\mathrm{shape1}\,\mathrm{shape2}$ and scale fixed'
    detect: 'all(x >= scale)'
    status: 'AllGeOne'
references:
- Shao, Q. (2004). Notes on maximum likelihood estimation for the three-parameter Burr XII distribution. Computational
  Statistics & Data Analysis 45, 675-687. doi:10.1016/S0167-9473(02)00367-5.
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994). Continuous Univariate Distributions, Volume 1, Chapter 20.
  Wiley, New York.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlburr())
  mlburr(x)

group: beta and generalised beta
construction:
- base
- generalisation
relations:
  special_case_of:
  - family: gb2
    at:
      shape1: shape1
      shape2: shape2
      shape3: 1
      scale: scale
  - family: pareto4
    at:
      shape1: shape1
      shape2: shape2
      scale: scale
      min: 0
  transform_of:
  - family: lomax
    at:
      lambda: 1/scale**shape2
      kappa: shape1
    transform: power
    power: shape2
fit:
  method: multistart
  certified: partial
  note: 'burr/README.md, Details; burr/mle.hpp (public fitter and held overloads); burr/test.cpp: One-free-coordinate
    and transformed Lomax subsets have monotone scores; free shapes/scale use numerical profiles or BFGS. Reciprocal
    delegation inherits these qualifications.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model insurance severity and size data needing a flexible polynomial upper tail.
summary: Burr provides a model for insurance severity and size data needing a flexible polynomial upper tail.
aliases:
- "Burr type XII"
- "Singh–Maddala"
- "beta-P"
key_references:
- 3
---

The Burr type XII distribution is a heavy-tailed distribution for positive size and loss data.

## Details

With scale held at $s$ and nonconstant observations all at least $s$, the Burr supremum is the endpoint-$s$ Pareto log-likelihood minus $Z\log 2$, where $Z$ counts observations equal to $s$; if every observation equals $s$, the likelihood is unbounded.

univariateML 1.x estimated only the shapes, with the scale fixed at 1.

Supply `scale = s` to estimate only the shapes. This fits the historical
scale-1 family to `x / s` and subtracts `length(x) * log(s)` from its
log likelihood. The fixed-scale path profiles shape1 and refines shape2
by Newton iteration, with a profile-likelihood fallback start. Starting
controls `shape10`, `shape20`, `scale0`, and BFGS tolerances apply only
to the free-scale path. The fixed-scale limit has endpoint `s`.

Holding shape2 gives a Lomax likelihood on x^shape2; its exponential escape is the Weibull with that power held. Holding shape1 gives a coercive log-axis location-scale likelihood for nonconstant samples. With scale also held, a free shape2 is unbounded only when every observation equals scale.
