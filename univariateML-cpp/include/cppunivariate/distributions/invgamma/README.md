---
id: invgamma
name: InvGamma
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: alpha
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: beta
  symbol: '\beta'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dinvgamma
  sampler: rinvgamma
  control:
    alpha: Optional held alpha; NULL estimates it.
    beta: Optional held beta; NULL estimates it.
    abstol: Absolute step tolerance in alpha.
    iterlim: Maximum number of iterations.
log_density: alpha*log(beta) - loggamma(alpha) - (alpha + 1)*log(x) - beta/x
references:
- 'Choi, S. C, and R. Wette. "Maximum likelihood estimation of the parameters of the gamma distribution and their
  bias." Technometrics 11.4 (1969): 683-690.'
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 17.
  Wiley, New York.
- 'Witkovsky, V. (2001). "Computing the Distribution of a Linear Combination of Inverted Gamma Variables". Kybernetika.
  37 (1): 79–90'
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
limits:
- kind: nonexistence
  name: unbounded likelihood
  when: '$\alpha$ tends to infinity with $\beta/\alpha$ equal to the common observation'
  detect: 'both coefficients are free and all observations are equal'
  status: Degenerate
  message: unbounded
examples: |
  set.seed(1)
  x <- rml(200, mlinvgamma())
  mlinvgamma(x)

group: gamma and Weibull
construction:
- transformation
relations:
  transform_of:
  - family: gamma
    transform: inverse
    at:
      shape: alpha
      scale: 1/beta
  special_case_of:
  - family: invtrgamma
    at:
      shape1: alpha
      shape2: 1
      scale: beta
fit:
  method: delegation
  certified: 'yes'
  note: 'invgamma/README.md, Details; invgamma/mle.hpp (public fitter and held overloads); invgamma/test.cpp: Gamma
    shape has a unique monotone digamma equation; scale is analytic, with bracketed fallback and held-coordinate
    solves. Transformation preserves the maximum.'
areas:
- actuarial
- reliability and survival
classification: classical
classification_reason: A standard model for positive inverse scales and loss sizes with a tunable polynomial tail.
summary: InvGamma provides a model for positive inverse scales and loss sizes with a tunable polynomial tail.
aliases:
- "inverse gamma"
- "inverted gamma"
- "scaled inverse chi-squared"
- "Vinci"
key_references:
- 4
existence:
  uniqueness: proved
  statement: >-
    For a nonempty positive sample with both coefficients free, the MLE
    exists and is unique exactly when the observations are not all equal.
    Holding either $\alpha$ or $\beta$ gives a unique finite estimate of
    the other coefficient even for constant data. Holding both coefficients
    leaves only likelihood evaluation.
  reason: >-
    The transformation $y_i=1/x_i$ gives a gamma likelihood with shape
    $\alpha$ and rate $\beta$, plus a coefficient-independent Jacobian.
    Profiling the rate gives $\beta=\alpha/\bar y$ and the equation
    $\log\alpha-\psi(\alpha)=\log\bar y-\overline{\log y}$.
    The left side decreases strictly from infinity to zero, and Jensen's
    inequality makes the right side positive exactly for nonconstant data.
    For constant data the profile is unbounded as $\alpha\to\infty$.
    With $\alpha$ held the rate formula still applies. With $\beta$ held,
    $\psi(\alpha)=\log\beta+\overline{\log y}$ has a unique root because
    the digamma function increases from minus infinity to infinity.
---

The inverse gamma distribution models positive quantities whose reciprocals follow a gamma distribution.

## Details

The coefficient $\alpha$ controls the shape and the polynomial right tail. The coefficient $\beta$ is a scale: multiplying $\beta$ by a positive constant multiplies the observations by the same constant. It is also the rate of the gamma variable $1/X$.

Fitting takes reciprocals and fits a gamma distribution, retaining the Jacobian in the reported likelihood. The free rate profiles out analytically, leaving a monotone digamma equation for the shape. Holding either coefficient permits constant data. With both free, constant data have unbounded likelihood as both coefficients grow at a fixed ratio. Holding both leaves only likelihood evaluation.

## R notes

`alpha = NULL` and `beta = NULL` estimate the corresponding coefficients. Supplied coefficients are held fixed. Empty or invalid data and unbounded likelihoods stop with an error. A reciprocal transformation outside floating-point range or a failed gamma solve also stops. `abstol` and `iterlim` control the gamma shape solve.

## Decision history

2026-10-31 (TASK-132): failures survive the reciprocal transformation. See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md) and the appended cases in `test.cpp` for the validation scope.
