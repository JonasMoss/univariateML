---
id: invweibull
inference_numeric:
  - regime: interior
    available: true
    condition: "finite shape > 0 and scale > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Inverse Weibull
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  symbol: 'k'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: '\lambda'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dinvweibull
  sampler: rinvweibull
  control:
    shape: Optional held shape; NULL estimates it.
    scale: Optional held scale; NULL estimates it.
    shape0: Optional positive finite starting shape; `NULL` uses the automatic start.
    reltol: Relative step tolerance in log(sigma) of the underlying Gumbel fit.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: log(shape) + shape*log(scale) - (shape + 1)*log(x) - (x/scale)**(-shape)
references:
- Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions in Economics and Actuarial Sciences, Wiley.
- Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition,
  Wiley.
- 'Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical
  Software, 25(7), 1-37.'
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
limits:
- name: unbounded likelihood
  firmness: proved
  fixed: [scale]
  when: 'The free shape $k$ tends to infinity while scale $\lambda$ is held at the common observation value.'
  detect: 'Every observation equals the held scale $\lambda$.'
  status: Unbounded
  message: unbounded
examples: |
  set.seed(1)
  x <- rml(200, mlinvweibull())
  mlinvweibull(x)

group: gamma and Weibull
construction:
- transformation
relations:
  special_case_of:
  - family: invtrgamma
    at:
      shape1: 1
      shape2: shape
      scale: scale
  - family: frechet
    at:
      lambda: shape
      mu: 0
      sigma: scale
  transform_of:
  - family: weibull
    transform: inverse
    at:
      shape: shape
      scale: 1/scale
  - family: gumbel
    at:
      mu: log(scale)
      sigma: 1/shape
    transform: log
fit:
  method: delegation
  certified: 'yes'
  note: 'invweibull/README.md, Details; invweibull/mle.hpp (public fitter and held overloads); invweibull/test.cpp:
    Gumbel location is profiled exactly and its scale equation has a unique root; held location has a strictly concave
    inverse-scale likelihood. Log/reciprocal transformations preserve this structure.'
areas:
- actuarial
- reliability and survival
classification: specialist
classification_reason: Used to model heavy-tailed lifetimes and positive extreme measurements.
summary: Inverse Weibull provides a model for heavy-tailed lifetimes and positive extreme measurements.
aliases:
- "inverted Weibull"
- "two-parameter Fréchet"
key_references:
- 4
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a unique finite MLE exists for every
    nonconstant positive sample. Constant samples have unbounded likelihood.
    Holding shape gives a unique finite scale estimate, including for constants.
    Holding scale gives a unique finite shape estimate unless every observation
    equals that scale, when the likelihood is unbounded. Holding both leaves
    only likelihood evaluation.
  reason: >-
    The reciprocal observations follow a Weibull law with shape $k$ and
    scale $1/\lambda$. The reciprocal transformation has a parameter-independent
    Jacobian and preserves likelihood maximizers and held coordinates.
    The Weibull profiled shape score has derivative $-n/k^2$ minus a
    nonnegative weighted variance. With scale held, its shape score has
    derivative $-n/k^2-\sum_i r_i^2e^{kr_i}<0$, where
    $r_i=\log(\lambda/x_i)$. These scores cross zero except in the stated
    constant-sample cases.
---

The inverse Weibull distribution models heavy-tailed positive measurements, including lifetimes and extreme values. It is the distribution of the reciprocal of a Weibull variable, and its scale $\lambda$ sets the measurement unit.

## Details

Smaller shape $k$ gives a heavier upper tail.

The joint fit uses a Gumbel model for the logarithms of the observations. Profiling its location leaves a scalar equation with a unique root for nonconstant data. Constant data instead allow the shape to increase without bound with scale at their common value.

Holding shape leaves an explicit scale estimate. Holding scale leaves a strictly decreasing shape score, solved by bracketing. This conditional fit permits constant data unless their common value equals the held scale. Holding both coefficients evaluates the likelihood. A failed delegated solve remains a fitting failure.

The [positive-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-positive.md) gives the boundary arguments and their held-parameter conditions.

## R notes

The optional `shape0` starting value is used and must be positive and finite.

## Decision history

The `shape0` argument was documented but ignored in univariateML 1.x; it is functional in the current fitter.

2026-10-31 (diagnostic contract): Empty or invalid input carries a non-success status. Degeneracy depends on which coefficients are free, and delegate failures survive transformations. See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md) and the TASK-132 cases in `test.cpp` for the validation scope.
