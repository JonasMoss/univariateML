---
id: invexp
inference_numeric:
  - regime: interior
    available: true
    condition: "finite scale > 0; scale free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Inverse exponential
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: scale
  symbol: '\theta'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dinvexp
  sampler: rinvexp
  control:
    scale: Optional held scale; NULL estimates it.
log_density: log(scale) - 2*log(x) - scale/x
references:
- Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition,
  Wiley.
- 'Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical
  Software, 25(7), 1-37.'
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
examples: |
  set.seed(1)
  x <- rml(200, mlinvexp())
  mlinvexp(x)

group: gamma and Weibull
construction:
- transformation
relations:
  special_case_of:
  - family: invgamma
    at:
      alpha: 1
      beta: scale
  - family: invweibull
    at:
      shape: 1
      scale: scale
  transform_of:
  - family: exp
    transform: inverse
    at:
      rate: scale
fit:
  method: delegation
  certified: 'yes'
  note: 'invexp/README.md, Details; invexp/mle.hpp (public fitter and held overloads); invexp/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- actuarial
- reliability and survival
classification: specialist
classification_reason: Used to model positive reciprocal waiting times with a fixed polynomial tail index.
summary: Inverse exponential provides a model for positive reciprocal waiting times with a fixed polynomial tail
  index.
aliases: []
key_references:
- 3
existence:
  uniqueness: proved
  statement: >-
    Every nonempty finite positive sample has a unique finite scale MLE,
    including constant samples. Holding scale leaves likelihood evaluation.
  reason: >-
    The scale-dependent log likelihood is
    $n\log\theta-\theta\sum_i x_i^{-1}$.
    Its second derivative is $-n/\theta^2<0$, and its unique stationary
    point is $\hat\theta=n/\sum_i x_i^{-1}>0$.
---

The inverse exponential distribution models positive heavy-tailed data obtained by taking the reciprocal of an exponential variable. Its positive scale $\theta$ is the rate of that reciprocal variable.

## Details

Reciprocating the observations reduces scale estimation to an exponential rate fit. The Jacobian changes the log likelihood by a term independent of scale, so it preserves the maximizer. The estimate $n/\sum_i x_i^{-1}$ is the harmonic mean of the observations. Constant positive samples also have a unique finite estimate.

A held positive scale evaluates the likelihood without estimation.

## R notes

Supplying `scale` holds it. Reciprocals that cannot be represented finitely produce a numerical failure rather than a boundary estimate.

## Decision history

2026-10-31 (diagnostic status contract): Empty or invalid input carries a non-success status. Delegate failures survive the reciprocal transformation. See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md) and the appended TASK-132 cases in `test.cpp` for the validation scope.
