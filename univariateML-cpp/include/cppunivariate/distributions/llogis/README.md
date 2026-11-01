---
id: llogis
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
name: Log-logistic
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  symbol: '\beta'
  role: shape
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: scale
  symbol: '\alpha'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dllogis
  sampler: rllogis
  control:
    shape: Optional held shape; NULL estimates it.
    scale: Optional held scale; NULL estimates it.
    abstol: Absolute step tolerance in (location, log(scale)) of the underlying logistic fit.
    iterlim: Maximum number of iterations.
log_density: log(shape) - log(scale) + (shape - 1)*log(x/scale) - 2*log(1 + (x/scale)**shape)
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
  fixed: [scale]
  when: Every observation equals the held scale.
  status: Unbounded
  message: unbounded
examples: |
  set.seed(1)
  x <- rml(200, mlllogis())
  mlllogis(x)

group: beta and generalised beta
construction:
- base
relations:
  special_case_of:
  - family: burr
    at:
      shape1: 1
      shape2: shape
      scale: scale
  - family: invburr
    at:
      shape1: 1
      shape2: shape
      scale: scale
  - family: pareto3
    at:
      shape: shape
      scale: scale
      min: 0
  transform_of:
  - family: logis
    transform: log
    at:
      location: log(scale)
      scale: 1/shape
fit:
  method: delegation
  certified: 'yes'
  note: 'llogis/README.md, Details; llogis/mle.hpp (public fitter and held overloads); llogis/test.cpp: Log transform
    transfers to the unique logistic maximum; held shape gives monotone log-location and held scale a monotone shape
    score, both bisected.'
areas:
- reliability and survival
classification: specialist
classification_reason: Used to model survival times with a nonmonotone hazard and polynomial upper tail.
summary: Log-logistic provides a model for survival times with a nonmonotone hazard and polynomial upper tail.
aliases:
- "Fisk"
key_references:
- 4
---

The log-logistic distribution describes positive data whose logarithm has a logistic distribution. It is used for size, lifetime and loss data.

## Details

Fitting requires strictly positive observations.

With scale held, the likelihood is unbounded if every observation equals that scale. Holding shape permits constant data.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
