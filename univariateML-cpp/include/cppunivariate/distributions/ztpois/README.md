---
id: ztpois
inference_numeric:
  - regime: interior
    available: true
    condition: "finite lambda > 0; lambda free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-truncated Poisson
support:
  type: integer
  lower: 1
  upper: inf
  closed:
  - true
  - false
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dztpois
  sampler: rztpois
  control:
    lambda: "Optional held lambda; NULL estimates it."
log_density: -lambda+x*log(lambda)-loggamma(x+1)-log(1-exp(-lambda))
examples: |
  x <- corbet
  mlztpois(x)

group: modified counts
construction:
- truncation
relations:
  modification_of:
  - family: pois
    at:
      lambda: lambda
    type: zero-truncated
  special_case_of:
  - family: zmpois
    at:
      lambda: lambda
      p0: 0
fit:
  method: scalar-solve
  certified: 'yes'
  note: 'ztpois/README.md, Details; ztpois/mle.hpp (public fitter and held overloads); ztpois/test.cpp: Conditional
    mean is strictly increasing in log rate (derivative is conditional variance), giving a unique bracketed root;
    bounds profile to sample extremes and zero mass separates analytically.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model Poisson counts sampled only when positive.
summary: Zero-truncated Poisson provides a model for Poisson counts sampled only when positive.
aliases:
- "positive Poisson"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
---

Zero-truncated Poisson distributions model positive counts.

## Details

Actuar defines the point mass at one at `lambda = 0`. This boundary is returned as an attained member.

Supplying `lambda` evaluates the likelihood at that value. Held values outside the parameter domain or incompatible with the observed counts stop.
