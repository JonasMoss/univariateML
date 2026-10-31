---
id: unif
name: Uniform
support: {type: real, lower: "min", upper: "max", closed: [true, true], endpoint_log_density: {lower: "-log(max-min)", upper: "-log(max-min)"}}
params:
- name: min
  symbol: 'a'
  role: lower
  domain: (-inf, max)
  simulate: 0.0
  bound: lower
  kind: endpoint
- name: max
  symbol: 'b'
  role: upper
  domain: (min, inf)
  simulate: 1.0
  bound: upper
  kind: endpoint
r:
  density: stats::dunif
  sampler: stats::runif
  control:
    min: Optional held min; `NULL` estimates it.
    max: Optional held max; `NULL` estimates it.
log_density: -log(max-min)
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 26.
  Wiley, New York.
holds: []
limits:
- name: zero-width collapse
  when: Free interval width tends to zero.
  detect: Constant data and no held endpoint strictly outside the data.
  status: Unbounded
  message: does not exist
- name: incompatible held endpoints
  when: A held endpoint excludes observations or min is not below max.
  detect: Held min exceeds the sample minimum or held max is below the sample maximum, or min >= max.
  status: Incompatible
  message: held uniform
examples: |
  set.seed(1)
  x <- rml(200, mlunif())
  mlunif(x)

group: bounded continuous
construction:
- base
relations:
  special_case_of:
  - family: nsbeta
    at:
      shape1: 1
      shape2: 1
      min: min
      max: max
fit:
  method: closed-form
  certified: 'yes'
  note: 'unif/README.md, Details; unif/mle.hpp (public fitter and held overloads); unif/test.cpp: Closed-form maximum
    for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- proportions
classification: classical
classification_reason: A standard model for continuous values with equal density over a bounded interval.
summary: Uniform provides a model for continuous values with equal density over a bounded interval.
aliases: []
key_references:
- 1
---

The uniform distribution assigns constant density to a bounded interval.

## Details

The endpoint estimates are `min(x)` and `max(x)`, so the MLE lies on the boundary determined by the sample order statistics. The fitted support is the closed interval between these endpoints, which must be distinct.

A free endpoint is its sample extreme; a held endpoint must contain every observation. A constant sample has a finite maximum exactly when a held endpoint lies strictly outside the observation. Both-held intervals must have positive width and contain the sample; otherwise fitting stops.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
