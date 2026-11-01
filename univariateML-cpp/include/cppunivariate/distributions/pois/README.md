---
id: pois
inference_numeric:
  - regime: interior
    available: true
    condition: "finite lambda > 0; lambda free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
  - regime: boundary or limit
    available: false
    condition: "lambda = 0 or lambda nonfinite"
    reason: "zero rate is an attained boundary and infinite rate is not an interior model"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
name: Poisson
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: stats::dpois
  sampler: stats::rpois
  control:
    lambda: "Optional held lambda; NULL estimates it."
log_density: "x*log(lambda) - lambda - loggamma(x + 1)"
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
examples: |
  x <- datasets::discoveries
  mlpois(x)

group: Poisson, binomial and negative binomial
construction:
- base
relations:
  special_case_of:
  - family: cmp
    at:
      lambda: lambda
      nu: 1
  - family: skellam
    at:
      mu1: lambda
      mu2: 0
  transform_of:
  - family: skellam
    at:
      mu1: 0
      mu2: lambda
    transform: negate
fit:
  method: closed-form
  certified: 'yes'
  note: 'pois/README.md, Details; pois/mle.hpp (public fitter and held overloads); pois/test.cpp: Closed-form maximum
    for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- counts and ecology
classification: classical
classification_reason: A standard model for independent event counts at constant intensity.
summary: Poisson provides a model for independent event counts at constant intensity.
aliases: []
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    For every nonempty sample of nonnegative integer counts, the free-rate
    MLE exists and is unique, including the boundary estimate $\hat\lambda=0$
    for all-zero data. A held positive rate gives positive likelihood for
    every such sample. A held zero rate is compatible only with all-zero data.
  reason: >-
    Writing $m$ for sample size and $S$ for the sum, the parameter-dependent
    log likelihood is $S\log\lambda-m\lambda$ for $\lambda>0$.
    If $S>0$, it is strictly concave and maximized at $S/m$.
    If $S=0$, it decreases strictly and is maximized at the included endpoint zero.

---

The Poisson distribution models counts of independent events over a fixed observation interval.

## Details

The rate $\lambda$ describes the expected event count over that interval.
Its estimate is the sample mean. All-zero data give the included boundary
estimate $\hat\lambda=0$, a point mass at zero.

Holding the rate evaluates the likelihood at the supplied value. A positive
held rate is compatible with every nonnegative integer sample. A held zero
rate requires every observation to be zero. Values outside the parameter
domain or incompatible with the counts are rejected.

## Decision history

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
