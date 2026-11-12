---
id: unif
inference_numeric:
  - regime: "endpoint free"
    available: false
    condition: "finite min < max; at least one endpoint free"
    reason: "moving endpoints are non-DQM and no regular free coefficient remains when both are held"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support"
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
  density: core_dunif
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
  firmness: proved
  when: The free width $b-a$ tends to zero around the common observation.
  detect: The sample is constant, at least one endpoint is free, and no held endpoint lies strictly outside the observation.
  status: Unbounded
  message: does not exist
- name: incompatible held endpoints
  firmness: proved
  when: A held endpoint excludes an observation.
  detect: The held lower endpoint exceeds the sample minimum or the held upper endpoint is below the sample maximum.
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
  - family: tlambda
    at:
      mu: (min+max)/2
      sigma: (max-min)/2
      lambda: 1
  - family: gpd
    at:
      mu: min
      sigma: max-min
      xi: -1
  transform_of:
  - family: power
    at:
      alpha: max-min
      beta: 1
    transform: shift
    shift: min
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
aliases:
- "rectangular"
- "continuous uniform"
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    With both endpoints free, a finite MLE exists exactly when the sample is nonconstant, and it is unique. With either endpoint held, each free endpoint is its sample extreme. This gives a unique finite fit exactly when the resulting interval has positive width and contains the sample. With both endpoints held, only compatibility is evaluated.
  reason: >-
    For a nonempty finite sample inside the interval, the likelihood is $(b-a)^{-n}$. Each free endpoint must therefore minimize the width. For constant data, a compatible held endpoint strictly outside the observation prevents collapse; otherwise shrinking the width makes the likelihood unbounded.
---

The uniform distribution assigns constant density between a lower endpoint $a$ and an upper endpoint $b$.

## d/p/q/r

### Logarithmic probabilities near the center

The binary64 value -log(2) represents a probability slightly different from one half. If the endpoints cancel, rounding that probability to one half first loses the entire quantile. Central inversion retains the low part of log(2) and the compensated endpoint midpoint.

## Details

Each free endpoint is estimated by the corresponding sample extreme. Holding an endpoint specifies a known limit of the measurement range. The fitted interval is closed and must have positive width. A constant sample has a finite fit only when a compatible held endpoint lies strictly outside the observation. With both endpoints held, the interval must contain every observation.

The [bounded-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-bounded.md) gives the width-minimization argument and all held cases.

## R notes

Invalid endpoint ordering (`min >= max`) is rejected as invalid input. An endpoint that excludes observations gives a separate compatibility error.

## Decision history

The diagnostic contract was recorded on 2026-10-31. Empty or invalid input has a non-success status, and constant-sample behavior depends on the held endpoints. See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md) and the TASK-132 cases in `test.cpp` for the validation scope.
