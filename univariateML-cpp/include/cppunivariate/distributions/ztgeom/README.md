---
id: ztgeom
inference_numeric:
  - regime: interior
    available: true
    condition: "0 < prob < 1; prob free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-truncated geometric
support:
  type: integer
  lower: 1
  upper: inf
  closed:
  - true
  - false
params:
- name: prob
  symbol: 'p'
  role: probability
  domain: (0, 1]
  simulate: 0.5
  kind: regular
r:
  density: dztgeom
  sampler: rztgeom
  control:
    prob: "Optional held prob; NULL estimates it."
log_density: log(prob)+(x-1)*log(1-prob)
examples: |
  x <- corbet
  mlztgeom(x)

group: modified counts
construction:
- truncation
relations:
  modification_of:
  - family: geom
    at:
      prob: prob
    type: zero-truncated
  special_case_of:
  - family: ztnbinom
    at:
      size: 1
      prob: prob
  - family: zmgeom
    at:
      prob: prob
      p0: 0
  transform_of:
  - family: geom
    at:
      prob: prob
    transform: shift
    shift: 1
  truncation_of:
  - family: dlaplace
    at:
      location: 1
      scale: 1-prob
    when: prob < 1
fit:
  method: closed-form
  certified: 'yes'
  note: "ztgeom/mle.hpp: The estimate is the reciprocal sample mean, including the attained probability-one member for all-one data; held probability only evaluates compatible likelihood."
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model geometric waiting counts sampled only when positive.
summary: Zero-truncated geometric provides a model for geometric waiting counts sampled only when positive.
aliases:
- "geometric on positive integers"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    For every nonempty sample of positive integers, the free-probability MLE exists and is unique, with $\hat p=1/\bar x$. All-one data attain $p=1$. Holding the probability leaves only likelihood evaluation. A held value $p=1$ requires all observations to equal one.
  reason: >-
    For $m$ observations with total $T$, the log likelihood is $m\log p+(T-m)\log(1-p)$. If $T>m$, its derivative vanishes only at $m/T$ and its second derivative is strictly negative. If $T=m$, it increases to its attained maximum at one.
---

The zero-truncated geometric distribution models positive waiting counts. Its probability $p$ is the success probability, and subtracting one from a count gives an ordinary geometric count of failures.

## Details

The estimate is the reciprocal of the sample mean. All-one samples give the attained point mass at one. Holding the probability evaluates the supplied distribution without estimation. An interior probability permits every positive integer, whereas probability one permits only ones.

## R notes

Supplying `prob` holds the probability. With `prob = NULL`, it is estimated. Invalid or incompatible held values stop. The point-mass convention at `prob = 1` agrees with actuar.
