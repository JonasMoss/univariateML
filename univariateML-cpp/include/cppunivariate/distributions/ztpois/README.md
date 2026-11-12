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
  note: "ztpois/mle.hpp: The positive conditional mean is strictly increasing in rate, giving a unique root in (0, sample mean), computed by 100 bisections; all-one data attain rate zero. Held rate evaluates compatible likelihood."
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
existence:
  uniqueness: proved
  statement: >-
    For every nonempty sample of positive integers, the free-rate MLE exists and is unique. It is $\hat\lambda=0$ when all observations equal one. Otherwise it is the unique positive solution of $\lambda/(1-e^{-\lambda})=\bar x$. Holding the rate leaves only likelihood evaluation. A held zero rate requires every observation to equal one.
  reason: >-
    In the natural parameter $\eta=\log\lambda$, the second derivative of the log likelihood is minus the sample size times the conditional variance, which is strictly negative. The conditional mean increases continuously from one to infinity as the rate increases from zero to infinity. Thus every sample mean above one gives exactly one score root. At mean one the attained zero-rate member maximizes the likelihood.
---

The zero-truncated Poisson distribution describes Poisson counts observed only when positive. The rate $\lambda$ belongs to the underlying Poisson law before conditioning.

## d/p/q/r

### Concentrated probability evaluation

At rate $10^8$, the omitted zero mass is $\exp(-10^8)$, while the body is
near count $10^8$. Enumerating a positive prefix to the body is unnecessary:
the base-CDF difference is well conditioned once its mass exceeds twice the
zero mass. The mode density also needs the centered Poisson log mass rather
than subtraction of terms of order $\lambda\log\lambda$.

## Details

The fit matches the conditional mean to the sample mean by bisection between zero and the sample mean. Strict monotonicity gives a unique solution when any count exceeds one. The zero-rate member is the point mass at one and is attained when all counts are one.

Holding the rate evaluates the supplied distribution without estimation. A positive rate gives positive probability to every positive integer. A zero rate is compatible only with ones.

## R notes

Supplying `lambda` holds the rate. With `lambda = NULL`, the rate is estimated. Invalid or incompatible held values stop. The boundary convention at `lambda = 0` agrees with actuar.
