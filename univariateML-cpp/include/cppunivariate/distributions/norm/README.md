---
id: norm
name: Normal
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mean
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sd
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: stats::dnorm
  sampler: stats::rnorm
  control:
    sd: Optional held sd; NULL estimates it.
    mean: Optional held mean; NULL estimates it.
log_density: -log(sd) - log(2*pi)/2 - (x-mean)**2/(2*sd**2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 13. Wiley, New York."
holds: []
limits:
  - name: scale collapse
    when: Scale tends to zero at the fitted or held location
    detect: sd is free and every observation equals the fitted or held location
    status: Unbounded
    message: "unbounded"
examples: |
  x <- datasets::trees$Height
  mlnorm(x)

summary: The normal distribution models symmetric continuous variation through a location and a standard deviation.
classification: classical
classification_reason: A standard model for additive measurement errors and approximately Gaussian fluctuations.
aliases:
- "Gaussian"
- "Gauss"
- "Laplace–Gauss"
- "law of error"
- "law of facility of errors"
- "Laplace's second law"
key_references:
- 1

group: normal and Student
construction:
- location-scale
relations:
  special_case_of:
  - family: sn
    at:
      xi: mean
      omega: sd
      alpha: 0
  - family: snorm
    at:
      mean: mean
      sd: sd
      xi: 1
  - family: ged
    at:
      mean: mean
      sd: sd
      nu: 2
  - family: bhatt
    at:
      mu: mean
      sigma: sd
      a: 0
  - family: tnorm
    at:
      mean: mean
      sd: sd
      a: -Inf
      b: Inf
fit:
  method: closed-form
  certified: 'yes'
  note: 'norm/README.md, Details; norm/mle.hpp (public fitter and held overloads); norm/test.cpp: Closed-form maximum
    for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- measurement and errors
existence:
  uniqueness: proved
  statement: >-
    With $\mu$ and $\sigma$ free, the MLE exists if and only if the
    observations are not all equal; it is then unique, with $\hat\mu = \bar x$
    and $\hat\sigma^2 = n^{-1}\sum_i (x_i - \bar x)^2$. With $\sigma$ held,
    $\hat\mu = \bar x$ always. With $\mu$ held, the MLE exists unless every
    observation equals $\mu$.
  reason: >-
    The normal family is a regular exponential family with sufficient
    statistic $(x, x^2)$ and natural parameter
    $(\mu/\sigma^2, -1/(2\sigma^2))$. The log-likelihood is strictly concave
    in the natural parameter, so it has at most one maximizer, and one exists
    exactly when $(\bar x, \overline{x^2})$ lies inside the convex hull of the
    parabola $\{(x, x^2)\}$, that is when $\overline{x^2} > \bar x^2$; this
    fails only for constant samples.
---

The normal (Gaussian) distribution is a symmetric distribution on the real line, used to model continuous measurements and errors.

## Details

The estimated `sd` uses the variance with divisor $n$, rather than the unbiased divisor $n-1$.


With mean held, scale is the root mean square residual about that value. Scale collapses only when every observation equals the held mean. Holding sd removes collapse, including for constant samples.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
