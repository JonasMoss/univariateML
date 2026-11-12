---
id: invgauss
inference_numeric:
  - regime: interior
    available: true
    condition: "finite mean > 0 and shape > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Inverse Gaussian
support: {type: real, lower: 0.0, upper: inf, closed: [false, false]}
params:
- name: mean
  symbol: '\mu'
  role: location
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape
  symbol: '\lambda'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dinvgauss
  sampler: rinvgauss
  control:
    mean: Optional held mean; NULL estimates it.
    shape: Optional held shape; NULL estimates it.
log_density: log(shape)/2 - log(2*pi)/2 - 3*log(x)/2 - shape*(x-mean)**2/(2*mean**2*x)
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 15.
  Wiley, New York.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
limits:
- kind: nonexistence
  firmness: proved
  name: unbounded likelihood
  when: '$\mu$ equals the common observation and $\lambda$ tends to infinity'
  detect: 'both coefficients are free and all observations are equal'
  status: Unbounded
  message: constant data
- name: unbounded likelihood
  firmness: proved
  fixed: [mean]
  when: 'the free shape $\lambda$ tends to infinity with mean $\mu$ held'
  detect: 'every observation equals the held $\mu$ and $\lambda$ is free'
  status: Unbounded
  message: constant data
examples: |
  x <- datasets::airquality$Wind[!is.na(datasets::airquality$Wind)]
  mlinvgauss(x)

group: other positive
construction:
- base
relations:
  closure_point:
  - name: Lévy
    path: '$\mu\to\infty$ with $\lambda$ fixed'
    reachability: never
    reason: 'For every finite positive sample and fixed positive shape, the likelihood in the mean is uniquely maximized at the sample mean.'
fit:
  method: closed-form
  certified: 'yes'
  note: 'invgauss/README.md, Details; invgauss/mle.hpp (public fitter and held overloads); invgauss/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- reliability and survival
classification: classical
classification_reason: A standard model for first-passage times of Brownian motion with positive drift.
summary: Inverse Gaussian provides a model for first-passage times of Brownian motion with positive drift.
aliases:
- "Wald"
key_references:
- 2
existence:
  uniqueness: proved
  statement: >-
    For a nonempty positive sample with both coefficients free, the MLE
    exists and is unique exactly when the observations are not all equal.
    With $\mu$ held and $\lambda$ free, it exists and is unique unless
    every observation equals $\mu$. With $\lambda$ held, the free mean
    has a unique MLE even for constant data. Holding both coefficients
    leaves only likelihood evaluation.
  reason: >-
    Apart from a constant, the log-likelihood is
    $(n/2)\log\lambda-(\lambda/2)Q(\mu)$, where
    $Q(\mu)=\sum_i(x_i-\mu)^2/(\mu^2 x_i)$.
    Its derivative is $Q'(\mu)=2(n\mu-\sum_i x_i)/\mu^3$, so its unique
    minimum over positive $\mu$ is at $\bar x$. For a fixed or fitted mean,
    $Q>0$ gives the unique maximum $\lambda=n/Q$. If $Q=0$, the
    log-likelihood grows without bound as $\lambda\to\infty$.
---

The inverse Gaussian (Wald) distribution models positive quantities, including first-passage times of Brownian motion with positive drift.

## Details

The [positive-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-positive.md) gives the boundary arguments and their held-parameter conditions.

The coefficient $\mu$ is the positive mean and $\lambda$ is a positive shape coefficient. Increasing $\lambda$ at fixed $\mu$ concentrates the distribution about its mean. The shape is not a dispersion coefficient.

The free mean estimate is the arithmetic sample mean, whether the shape is free or held. With the mean supplied or fitted, the shape estimate is the reciprocal of the average squared relative residual weighted by $1/x_i$. It is finite unless all observations equal that mean. In that case the likelihood is unbounded as $\lambda$ increases. Holding a positive shape permits constant data.

Although the distribution tends to a Lévy law as $\mu\to\infty$ with $\lambda$ fixed, this is not a likelihood escape for a finite positive sample: the likelihood in the mean is uniquely maximized at $\bar x$.

For highly concentrated samples, rounding the mean by $\delta$ loses
$n\lambda\delta^2/(2\mu^3)$ in log-likelihood to leading order. Consequently,
even a one-ulp mean error can exceed the likelihood accuracy target when
$\lambda/\mu$ is large. The smooth maximum requires a sufficiently accurate
public mean as well as an accurate residual calculation.

## R notes

`mean = NULL` and `shape = NULL` estimate the corresponding coefficients. Held coefficients must be finite and positive. Both held coefficients evaluate the likelihood without estimation. Empty or invalid samples and unattained or numerically unrepresentable fits stop with an error.

## Decision history

2026-10-31 (TASK-132): invalid and empty samples carry failure statuses, including delegate paths. See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md) and the appended cases in `test.cpp` for the validation scope.
