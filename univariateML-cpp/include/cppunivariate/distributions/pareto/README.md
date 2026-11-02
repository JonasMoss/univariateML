---
id: pareto
inference_numeric:
  - regime: "shape with endpoint held"
    available: true
    condition: "finite a > 0 free; finite b > 0 held"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support"
  - regime: "endpoint free"
    available: false
    condition: "finite a > 0; b free"
    reason: "the fitted endpoint is non-DQM and is not a free inference coordinate"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support"
name: Pareto
support: {type: real, lower: "b", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "log(a) - log(b)"}}
params:
- name: a
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: b
  symbol: 'b'
  role: lower
  domain: (0, inf)
  simulate: 1
  bound: lower
  kind: endpoint
r:
  density: dpareto
  sampler: rpareto
  control:
    a: Optional held a; NULL estimates it.
    b: Optional held b; NULL estimates it.
log_density: log(a) + a*log(b) - (a+1)*log(x)
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 20.
  Wiley, New York.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
limits:
- name: zero likelihood
  firmness: proved
  fixed: [b]
  when: No parameter path within the held support can give positive likelihood.
  detect: The held endpoint $b$ exceeds at least one observation.
  status: InvalidInput
  message: zero likelihood
- name: unbounded likelihood
  firmness: proved
  fixed: [b]
  when: The shape $\alpha$ tends to infinity with $b$ at the common observation.
  detect: Shape is free and every observation equals the held endpoint $b$.
  status: Unbounded
  message: constant data
examples: |
  set.seed(1)
  x <- rml(200, mlpareto())
  mlpareto(x)

group: Pareto and Lomax
construction:
- base
relations:
  special_case_of:
  - family: pareto2
    at:
      shape: a
      scale: b
      min: b
  transform_of:
  - family: gpd
    at:
      mu: log(b)
      sigma: 1/a
      xi: 0
    transform: log
fit:
  method: closed-form
  certified: 'yes'
  note: 'pareto/README.md, Details; pareto/mle.hpp (public fitter and held overloads); pareto/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- actuarial
- economics and income
classification: classical
classification_reason: A standard model for sizes above a minimum with a constant log-log survival slope.
summary: Pareto provides a model for sizes above a minimum with a constant log-log survival slope.
aliases:
- "Pareto type I"
key_references:
- 2
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, every nonconstant positive sample has a unique
    finite MLE. Constant data have unbounded likelihood. Holding $\alpha$ gives the
    unique endpoint estimate $b=\min_i x_i$, even for constant data. Holding
    $b\le\min_i x_i$ gives a unique finite shape estimate unless every observation
    equals $b$, when the likelihood is unbounded. A held endpoint above an
    observation gives zero likelihood. Holding both leaves likelihood evaluation.
  reason: >-
    For fixed positive $\alpha$, the log-likelihood increases strictly in $b$ on
    $0<b\le\min_i x_i$. With $b$ fixed, its shape-dependent part is
    $n\log\alpha-\alpha\sum_i\log(x_i/b)$. It is strictly concave and has maximizer
    $n/\sum_i\log(x_i/b)$ when that sum is positive; otherwise it increases
    without bound as $\alpha\to\infty$.
---

The Pareto distribution models positive sizes above a lower endpoint $b$. The shape $\alpha$ controls the right tail, with smaller values giving heavier tails.

## Details

The endpoint estimate is the sample minimum. This is an attained boundary maximum, rather than a stationary point in the interior. Holding the endpoint fixes the lower support boundary and leaves an explicit shape estimate. Holding shape still permits estimation of the endpoint, including for constant data. A held endpoint cannot exceed any observation.

The [positive-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-positive.md) gives the boundary arguments and their held-parameter conditions.

## Decision history

2026-10-31 (diagnostic contract): Empty or invalid input carries a failure status. Degenerate samples depend on which coefficients are free, and delegate failures survive transformations. See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md) and the TASK-132 cases in `test.cpp` for the validation scope.

2026-11-01 (range portability): The log support ratio uses `log1p((x-b)/b)` near the endpoint and `log(x)-log(b)` when the ratio would overflow. The R concentration and `1e-300` to `1e300` tests cover both regimes. This rearranges the closed-form likelihood without changing its maximizer.
