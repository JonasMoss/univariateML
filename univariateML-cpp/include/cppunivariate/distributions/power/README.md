---
id: power
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite alpha > 0 held and beta > 0 free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support"
name: Power
support: {type: real, lower: "0", upper: "alpha", closed: [false, true], endpoint_log_density: {upper: "log(beta) - log(alpha)"}}
params:
- name: alpha
  symbol: 'a'
  role: upper
  domain: (0, inf)
  simulate: 1.0
  bound: upper
  kind: endpoint
- name: beta
  symbol: 'b'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dpower
  sampler: rpower
  control:
    alpha: Optional held alpha; NULL estimates it.
    beta: Optional held beta; NULL estimates it.
    epsilon: Deprecated compatibility argument, validated but ignored; alpha is exactly max(x).
log_density: log(beta) - beta*log(alpha) + (beta-1)*log(x)
references:
- 'Arslan, G. "A new characterization of the power distribution." Journal of Computational and Applied Mathematics
  260 (2014): 99-102.'
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds: []
limits:
- name: zero likelihood
  firmness: proved
  fixed: [alpha]
  when: The held upper endpoint $a$ is below an observation, giving zero likelihood.
  detect: The sample maximum exceeds the held upper endpoint $a$.
  status: InvalidInput
  message: zero likelihood
examples: |
  set.seed(1)
  x <- rml(200, mlpower())
  mlpower(x)

group: bounded continuous
construction:
- base
relations:
  special_case_of:
  - family: genbeta
    at:
      shape1: beta
      shape2: 1
      shape3: 1
      scale: alpha
  - family: nsbeta
    at:
      shape1: beta
      shape2: 1
      min: 0
      max: alpha
  transform_of:
  - family: unif
    at:
      min: 0
      max: alpha**beta
    transform: power
    power: beta
  - family: pareto
    at:
      a: beta
      b: 1/alpha
    transform: inverse
fit:
  method: closed-form
  certified: 'yes'
  note: 'power/README.md, Details; power/mle.hpp (public fitter and held overloads); power/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- proportions
classification: specialist
classification_reason: Used to model bounded positive sizes with a simple power-shaped distribution function.
summary: Power provides a model for bounded positive sizes with a simple power-shaped distribution function.
aliases:
- "power-function"
key_references:
- 2
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a positive finite sample has a unique MLE exactly when it is nonconstant. With $a$ held at or above the sample maximum and $b$ free, the MLE exists and is unique unless every observation equals $a$. With $b$ held, the unique free endpoint estimate is the sample maximum, even for constant data. With both held, only support compatibility is evaluated.
  reason: >-
    For every $b>0$, likelihood decreases strictly with $a$, so a free endpoint equals the sample maximum. At fixed $a$, the shape log-likelihood is $n\log b-bS$ plus a constant, where $S=\sum_i\log(a/x_i)$. It is strictly concave and has its unique maximum at $b=n/S$ when $S>0$. When $S=0$, it increases without bound.
---

The power distribution models positive measurements below an upper endpoint $a$. The shape $b$ determines whether density decreases toward the endpoint ($b<1$), is uniform ($b=1$), or increases toward it ($b>1$).

## Details

The support is $(0,a]$, including its upper endpoint with density $b/a$. Each coefficient may be held. A free endpoint is exactly the sample maximum. With the endpoint known, the shape estimate is the reciprocal of the sample mean of $\log(a/x)$. If every observation equals the endpoint and shape is free, likelihood is unbounded as $b\to\infty$. Holding shape prevents this failure, including for singleton samples.

The [positive-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-positive.md) gives the boundary arguments and held cases. A held endpoint must be at least the sample maximum.

## R notes

The compatibility argument `epsilon` is validated and then ignored. It does not move the endpoint away from `max(x)`.
