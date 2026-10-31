---
id: power
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
  fixed: [alpha]
  when: Held alpha is below the sample maximum.
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
aliases: []
key_references:
- 2
---

The power distribution models positive quantities below a finite upper endpoint.

## Details

The fitted support is $(0, \alpha]$. The upper endpoint is exactly
`max(x)` when estimated; its density is `beta/alpha`, the interior limit.
`epsilon` is retained for compatibility and ignored after validation.

A held alpha must be at least max(x). A free beta is the reciprocal of
`mean(log(alpha/x))`. If all observations equal alpha, the likelihood is
unbounded as beta tends to infinity; the fitter stops. With beta held,
constant and singleton samples have an attained fit at alpha = max(x).
