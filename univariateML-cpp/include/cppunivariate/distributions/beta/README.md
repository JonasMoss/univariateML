---
id: beta
name: Beta
support: {type: real, lower: 0, upper: 1, closed: [false, false]}
params:
- name: shape1
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape2
  symbol: '\beta'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: stats::dbeta
  sampler: stats::rbeta
  control:
    shape1: Optional held shape1; `NULL` estimates it.
    shape2: Optional held shape2; `NULL` estimates it.
    reltol: Relative step tolerance in `log(shape2)`.
    iterlim: Maximum number of iterations.
log_density: (shape1-1)*log(x) + (shape2-1)*log(1-x) - loggamma(shape1) - loggamma(shape2) + loggamma(shape1+shape2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995). Continuous Univariate Distributions, Volume 2, Chapter 25. Wiley, New York."
holds: []
limits:
- name: point-mass collapse
  when: Both free shapes grow without bound on constant data
  detect: All observations are equal and neither shape is held
  status: Degenerate
  message: does not exist
examples: |
  set.seed(1)
  x <- rml(200, mlbeta())
  mlbeta(x)

summary: The beta distribution models continuous proportions on the unit interval.
classification: classical
classification_reason: A standard model for continuous proportions with two independently adjustable boundary shapes.
aliases:
- beta of the first kind
key_references:
- 1

group: beta and generalised beta
construction:
- base
relations:
  special_case_of:
  - family: genbeta
    at:
      shape1: shape1
      shape2: shape2
      shape3: 1
      scale: 1
  - family: nsbeta
    at:
      shape1: shape1
      shape2: shape2
      min: 0
      max: 1
fit:
  method: scalar-solve
  certified: 'yes'
  note: 'beta/README.md, Details; beta/mle.hpp (public fitter and held overloads); beta/test.cpp: Strictly concave
    beta likelihood in its two shapes; held-shape digamma scores decrease strictly. nsbeta uses supplied endpoints;
    unsupported endpoint fits stop.'
areas:
- proportions
---

The beta distribution models proportions and other continuous quantities between zero and one.

## Details

With either positive finite shape held, the remaining shape has a unique finite maximum even for constant data. Its strictly decreasing digamma score is solved by bracketing. With both shapes free, constant data have unbounded likelihood; with both held the likelihood is evaluated.
