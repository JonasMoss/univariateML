---
id: geom
name: Geometric
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: prob
  symbol: 'p'
  role: probability
  domain: (0, 1]
  simulate: 0.5
  kind: regular
r:
  density: stats::dgeom
  sampler: stats::rgeom
  control:
    prob: "Optional held prob; NULL estimates it."
log_density: "log(prob) + x*log(1 - prob)"
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
examples: |
  set.seed(1)
  x <- rml(200, mlgeom())
  mlgeom(x)

group: Poisson, binomial and negative binomial
construction:
- base
relations:
  special_case_of:
  - family: nbinom
    at:
      size: 1
      prob: prob
  - family: cmp
    at:
      lambda: 1-prob
      nu: 0
  - family: zmgeom
    at:
      prob: prob
      p0: prob
  - family: dgamma
    at:
      shape: 1
      rate: -log(1-prob)
    when: prob < 1
  - family: dweibull
    at:
      shape1: 1-prob
      shape2: 1
    when: prob < 1
  truncation_of:
  - family: dlaplace
    at:
      location: 0
      scale: 1-prob
    when: prob < 1
fit:
  method: closed-form
  certified: 'yes'
  note: 'geom/README.md, Details; geom/mle.hpp (public fitter and held overloads); geom/test.cpp: Closed-form maximum
    for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- counts and ecology
classification: classical
classification_reason: A standard model for failures before the first success in repeated independent trials.
summary: Geometric provides a model for failures before the first success in repeated independent trials.
aliases:
- "Furry"
key_references:
- 1
---

The geometric distribution models the number of failures before the first success in independent Bernoulli trials.

## Details

The count starts at zero, following the R convention. All-zero data give the boundary estimate `prob = 1`.

Supplying `prob` evaluates the likelihood at that value. Held values outside the parameter domain or incompatible with the observed counts stop.
