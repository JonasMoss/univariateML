---
id: lgser
name: Logarithmic series
support: {type: integer, lower: 1, upper: inf, closed: [true, false]}
params:
- name: theta
  symbol: '\theta'
  role: rate
  domain: (0, 1)
  simulate: 0.5
  kind: regular
r:
  density: dlgser
  sampler: rlgser
  control:
    theta: "Optional held theta; NULL estimates it."
log_density: "x*log(theta) - log(x) - log(-log(1 - theta))"
references:
- Fisher, R. A., Corbet, A. S., and Williams, C. B. (1943). The relation between the number of species and the number
  of individuals in a random sample of an animal population. The Journal of Animal Ecology, 12(1), 42-58.
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
examples: |
  x <- corbet
  mllgser(x)

group: discrete heavy-tailed
construction:
- base
relations:
  special_case_of:
  - family: zmlogarithmic
    at:
      prob: theta
      p0: 0
  - family: ztnbinom
    at:
      size: 0
      prob: 1-theta
fit:
  method: scalar-solve
  certified: 'yes'
  note: 'lgser/README.md, Details; lgser/mle.hpp (public fitter and held overloads); lgser/test.cpp: Strictly monotone
    one-parameter exponential-family or log-shape score gives a bracketed unique root or the documented boundary;
    finite Zipf endpoint and modified zero mass profile exactly.'
areas:
- rank-frequency
classification: specialist
classification_reason: Used to model species abundance and other highly skewed positive frequencies.
summary: Logarithmic series provides a model for species abundance and other highly skewed positive frequencies.
aliases:
- Logarithmic series
key_references:
- 2
---

The logarithmic series distribution models positive counts and has a history of use in species-abundance data.

## Details

When every observation is one, the optimum lies at $\theta \to 0$, the point-mass boundary. The fitter reports an error instead of returning this boundary fit.

Supplying `theta` evaluates the likelihood at that value. Held values outside the parameter domain or incompatible with the observed counts stop.
