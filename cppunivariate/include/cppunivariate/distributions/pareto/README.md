---
id: pareto
name: Pareto
support: {type: real, lower: 0.0, upper: inf, closed: [false, false]}
params:
  - {name: a, role: shape, domain: "(0, inf)", default: 1.0}
  - {name: b, role: lower, domain: "(0, inf)", default: 2.0}
r:
  density: extraDistr::dpareto
  sampler: extraDistr::rpareto
  control: {}
log_density: log(a) + a*log(b) - (a+1)*log(x)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 20. Wiley, New York."
examples: |
  mlpareto(precip)
---

The Pareto distribution is a heavy-tailed model for positive quantities above a lower threshold.

## Details

The lower endpoint `b` is estimated by `min(x)`, a boundary MLE at an order statistic; `a` is the tail shape. The fitted support is $[b, \infty)$, with $b>0$.
