---
id: pareto
name: Pareto
support: {type: real, lower: "b", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "log(a) - log(b)"}}
params:
- name: a
  role: shape
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: b
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
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 20. Wiley, New York."
limits:
- name: zero likelihood
  fixed: [b]
  when: Held b exceeds the sample minimum.
  status: InvalidInput
  message: zero likelihood
- name: unbounded likelihood
  fixed: [b]
  when: Every observation equals held b.
  status: Unbounded
  message: constant data
examples: |
  set.seed(1)
  x <- rml(200, mlpareto())
  mlpareto(x)
---

The Pareto distribution is a heavy-tailed model for positive quantities above a lower threshold.

## Details

The lower endpoint `b` is estimated by `min(x)`, a boundary MLE at an order statistic; `a` is the tail shape. The fitted support is $[b, \infty)$, with $b>0$.

A held b must not exceed min(x). With a free the likelihood is unbounded if all observations equal b. Holding a permits constant data and estimates b=min(x).
