---
id: pareto
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
---

The Pareto distribution is a heavy-tailed model for positive quantities above a lower threshold.

## Details

The lower endpoint `b` is estimated by `min(x)`, a boundary MLE at an order statistic; `a` is the tail shape. The fitted support is $[b, \infty)$, with $b>0$.

A held b must not exceed min(x). With a free the likelihood is unbounded if all observations equal b. Holding a permits constant data and estimates b=min(x).

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
