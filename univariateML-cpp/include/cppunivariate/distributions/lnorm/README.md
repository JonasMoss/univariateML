---
id: lnorm
name: Lognormal
support: {type: real, lower: 0.0, upper: inf, closed: [false, false]}
params:
- name: meanlog
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sdlog
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: stats::dlnorm
  sampler: stats::rlnorm
  control:
    meanlog: Optional held meanlog; NULL estimates it.
    sdlog: Optional held sdlog; NULL estimates it.
log_density: -log(x) - log(sdlog) - log(2*pi)/2 - (log(x)-meanlog)**2/(2*sdlog**2)
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 14.
  Wiley, New York.
limits:
- name: unbounded likelihood
  fixed: [meanlog]
  when: Every log observation equals held meanlog.
  status: Unbounded
  message: unbounded
examples: |
  x <- datasets::precip
  mllnorm(x)

group: normal and Student
construction:
- transformation
relations:
  transform_of:
  - family: norm
    transform: log
    at:
      mean: meanlog
      sd: sdlog
fit:
  method: delegation
  certified: 'yes'
  note: 'lnorm/README.md, Details; lnorm/mle.hpp (public fitter and held overloads); lnorm/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- finance
- measurement and errors
classification: classical
classification_reason: A standard model for positive multiplicative variation represented by Gaussian logarithms.
summary: Lognormal provides a model for positive multiplicative variation represented by Gaussian logarithms.
aliases:
- "log-normal"
- "Galton"
key_references:
- 1
---

The lognormal distribution models positive quantities whose logarithms follow a normal distribution.

## Details

2026-10-30 (TASK-125): when squared log residuals overflow, divide them by their maximum magnitude before summation, then restore the scale. The held-mean 1e300 regression exercises this path.

The parameters describe the normal distribution of the logarithms: `meanlog` is their sample mean and `sdlog` uses divisor $n$.

With meanlog held and sdlog free, the likelihood is unbounded if every log observation equals meanlog. A positive held sdlog permits constant data.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
