---
id: lnorm
name: Lognormal
support: {type: real, lower: 0.0, upper: inf, closed: [false, false]}
params:
- name: meanlog
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sdlog
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
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 14. Wiley, New York."
limits:
- name: unbounded likelihood
  fixed: [meanlog]
  when: Every log observation equals held meanlog.
  status: Unbounded
  message: unbounded
examples: |
  x <- datasets::precip
  mllnorm(x)
---

The lognormal distribution models positive quantities whose logarithms follow a normal distribution.

## Details

2026-10-30 (TASK-125): when squared log residuals overflow, divide them by their maximum magnitude before summation, then restore the scale. The held-mean 1e300 regression exercises this path.

The parameters describe the normal distribution of the logarithms: `meanlog` is their sample mean and `sdlog` uses divisor $n$.

With meanlog held and sdlog free, the likelihood is unbounded if every log observation equals meanlog. A positive held sdlog permits constant data.
