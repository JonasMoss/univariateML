---
id: lnorm
name: Lognormal
support: {type: real, lower: 0.0, upper: inf, closed: [false, false]}
params:
  - {name: meanlog, role: location, domain: "(-inf, inf)", default: 0.0}
  - {name: sdlog, role: scale, domain: "(0, inf)", default: 1.0}
r:
  density: stats::dlnorm
  sampler: stats::rlnorm
  control: {}
log_density: -log(x) - log(sdlog) - log(2*pi)/2 - (log(x)-meanlog)**2/(2*sdlog**2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 14. Wiley, New York."
examples: |
  mllnorm(precip)
---

The lognormal distribution models positive quantities whose logarithms follow a normal distribution.

## Details

The parameters describe the normal distribution of the logarithms: `meanlog` is their sample mean and `sdlog` uses divisor $n$.
