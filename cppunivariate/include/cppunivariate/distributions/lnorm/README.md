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
  control: {}
log_density: -log(x) - log(sdlog) - log(2*pi)/2 - (log(x)-meanlog)**2/(2*sdlog**2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 14. Wiley, New York."
examples: |
  mllnorm(precip)
holds:
- held:
  - meanlog
  status: not-implemented
  reason: The adapter does not accept `meanlog`; this hold is not implemented (P2).
- held:
  - sdlog
  status: not-implemented
  reason: The adapter does not accept `sdlog`; this hold is not implemented (P2).
---

The lognormal distribution models positive quantities whose logarithms follow a normal distribution.

## Details

The parameters describe the normal distribution of the logarithms: `meanlog` is their sample mean and `sdlog` uses divisor $n$.
