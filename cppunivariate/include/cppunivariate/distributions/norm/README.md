---
id: norm
name: Normal
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mean, role: location, domain: "(-inf, inf)", default: 0.0}
  - {name: sd, role: scale, domain: "(0, inf)", default: 1.0}
r:
  density: stats::dnorm
  sampler: stats::rnorm
  control: {}
log_density: -log(sd) - log(2*pi)/2 - (x-mean)**2/(2*sd**2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 13. Wiley, New York."
examples: |
  mlnorm(precip)
---

The normal (Gaussian) distribution is a symmetric distribution on the real line, used to model continuous measurements and errors.

## Details

The estimated `sd` uses the variance with divisor $n$, rather than the unbiased divisor $n-1$.
