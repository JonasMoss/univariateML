---
id: invgamma
name: InvGamma
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
  - {name: alpha, role: shape, domain: "(0, inf)", default: 3.0}
  - {name: beta, role: scale, domain: "(0, inf)", default: 4.0}
r:
  density: extraDistr::dinvgamma
  sampler: extraDistr::rinvgamma
  control:
    abstol: Absolute step tolerance in alpha.
    iterlim: Maximum number of iterations.
log_density: alpha*log(beta) - loggamma(alpha) - (alpha + 1)*log(x) - beta/x
references:
  - "Choi, S. C, and R. Wette. \"Maximum likelihood estimation of the parameters of the gamma distribution and their bias.\" Technometrics 11.4 (1969): 683-690."
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 17. Wiley, New York."
  - "Witkovsky, V. (2001). \"Computing the Distribution of a Linear Combination of Inverted Gamma Variables\". Kybernetika. 37 (1): 79–90"
examples: |
  mlinvgamma(precip)
---

The inverse gamma distribution is the distribution of the reciprocal of a gamma variable.

## Details

The parameter `beta` is the rate of the gamma variable `1/x`, and is the scale parameter in the inverse gamma density.
