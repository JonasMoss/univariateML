---
id: hnorm
name: Translated Half-Normal
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mu, role: location, domain: "(-inf, inf)", default: 0}
  - {name: sigma, role: scale, domain: "(0, inf)", default: 1}
r:
  density: dhalfnorm
  sampler: rhalfnorm
  control: {}
log_density: log(2/pi)/2 - log(sigma) - (x-mu)**2/(2*sigma**2)
references:
  - Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994). Continuous Univariate Distributions, Volume 1, second edition. Wiley.
examples: |
  set.seed(7)
  mlhnorm(rhalfnorm(100, mu = -2, sigma = 3))
---

The translated half-normal distribution models nonnegative normal deviations from an unknown endpoint.

## Details

The support is $[\mu, \infty)$. The endpoint density is the continuous extension from the right. The maximum likelihood estimates are `mu = min(x)` and `sigma = sqrt(mean((x - mu)^2))`; the endpoint estimate is a boundary MLE at an order statistic. At least two observations with distinct values are required; constant data have an unbounded likelihood as the scale tends to zero. The parameter `sigma` is a scale, not the standard deviation.
