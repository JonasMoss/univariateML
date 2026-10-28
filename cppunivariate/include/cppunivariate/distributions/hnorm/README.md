---
id: hnorm
name: Translated Half-Normal
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mu, role: location, domain: "(-inf, inf)", default: 0, bound: lower}
  - {name: sigma, role: scale, domain: "(0, inf)", default: 1}
r:
  density: extraDistr::dtnorm
  sampler: extraDistr::rtnorm
  args: {mean: mu, sd: sigma, a: mu}
  control: {}
log_density: log(2/pi)/2 - log(sigma) - (x-mu)**2/(2*sigma**2)
references:
  - Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994). Continuous Univariate Distributions, Volume 1, second edition. Wiley.
examples: |
  set.seed(7)
  mlhnorm(extraDistr::rtnorm(100, mean = -2, sd = 3, a = -2))
---

The translated half-normal distribution models nonnegative normal deviations from an unknown endpoint.

## Details

The support is $(\mu, \infty)$ and `sigma` is a scale, not the standard deviation. The likelihood is maximised at the boundary `mu = min(x)` with `sigma = sqrt(mean((x - mu)^2))`. `extraDistr::dtnorm` excludes its lower endpoint, so the fitter reports `mu` as the largest double below `min(x)`; then `dml` is positive at every observation and `sum(dml(x, fit, log = TRUE))` equals `logLik`. At least two distinct observations are required: for constant data the likelihood is unbounded as `sigma` tends to zero.

Data with a sharp maximum instead of a minimum come from the reflected model; fit `mlhnorm(-x)`.
