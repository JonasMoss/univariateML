---
id: hnorm
name: Translated Half-Normal
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mu
  role: location
  domain: (-inf, inf)
  simulate: 0
  bound: lower
  kind: threshold
  advice: mu = 0 is the ordinary half-normal; NULL estimates the translated endpoint.
- name: sigma
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: extraDistr::dtnorm
  sampler: extraDistr::rtnorm
  args: {mean: mu, sd: sigma, a: mu}
  control:
    sigma: Held positive finite coefficient; NULL estimates it.
    mu: Required endpoint; a value holds it and explicit `NULL` estimates the translation.
log_density: log(2/pi)/2 - log(sigma) - (x-mu)**2/(2*sigma**2)
limits:
  - {name: scale collapse, when: every observation equals the free or held endpoint and sigma tends to zero, status: DegenerateData}
references:
  - Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994). Continuous Univariate Distributions, Volume 1, second edition. Wiley.
examples: |
  set.seed(7)
  mlhnorm(extraDistr::rtnorm(100, mean = -2, sd = 3, a = -2), mu = NULL)
fit_args:
  mu: ~
---

The translated half-normal distribution models nonnegative normal deviations from an unknown endpoint.

## Details

`extraDistr::dtnorm`, `ptnorm` and `qtnorm` are repaired at runtime; each repair switches itself off once the reference is fixed. Fitting and `logLik` do not use them.

The support is $(\mu, \infty)$ and `sigma` is a scale, not the standard deviation. The likelihood is maximised at the boundary `mu = min(x)` with `sigma = sqrt(mean((x - mu)^2))`. `extraDistr::dtnorm` excludes its lower endpoint, so the fitter reports `mu` as the largest double below `min(x)`; then `dml` is positive at every observation and `sum(dml(x, fit, log = TRUE))` equals `logLik`. At least two distinct observations are required: for constant data the likelihood is unbounded as `sigma` tends to zero.

Holding `mu` uses the closed support $[\mu, \infty)$ and requires all observations to be at least `mu`, with at least one strictly larger. The scale estimate is the root mean square distance from that endpoint, including for a single observation. `mlhnorm(x, mu = 0)` fits the ordinary extraDistr half-normal. Its d/p/q/r use shifted extraDistr half-normal functions, including their runtime tail repairs.

Data with a sharp maximum instead of a minimum come from the reflected model; fit `mlhnorm(-x, mu = NULL)`.

Holding positive finite `sigma` removes scale collapse, including for singleton
and constant samples. The free endpoint is still the sample minimum (reported
as its predecessor until the common closed-endpoint migration); a held endpoint
may equal the observations. Both held coefficients are returned unchanged.
