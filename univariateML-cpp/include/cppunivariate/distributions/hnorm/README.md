---
id: hnorm
name: Translated Half-Normal
support: {type: real, lower: "mu", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "log(2/pi)/2 - log(sigma)"}}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  bound: lower
  kind: threshold
  advice: mu = 0 is the ordinary half-normal; NULL estimates the translated endpoint.
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dhnorm
  sampler: rhnorm
  control:
    sigma: Held positive finite coefficient; NULL estimates it.
    mu: Required endpoint; a value holds it and explicit `NULL` estimates the translation.
log_density: log(2/pi)/2 - log(sigma) - (x-mu)**2/(2*sigma**2)
limits:
  - {name: scale collapse, when: every observation equals the free or held endpoint and sigma tends to zero, status: DegenerateData}
references:
  - Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994). Continuous Univariate Distributions, Volume 1, second edition. Wiley.
fit_args:
  mu: ~
examples: |
  set.seed(1)
  x <- rml(200, mlhnorm())
  mlhnorm(x, mu = NULL)
---

The translated half-normal distribution models nonnegative normal deviations from an unknown endpoint.

## Details

The support is $[\mu, \infty)$ and `sigma` is a scale, not the standard deviation.
The likelihood is maximised at `mu = min(x)` exactly, with
`sigma = sqrt(mean((x - mu)^2))`. At the endpoint `dml` returns the
interior limit `sqrt(2/pi)/sigma`. At least two distinct observations are
required when both coefficients are estimated; constant data have unbounded
likelihood as sigma tends to zero.

Holding `mu` uses the closed support $[\mu, \infty)$ and requires all observations to be at least `mu`, with at least one strictly larger. The scale estimate is the root mean square distance from that endpoint, including for a single observation. `mlhnorm(x, mu = 0)` fits the ordinary half-normal. Evaluation uses the internal translated half-normal functions.

Data with a sharp maximum instead of a minimum come from the reflected model; fit `mlhnorm(-x, mu = NULL)`.

Holding positive finite `sigma` removes scale collapse, including for singleton
and constant samples. The free endpoint is sample minimum exactly; a held endpoint
may equal the observations. Both held coefficients are returned unchanged.

### Adapter independence (2026-10-30)

The adapter now returns the core held-scale log likelihood directly. The removed duplicate used the identical sum order and formula; support and finite-likelihood checks already live in `hnorm_mle`.
