---
id: hcauchy
name: Half-Cauchy
support: {type: real, lower: "mu", upper: inf, closed: [true, false], endpoint_log_density: {lower: "log(2/(pi*sigma))"}}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: threshold
  advice: mu = 0 is the ordinary half-Cauchy; NULL estimates the translated endpoint.
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dhcauchy
  sampler: rhcauchy
  control:
    mu: Required endpoint; a value holds it and explicit `NULL` estimates the translation.
    sigma: Held positive finite scale; NULL estimates it.
log_density: log(2/pi) - log(sigma) - log(1+(x-mu)**2/sigma**2)
limits:
  - name: unbounded scale collapse
    when: '$\sigma \to 0$ with more than half the observations at the free or held endpoint'
    status: Unbounded
  - name: finite unattained scale-zero supremum
    when: '$\sigma \to 0$ with exactly half the observations at the free or held endpoint'
    status: ScaleBoundary
references:
  - extraDistr package, HalfCauchy documentation (density and parameterization).
fit_args:
  mu: 0
examples: |
  set.seed(1)
  x <- rml(200, mlhcauchy())
  mlhcauchy(x, mu = 0L)
---

The half-Cauchy models nonnegative magnitudes with heavy tails and an endpoint, translated from zero.

## Details

Let $k$ count the observations at the free or held endpoint ($k$ ties at `min(x)` when `mu` is estimated; at the held `mu` otherwise). The likelihood increases in `mu` up to the sample minimum, so a free endpoint is exactly `mu = min(x)`, including tied minima; a held `mu` above any observation is invalid. Given that endpoint, a positive-scale MLE exists uniquely iff $2k<n$: the log-scale score is $n-\sum_i 2\sigma^2/(\sigma^2+(x_i-\mu)^2)$, strictly decreasing from $n-2k$ to $-n$ whenever some observation exceeds the endpoint. For $2k=n$ it is strictly negative at every positive scale: the finite scale-zero supremum is unattained. For $2k>n$, including constant data, the likelihood is unbounded as scale tends to zero. Even one observation strictly above the endpoint identifies a unique scale estimate.

Evaluation uses the internal d/p/q/r functions. The endpoint density is positive.

A positive finite held `sigma` is evaluated directly at any `mu` (free or held), including for data entirely at the endpoint; the scale-collapse conditions apply only when scale is estimated.
