---
id: hcauchy
inference_numeric:
  - regime: "known threshold scale"
    available: true
    condition: "finite mu held and sigma > 0 free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-threshold-and-fixed-shape-submodels"
  - regime: unreviewed or unavailable
    available: false
    condition: "threshold free"
    reason: "a free threshold changes support with positive endpoint density and is non-DQM"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support"
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
    firmness: proved
    when: '$\sigma \to 0$ with more than half the observations at the free or held endpoint'
    status: Unbounded
  - name: finite unattained scale-zero supremum
    firmness: proved
    when: '$\sigma \to 0$ with exactly half the observations at the free or held endpoint'
    status: ScaleBoundary
references:
- extraDistr package, HalfCauchy documentation (density and parameterization).
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
fit_args:
  mu: 0
examples: |
  set.seed(1)
  x <- rml(200, mlhcauchy())
  mlhcauchy(x, mu = 0L)

group: skewed and robust location-scale
construction:
- folding
relations:
  transform_of:
  - family: cauchy
    at:
      location: 0
      scale: sigma
    transform: abs
    shift: mu
  special_case_of:
  - family: ht
    at:
      mu: mu
      sigma: sigma
      nu: 1
fit:
  method: profile-search
  certified: 'yes'
  note: 'hcauchy/README.md, Details; hcauchy/mle.hpp (public fitter and held overloads); hcauchy/test.cpp: Endpoint
    is the sample minimum; the strictly decreasing log-scale score has a unique root exactly below the documented
    tie threshold; held scale is direct.'
areas:
- reliability and survival
classification: specialist
classification_reason: Used to model unsigned Cauchy errors with a supplied or estimated lower endpoint.
summary: Half-Cauchy provides a model for unsigned Cauchy errors with a supplied or estimated lower endpoint.
aliases: []
key_references:
- 2
---

The half-Cauchy models nonnegative magnitudes with heavy tails and an endpoint, translated from zero.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-halfthreshold.md) derives these boundary conditions, including the relevant held parameters.

Let $k$ count the observations at the free or held endpoint ($k$ ties at `min(x)` when `mu` is estimated; at the held `mu` otherwise). The likelihood increases in `mu` up to the sample minimum, so a free endpoint is exactly `mu = min(x)`, including tied minima; a held `mu` above any observation is invalid. Given that endpoint, a positive-scale MLE exists uniquely iff $2k<n$: the log-scale score is $n-\sum_i 2\sigma^2/(\sigma^2+(x_i-\mu)^2)$, strictly decreasing from $n-2k$ to $-n$ whenever some observation exceeds the endpoint. For $2k=n$ it is strictly negative at every positive scale: the finite scale-zero supremum is unattained. For $2k>n$, including constant data, the likelihood is unbounded as scale tends to zero. Even one observation strictly above the endpoint identifies a unique scale estimate.

Evaluation uses the internal d/p/q/r functions. The endpoint density is positive.

A positive finite held `sigma` is evaluated directly at any `mu` (free or held), including for data entirely at the endpoint; the scale-collapse conditions apply only when scale is estimated.
