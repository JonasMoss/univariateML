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
  advice: Set mu = 0 for the ordinary half-Cauchy model. Set mu = NULL to estimate the translated endpoint.
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
    when: 'The scale $\sigma$ tends to zero at the estimated or held endpoint $\mu$.'
    detect: 'Scale is free and more than half the observations equal the endpoint.'
    status: Unbounded
  - name: finite unattained scale-zero supremum
    firmness: proved
    when: 'The scale $\sigma$ tends to zero at the estimated or held endpoint $\mu$.'
    detect: 'Scale is free and exactly half the observations equal the endpoint.'
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
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a unique finite MLE exists exactly when
    the multiplicity $k$ of the sample minimum satisfies $2k<n$.
    With endpoint held at $\mu\le\min_i x_i$, the same criterion applies
    with $k$ counting observations equal to that endpoint.
    Equality gives a finite unattained scale-zero supremum, and $2k>n$
    gives unbounded likelihood. With positive scale held, the free endpoint
    has the unique estimate $\min_i x_i$, even for constant data.
    Holding both coefficients leaves likelihood evaluation on their support.
  reason: >-
    For any positive scale the likelihood increases strictly as the endpoint
    rises to the sample minimum. At that endpoint, or a held endpoint,
    the log-scale score is
    $n-2\sum_i\sigma^2/(\sigma^2+(x_i-\mu)^2)$.
    If some residual is positive, this score decreases strictly from
    $n-2k$ to $-n$, giving exactly one root when $2k<n$.
    The likelihood near zero scale is proportional to $\sigma^{n-2k}$.
    At equality the score is negative for every positive scale, so the
    finite supremum is unattained. All-zero residuals give unbounded collapse.
---

The half-Cauchy distribution models magnitudes with heavy tails above a lower endpoint $\mu$. Its positive scale $\sigma$ sets the width. The endpoint has positive density and is included in the support.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-halfthreshold.md) gives the endpoint and collapse arguments.

For every positive scale, increasing the endpoint towards the sample minimum increases the likelihood. The estimated endpoint is therefore the sample minimum, including tied minima. A held endpoint must not exceed any observation.

Let $k$ count observations at the estimated or held endpoint. A finite positive scale exists uniquely when $2k<n$. At $2k=n$, the scale-zero supremum is finite and unattained. For $2k>n$, including data entirely at the endpoint, the likelihood is unbounded. These conditions apply only when scale is free. A held positive scale allows data entirely at the endpoint and still gives a unique free endpoint estimate.

## R notes

Supply `mu` explicitly. Setting `mu = 0` gives the ordinary half-Cauchy model, while `mu = NULL` estimates the translated endpoint. Supplying `sigma` holds scale. Evaluation uses the internal density, distribution, quantile and random-generation functions.

### Extreme d/p/q arithmetic

Near the median, tan(pi/4+d)-1 = 2 tan(d)/(1-tan(d)) avoids subtracting one from a rounded tangent. The offset is combined with a compensated location-plus-scale.
