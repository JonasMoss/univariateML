---
id: logitnorm
inference_numeric:
  - regime: interior
    available: true
    condition: "finite mu and sigma > 0; either or both free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
  - regime: boundary or limit
    available: false
    condition: "sigma = 0 or nonfinite parameters"
    reason: "zero scale and infinite coefficients are outside the interior model"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
name: Logit-normal
support: {type: real, lower: 0.0, upper: 1.0, closed: [false, false]}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: regular
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dlogitnorm
  sampler: rlogitnorm
  control:
    mu: Optional held mu; `NULL` estimates it.
    sigma: Optional held sigma; `NULL` estimates it.
log_density: -log(sigma) - log(2*pi)/2 - log(x) - log(1-x) - (log(x/(1-x))-mu)**2/(2*sigma**2)
references:
  - "Aitchison, J., & Shen, S. M. (1980). Logistic-normal distributions: Some properties and uses. Biometrika, 67(2), 261-272. doi:10.1093/biomet/67.2.261."
holds: []
limits:
- name: zero-scale collapse
  firmness: proved
  when: 'The free scale $\sigma$ tends to zero at the fitted or held logit location.'
  detect: 'Scale is free and every logit equals the fitted or held location $\mu$.'
  status: zero scale
  message: does not exist
examples: |
  set.seed(1)
  x <- rml(200, mllogitnorm())
  mllogitnorm(x)

summary: The logit-normal distribution models proportions whose logits are normally distributed.
classification: specialist
classification_reason: Used to model continuous proportions represented by Gaussian log odds.
aliases:
- "logistic-normal"
key_references:
- 1

group: bounded continuous
construction:
- transformation
relations:
  transform_of:
  - family: norm
    transform: logit
    at:
      mean: mu
      sd: sigma
fit:
  method: delegation
  certified: 'yes'
  note: 'loginorm/README.md, Details; loginorm/tnorm_mle.hpp (public fitter and held overloads); logitnorm/test.cpp:
    Closed-form maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- proportions
existence:
  uniqueness: proved
  statement: >-
    For observations strictly between zero and one, the MLE with both
    coefficients free exists exactly when the sample is nonconstant, and is
    unique. With $\mu$ held it exists uniquely unless every logit equals
    $\mu$. With positive $\sigma$ held the location estimate always exists
    and is unique, including for constant samples.
  reason: >-
    The transformation $y=\log(x/(1-x))$ is one-to-one and its Jacobian
    is independent of the coefficients. Maximization therefore reduces to
    normal likelihood maximization on the logits. The unique location is
    their mean and the unique squared scale is their mean squared distance
    from the fitted or held location, unless that distance is zero.
---

The logit-normal distribution models proportions whose log odds follow a normal distribution.

## d/p/q/r

### Extreme d/p/q arithmetic

The normal inverse retains the offset of a log-probability input from exact log(1/2) before multiplication by sigma. Equality with a rounded logarithmic median is insufficient when sigma is large.

### Thin normal images

A rounded logit can equal the supplied normal mean while their exact difference is nonzero. Dividing that lost residual by a tiny standard deviation changes both density and tails. When the ordinary coordinate margin is too large, a two-word atanh-series logarithm retains the residual before division. The only exact rational logit center, x=1/2, is handled directly. Density and tails return NaN when the stated practical two-word margin still cannot resolve the transformed coordinate; this is the remaining named precision region.

## Details

The coefficients $\mu$ and $\sigma$ are the mean and standard deviation of the logits, rather than of the proportions. Every observation must lie strictly between zero and one. Fitting transforms the data to logits and fits a normal distribution, with a coefficient-independent Jacobian adjustment to the likelihood. The scale estimate uses divisor $n$.

With location held, the scale is the root mean squared distance of the logits from that value. With scale held, location is their mean, including for constant data. A free scale has no MLE exactly when every logit equals the fitted or held location. With both coefficients held, the likelihood is evaluated directly.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-bounded.md) gives the boundary argument. Sampling applies the logistic transformation to a normal variate.

## R notes

Held `mu` and `sigma` apply on the logit scale. Both-held fits evaluate the likelihood.

## Decision history

On 2026-10-30 (TASK-125), normal log tails began using the shared normal-tail helper, including its Mills expansion before subnormal loss. Quantile inversion uses the same calculation and checks a monotone bracket until adjacent floating-point endpoints are reached, otherwise returning NaN. Truth rows and extreme round trips validate selected tails.

The 2026-10-31 diagnostic audit checks that empty or invalid input has a non-success status and that delegate failures survive transformations. See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md) and the TASK-132 cases in the family C++ tests.

Centered logits use the difference of two log1p terms, retaining raw proportion spacings and their complements. Two-component absolute logit sums retain a fitted mean close to zero. The reported likelihood uses the returned mean and scale with the logit Jacobian; a finite fit whose rounded logit mean loses the likelihood certificate carries an explicit uncertified flag.
