---
id: cauchy
inference_numeric:
  - regime: interior
    available: true
    condition: "finite location and finite scale > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Cauchy
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: location
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: scale
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: stats::dcauchy
  sampler: stats::rcauchy
  control:
    scale: Optional held scale; NULL estimates it.
    location: Optional held location; NULL estimates it.
    abstol: Absolute step tolerance in (location, log(scale)).
    iterlim: Maximum number of iterations.
log_density: -log(pi) - log(scale) - log(1 + ((x-location)/scale)**2)
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 16.
  Wiley, New York.
holds: []
limits:
  - name: scale collapse
    firmness: proved
    when: Scale tends to zero at the fitted or held location.
    detect: Excessive mass at that location or vanishing residual spread.
    status: NoMLE
    message: "does not exist"
examples: |
  set.seed(1)
  x <- rml(200, mlcauchy())
  mlcauchy(x)

group: skewed and robust location-scale
construction:
- location-scale
relations:
  special_case_of:
  - family: lst
    at:
      mu: location
      sigma: scale
      df: 1
fit:
  method: profile-search
  certified: partial
  note: 'cauchy/README.md, Details; cauchy/mle.hpp (public fitter and held overloads); cauchy/test.cpp: Held location
    leaves a unique monotone scale solve; free location can be multimodal and the observation-gap search has no
    root-isolation certificate.'
areas:
- robust modelling
classification: classical
classification_reason: A standard model for symmetric errors whose extreme values require tails without finite variance.
summary: Cauchy provides a model for symmetric errors whose extreme values require tails without finite variance.
aliases:
- "Lorentz"
- "Cauchy–Lorentz"
- "Breit–Wigner"
key_references:
- 1
---

The Cauchy distribution is a symmetric location-scale distribution with heavy tails.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) derives these boundary conditions, including the relevant held parameters.

No maximum likelihood estimator exists when a value has at least half the sample mass, except for two distinct values with equal multiplicities. In that exception the maximum is attained and nonunique; the fitter returns the midpoint and half the distance as location and scale. Other such samples report `NoMLE`.


With location held and scale free, let k count observations equal to that location. A finite scale maximum exists precisely when 2k < n; at equality the collapse supremum is not attained. Holding scale removes collapse. The conditional location likelihood can be multimodal and is searched across the observation gaps.

For held location, the log-scale score is
`g(t) = -k + sum(tanh(log(abs(x-location))-t))`, summing over nonzero
residuals. Its derivative is strictly negative and its limits are `n-2k`
and `-n`, proving uniqueness and global maximality when `2k < n`.
The solver starts at the smallest and largest positive log residual and expands
until strict positive/negative endpoint scores are verified, bounded by the
smallest positive and largest finite double scales. It then bisects to log-width
`1e-12` (or a computed zero score), within `iterlim`. A missing bracket,
iteration exhaustion, nonfinite result or rounded-scale score exceeding
`1e-10*n` reports `IterationFailure`; no unchecked endpoint is returned.
Log residuals and a log-sum-exp likelihood avoid residual-ratio overflow.
This certificate holds in exact arithmetic conditional on the computed
comparisons; the score gate states the practical floating-point margin,
not an outward-rounded interval guarantee. The held-scale search is unchanged.
The prior radius-minus-350 bracket failed for `c(0,1e-300,1)` at location zero;
see the [regression report](../../../../../dev/research/evaluations/2026-11-02-cauchy-bracket-report.md).
