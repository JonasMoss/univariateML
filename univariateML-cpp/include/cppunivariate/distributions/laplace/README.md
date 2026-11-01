---
id: laplace
inference_numeric:
  - regime: "scale with location held"
    available: true
    condition: "finite mu held; finite sigma > 0 free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#laplace"
  - regime: "location free"
    available: false
    condition: "finite mu and sigma > 0; location free"
    reason: "the log density has a moving absolute-value kink; the pointwise Hessian misses sensitivity"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#laplace"
name: Laplace
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sigma
  symbol: 'b'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: dlaplace
  sampler: rlaplace
  control:
    sigma: Optional held sigma; NULL estimates it.
    mu: Optional held mu; NULL estimates it.
log_density: -log(2*sigma) - Abs(x-mu)/sigma
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 24.
  Wiley, New York.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds: []
limits:
  - name: scale collapse
    firmness: proved
    when: 'The scale $b$ tends to zero at the estimated or held location $\mu$.'
    detect: Scale is free and every observation equals the estimated or held location.
    status: Unbounded
    message: "does not exist"
examples: |
  set.seed(1)
  x <- rml(200, mllaplace())
  mllaplace(x)

group: skewed and robust location-scale
construction:
- location-scale
relations:
  special_case_of:
  - family: ged
    at:
      mean: mu
      sd: sqrt(2)*sigma
      nu: 1
fit:
  method: closed-form
  certified: 'yes'
  note: 'laplace/README.md, Details; laplace/mle.hpp (public fitter and held overloads); laplace/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- robust modelling
classification: classical
classification_reason: A standard model for symmetric errors with a sharp central peak and exponential tails.
summary: Laplace provides a model for symmetric errors with a sharp central peak and exponential tails.
aliases:
- "double exponential"
key_references:
- 2
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a finite MLE exists exactly for nonconstant
    samples. Its locations are all sample medians, and its scale is unique.
    The location is unique exactly when the median interval is a singleton.
    With location held, a unique positive scale MLE exists unless every
    observation equals that location, when the likelihood is unbounded.
    With positive scale held, all sample medians maximize the likelihood,
    including the unique common value for constant samples.
    Holding both coefficients leaves likelihood evaluation.
  reason: >-
    Let $A(\mu)=\sum_i|x_i-\mu|$. This convex function is minimized
    precisely on the median interval and is constant there.
    For $A>0$, maximizing $-n\log b-A/b$ gives the unique scale $b=A/n$.
    The profiled likelihood decreases strictly in $A$, so all and only
    medians maximize it. If $A=0$, the likelihood grows without bound
    as $b\to0$.
---

The Laplace (double exponential) distribution models symmetric errors with a sharp central peak and exponential tails. Its location $\mu$ is the centre, and its positive scale $b$ sets the mean absolute deviation from that centre.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) derives the boundary conditions for free and held coefficients.

The location estimates are the sample medians. Scale is the mean absolute deviation from any such median, with the same value throughout the median interval. For even sample size, every point between the two central order statistics maximizes the likelihood. The fit selects the upper endpoint of that interval.

Holding location estimates scale from absolute residuals about the supplied value. Collapse occurs only when all those residuals vanish. Holding positive scale permits constant samples and leaves the same median rule for location. Holding both coefficients evaluates the likelihood.

## R notes

Supplying `mu` or `sigma` holds that coefficient. Constant data stop when scale is free and the fitted or held location equals their common value.

## Decision history

2026-10-31 (diagnostic status contract): Empty or invalid input carries a non-success status. Degeneracy depends on which coefficients are free, and delegate failures survive transformations. See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md) and the appended TASK-132 cases in `test.cpp` for the validation scope.
