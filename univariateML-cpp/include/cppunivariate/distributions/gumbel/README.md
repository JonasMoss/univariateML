---
id: gumbel
inference_numeric:
  - regime: interior
    available: true
    condition: "finite mu and finite sigma > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Gumbel
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
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
  density: dgumbel
  sampler: rgumbel
  control:
    sigma: Optional held sigma; NULL estimates it.
    mu: Optional held mu; NULL estimates it.
    reltol: Relative step tolerance in log(sigma) of the underlying Gumbel fit.
    iterlim: Maximum number of iterations.
log_density: -log(sigma) - (x-mu)/sigma - exp(-(x-mu)/sigma)
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 22.
  Wiley, New York.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds: []
limits:
  - name: scale collapse
    firmness: proved
    when: 'The scale $\sigma$ tends to zero at the estimated or held location $\mu$.'
    detect: Scale is free and every observation equals the estimated or held location.
    status: Unbounded
    message: "unbounded"
examples: |
  set.seed(1)
  x <- rml(200, mlgumbel())
  mlgumbel(x)

group: extreme value
construction:
- base
relations:
  special_case_of:
  - family: gev
    at:
      mu: mu
      sigma: sigma
      xi: 0
fit:
  method: profile-search
  certified: 'yes'
  note: 'gumbel/README.md, Details; gumbel/mle.hpp (public fitter and held overloads); gumbel/test.cpp: Gumbel location
    is profiled exactly and its scale equation has a unique root; held location has a strictly concave inverse-scale
    likelihood. Log/reciprocal transformations preserve this structure.'
areas:
- extremes and environment
classification: classical
classification_reason: A standard model for block maxima in the exponential-tail extreme-value domain.
summary: Gumbel provides a model for block maxima in the exponential-tail extreme-value domain.
aliases:
- "type I extreme value"
- "type I Fisher–Tippett"
- "maximum extreme value"
key_references:
- 2
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a unique finite MLE exists for every
    nonconstant finite sample. Constant samples have unbounded likelihood.
    With location held, a unique positive scale MLE exists unless every
    observation equals that location. With positive scale held, the location
    MLE is unique even for constant data. Holding both coefficients leaves
    likelihood evaluation.
  reason: >-
    Profiling location gives
    $\mu(\sigma)=-\sigma\log(n^{-1}\sum_i e^{-x_i/\sigma})$.
    The scale equation is $F(\sigma)=\sigma+E_\sigma[x]-\bar x=0$,
    with weights proportional to $e^{-x_i/\sigma}$.
    Its derivative is $1+\operatorname{Var}_\sigma(x)/\sigma^2>0$.
    For nonconstant data, its limit at zero is $\min_i x_i-\bar x<0$,
    and it tends to infinity with scale. The profile score has sign $-F$,
    giving a unique maximum. With location held, put $t=1/\sigma$
    and $d_i=x_i-\mu$. The inverse-scale score
    $n/t-\sum_i d_i(1-e^{-td_i})$ has derivative
    $-n/t^2-\sum_i d_i^2e^{-td_i}<0$ and crosses zero whenever
    some $d_i\ne0$. All-zero residuals instead give unbounded collapse.
---

The Gumbel distribution models maxima in the exponential-tail extreme-value domain. Its location $\mu$ shifts the distribution, and its positive scale $\sigma$ sets the spread. This is the convention for maxima.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) derives the boundary conditions for free and held coefficients.

With both coefficients free, location profiles out exactly and scale solves a strictly monotone scalar equation. Nonconstant samples have a unique finite maximum. Constant data allow scale to shrink to zero at their common value, giving unbounded likelihood.

Holding location leaves a strictly concave likelihood in inverse scale. It has a unique maximum unless all observations equal the supplied location. Holding positive scale gives an explicit unique location estimate and permits constant samples. Holding both coefficients evaluates the likelihood.

## R notes

Supplying `mu` or `sigma` holds that coefficient. The free joint fit uses Newton iteration in log scale. Failure to converge stops the fit.

## Decision history

2026-10-31 (diagnostic status contract): Empty or invalid input carries a non-success status. Degeneracy depends on which coefficients are free, and delegate failures survive transformations. See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md) and the appended TASK-132 cases in `test.cpp` for the validation scope.
