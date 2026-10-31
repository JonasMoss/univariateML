---
id: gumbel
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
    when: Scale tends to zero at the fitted or held location.
    detect: Every observation equals the fitted or held location.
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
---

The Gumbel distribution models maxima and extreme measurements.

## Details

This is the Gumbel distribution for maxima; `sigma` is positive.


Holding mu gives a unique inverse-scale maximum unless every observation equals that mu. Holding sigma gives an exact location profile and permits constant samples.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
