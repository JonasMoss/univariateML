---
id: laplace
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
    when: Scale tends to zero at the fitted or held location.
    detect: Every observation equals the fitted or held location.
    status: Unbounded
    message: "does not exist"
examples: |
  set.seed(1)
  x <- rml(200, mllaplace())
  mllaplace(x)

group: skewed and robust location-scale
construction:
- location-scale
relations: {}
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
- Double exponential
key_references:
- 2
---

The Laplace (double exponential) distribution is a symmetric location-scale model with exponential tails.

## Details

The location estimate is a sample median and the scale estimate is the mean absolute deviation from it. For even sample size, every point between the two central order statistics maximises the location likelihood; this fitter returns the upper endpoint of that interval.


With mu held, sigma is the mean absolute residual about that value. Scale collapses only when every observation equals the held mu. Holding sigma permits constant samples.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
