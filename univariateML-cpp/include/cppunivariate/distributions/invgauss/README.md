---
id: invgauss
name: Inverse Gaussian
support: {type: real, lower: 0.0, upper: inf, closed: [false, false]}
params:
- name: mean
  symbol: '\mu'
  role: location
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape
  symbol: '\lambda'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dinvgauss
  sampler: rinvgauss
  control:
    mean: Optional held mean; NULL estimates it.
    shape: Optional held shape; NULL estimates it.
log_density: log(shape)/2 - log(2*pi)/2 - 3*log(x)/2 - shape*(x-mean)**2/(2*mean**2*x)
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 15.
  Wiley, New York.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
limits:
- name: unbounded likelihood
  fixed: [mean]
  when: Every observation equals held mean.
  status: Unbounded
  message: constant data
examples: |
  x <- datasets::airquality$Wind[!is.na(datasets::airquality$Wind)]
  mlinvgauss(x)

group: other positive
construction:
- base
relations: {}
fit:
  method: closed-form
  certified: 'yes'
  note: 'invgauss/README.md, Details; invgauss/mle.hpp (public fitter and held overloads); invgauss/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- reliability and survival
classification: classical
classification_reason: A standard model for first-passage times of Brownian motion with positive drift.
summary: Inverse Gaussian provides a model for first-passage times of Brownian motion with positive drift.
aliases:
- Wald
key_references:
- 2
---

The inverse Gaussian (Wald) distribution models positive quantities, including first-passage times of Brownian motion with positive drift.

## Details

The `mean` parameter is the positive mean and `shape` is the positive inverse Gaussian shape, rather than a dispersion. For nonconstant positive data the MLE is finite. Although the family has a Levy limit as the mean tends to infinity with shape fixed, no limit status is reported by this fitter.

With mean held and shape free, the likelihood is unbounded if every observation equals that mean. A held positive shape permits constant data. Held parameters must be finite.

### Diagnostic status contract (2026-10-31)

Invalid and empty samples carry non-success statuses, including delegate paths.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp`.
