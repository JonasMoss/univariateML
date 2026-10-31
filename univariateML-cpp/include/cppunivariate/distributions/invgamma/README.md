---
id: invgamma
name: InvGamma
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: alpha
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: beta
  symbol: '\beta'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dinvgamma
  sampler: rinvgamma
  control:
    alpha: Optional held alpha; NULL estimates it.
    beta: Optional held beta; NULL estimates it.
    abstol: Absolute step tolerance in alpha.
    iterlim: Maximum number of iterations.
log_density: alpha*log(beta) - loggamma(alpha) - (alpha + 1)*log(x) - beta/x
references:
- 'Choi, S. C, and R. Wette. "Maximum likelihood estimation of the parameters of the gamma distribution and their
  bias." Technometrics 11.4 (1969): 683-690.'
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 17.
  Wiley, New York.
- 'Witkovsky, V. (2001). "Computing the Distribution of a Linear Combination of Inverted Gamma Variables". Kybernetika.
  37 (1): 79–90'
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
examples: |
  set.seed(1)
  x <- rml(200, mlinvgamma())
  mlinvgamma(x)

group: gamma and Weibull
construction:
- transformation
relations:
  transform_of:
  - family: gamma
    transform: inverse
    at:
      shape: alpha
      scale: 1/beta
  special_case_of:
  - family: invtrgamma
    at:
      shape1: alpha
      shape2: 1
      scale: beta
fit:
  method: delegation
  certified: 'yes'
  note: 'invgamma/README.md, Details; invgamma/mle.hpp (public fitter and held overloads); invgamma/test.cpp: Gamma
    shape has a unique monotone digamma equation; scale is analytic, with bracketed fallback and held-coordinate
    solves. Transformation preserves the maximum.'
areas:
- actuarial
- reliability and survival
classification: classical
classification_reason: A standard model for positive inverse scales and loss sizes with a tunable polynomial tail.
summary: InvGamma provides a model for positive inverse scales and loss sizes with a tunable polynomial tail.
aliases:
- "inverse gamma"
- "inverted gamma"
- "scaled inverse chi-squared"
- "Vinci"
key_references:
- 4
---

The inverse gamma distribution is the distribution of the reciprocal of a gamma variable.

## Details

The parameter `beta` is the rate of the gamma variable `1/x`, and is the scale parameter in the inverse gamma density.

Holding either alpha or beta permits constant data; the unconstrained fit has unbounded likelihood there.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
