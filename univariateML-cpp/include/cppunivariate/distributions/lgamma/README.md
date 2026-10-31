---
id: lgamma
name: Log-gamma
support: {type: real, lower: 1, upper: inf, closed: [false, false]}
params:
- name: shapelog
  symbol: 'k'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: ratelog
  symbol: '\lambda'
  role: rate
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dlgamma
  sampler: rlgamma
  control:
    shapelog: Optional held shapelog; NULL estimates it.
    ratelog: Optional held ratelog; NULL estimates it.
    abstol: Absolute step tolerance in shapelog.
    iterlim: Maximum number of iterations.
log_density: shapelog*log(ratelog) - loggamma(shapelog) + (shapelog - 1)*log(log(x)) - (ratelog + 1)*log(x)
references:
- Hogg, R. V. and Klugman, S. A. (1984), Loss Distributions, Wiley.
- 'Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical
  Software, 25(7), 1-37.'
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
examples: |
  set.seed(1)
  x <- rml(200, mllgamma())
  mllgamma(x)

group: gamma and Weibull
construction:
- transformation
relations:
  transform_of:
  - family: gamma
    transform: log
    at:
      shape: shapelog
      scale: 1/ratelog
fit:
  method: delegation
  certified: 'yes'
  note: 'lgamma/README.md, Details; lgamma/mle.hpp (public fitter and held overloads); lgamma/test.cpp: Gamma shape
    has a unique monotone digamma equation; scale is analytic, with bracketed fallback and held-coordinate solves.
    Transformation preserves the maximum.'
areas:
- actuarial
- reliability and survival
classification: specialist
classification_reason: Used to model insurance sizes above one whose logarithms have gamma variation.
summary: Log-gamma provides a model for insurance sizes above one whose logarithms have gamma variation.
aliases: []
key_references:
- 3
---

The log-gamma distribution describes positive data whose logarithm follows a gamma distribution.

## Details

The support is above 1: `log(x)` has gamma shape `shapelog` and rate `ratelog`. This is the actuar log-gamma convention, rather than the distribution of the logarithm of a gamma variable.

Holding either shapelog or ratelog gives a unique finite fit even for constant data above one. With both free the likelihood is unbounded for constant data.

### C++ independence (2026-10-30)

`lgamma_mle` owns the previous R adapter's exact transform and sum order:
fit gamma to `log(x)` and subtract `sum(log(x))` from its log likelihood.
The change-of-variables identity is exact mathematically. NaN marks free
coefficients; held ratelog is passed as reciprocal scale and returned verbatim.
Gamma iteration failure is preserved; constant and precision-collapsed
transforms are distinguished. No new search or convergence guarantee is added.
The appended C++ regression compares all four held subsets with the previous
adapter computation; golden agreement is numerical evidence.
