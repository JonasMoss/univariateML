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
  - "Hogg, R. V. and Klugman, S. A. (1984), Loss Distributions, Wiley."
  - "Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37."
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
  certified: unknown
  note: Primary route from the 2026-10-30 fitter inventory; guarantees across held subsets are not classified.
areas:
- actuarial
- reliability and survival
classification: uncommon
classification_reason: A less routinely used extension within this catalog; prevalence has not been independently
  surveyed.
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
