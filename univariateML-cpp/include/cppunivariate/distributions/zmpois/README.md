---
id: zmpois
name: Zero-modified Poisson
support:
  type: integer
  lower: 0
  upper: inf
  closed:
  - true
  - false
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: p0
  symbol: 'p_0'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzmpois
  sampler: rzmpois
  control:
    lambda: "Optional held lambda; NULL estimates it."
    p0: "Optional held p0; NULL estimates it."
log_density: Piecewise((log(p0), Eq(x, 0)), (log(1-p0)+(-lambda+x*log(lambda)-loggamma(x+1)-log(1-exp(-lambda))), True))
examples: |
  set.seed(1)
  x <- rml(200, mlzmpois())
  mlzmpois(x)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: pois
    at:
      lambda: lambda
    type: zero-modified
    mass: p0
fit:
  method: multistart
  certified: 'yes'
  note: 'zmpois/README.md, Details; zmpois/mle.hpp (public fitter and held overloads); zmpois/test.cpp: Conditional
    mean is strictly increasing in log rate (derivative is conditional variance), giving a unique bracketed root;
    bounds profile to sample extremes and zero mass separates analytically.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model Poisson positive counts with a separately fitted zero probability.
summary: Zero-modified Poisson provides a model for Poisson positive counts with a separately fitted zero probability.
aliases:
- "hurdle Poisson"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
---

Zero-modified Poisson distributions model counts with a separate zero component.

## Details

With no positives the fitter returns `p0 = 1` and conventional base parameters with a warning. With no zeros it returns `p0 = 0`. Positive observations all equal to one give an attained two-point law; the fitter warns that `mlbinom(x, size = 1)` fits it exactly.

The zero-indicator likelihood and the positive conditional likelihood factorize. Holding `p0`, including its empirical optimum, leaves the positive conditional fit unchanged. Held `p0 = 0` with zeros or `p0 = 1` with positives is incompatible and stops. With no positives, base parameters are unidentified; supplied values are preserved and free base parameters use conventional representatives.
