---
id: lomax
name: Lomax
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: kappa
  symbol: '\kappa'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dlomax
  sampler: rlomax
  control:
    lambda: Optional held lambda; NULL estimates it.
    kappa: Optional held kappa; NULL estimates it.
    lambda0: Optional starting value for `lambda`; `NULL` uses a method-of-moments start.
    reltol: Relative tolerance on the Newton step for `lambda * mean(x)`, with an absolute floor of the same size in these dimensionless units.
    iterlim: Maximum number of Newton iterations.
log_density: log(lambda) + log(kappa) - (kappa + 1)*log(1 + lambda*x)
limits:
  - model: exp
    when: '$\kappa \to \infty$ with $\lambda \kappa$ fixed'
    detect: max over 0 < u < ubar of the profile log-likelihood gap is at most 1e-8*n
    status: ExponentialLimit
references:
- del Castillo, J. and Daoudi, J. (2009). Estimation of the generalized Pareto distribution. Statistics & Probability
  Letters 79(5), 684–688. doi:10.1016/j.spl.2008.10.021.
- Grimshaw, S. D. (1993). Computing maximum likelihood estimates for the generalized Pareto distribution. Technometrics
  35(2), 185–191. doi:10.1080/00401706.1993.10485040.
- Kleiber, C. and Kotz, S. (2003). Statistical Size Distributions in Economics and Actuarial Sciences. Wiley, p.
  60.
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
examples: |
  set.seed(1)
  x <- rml(200, mllomax())
  mllomax(x)

group: Pareto and Lomax
construction:
- base
relations:
  special_case_of:
  - family: burr
    at:
      shape1: kappa
      shape2: 1
      scale: 1/lambda
  - family: pareto2
    at:
      shape: kappa
      scale: 1/lambda
      min: 0
  - family: betapr
    at:
      shape1: 1
      shape2: kappa
      scale: 1/lambda
fit:
  method: profile-search
  certified: partial
  note: 'lomax/README.md, Details; lomax/mle.hpp (public fitter and held overloads); lomax/test.cpp: Either held
    coefficient gives a unique finite fit; joint scale profile and exponential-face comparison remain numerical
    rather than a global certificate.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model positive losses and waiting times with a Pareto tail from zero.
summary: Lomax provides a model for positive losses and waiting times with a Pareto tail from zero.
aliases: []
key_references:
- 4
---

The Lomax (Pareto type II) distribution is a heavy-tailed distribution on
the positive half-line, used for incomes, losses and waiting times.

## Details

The exact existence criterion is $\psi(u)\leq0$ for some $0<u<\bar u$, where $y_i=x_i/\bar x$, $\psi(u)=n^{-1}\sum_i\log(e^{-u}+uy_i)$ and $\bar u=\bar x/(\prod_i x_i)^{1/n}$; the implementation prefers the exponential face within $10^{-8}n$ log-likelihood units. The moment condition decides only local behaviour at that face: samples with `mean(x^2) < 2*mean(x)^2` can have an MLE, for example `c(1, 100)`. A pair has an MLE iff larger/smaller exceeds approximately 28.81.

Holding either lambda or kappa gives a unique finite fit for positive samples, including constant data. The exponential escape concerns the fit with both free.
