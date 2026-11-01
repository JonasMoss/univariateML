---
id: invpareto
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape > 0 and scale > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Inverse Pareto
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: '\theta'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dinvpareto
  sampler: rinvpareto
  control:
    shape: Optional held shape; NULL estimates it.
    scale: Optional held scale; NULL estimates it.
    scale0: Optional positive finite starting value; NULL uses the automatic start.
    reltol: Tolerance for the underlying fit.
    iterlim: Maximum number of iterations.
log_density: log(shape) + log(scale) - 2*log(x) - (shape+1)*log(1+scale/x)
limits:
  - model: invexp
    when: '$\alpha$ tends to infinity while $\alpha\theta$ tends to a positive inverse exponential scale'
    detect: 'both coefficients are free and the numerical profile search on the reciprocals finds no improvement over the best inverse exponential larger than $10^{-8}n$ log-likelihood units'
    status: ExponentialLimit
    numerical: true
references:
- del Castillo, J. and Daoudi, J. (2009). Estimation of the generalized Pareto distribution. Statistics & Probability
  Letters 79(5), 684–688. doi:10.1016/j.spl.2008.10.021.
- Grimshaw, S. D. (1993). Computing maximum likelihood estimates for the generalized Pareto distribution. Technometrics
  35(2), 185–191. doi:10.1080/00401706.1993.10485040.
- Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition,
  Wiley.
- 'Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical
  Software, 25(7), 1-37.'
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
examples: |
  set.seed(1)
  x <- rml(200, mlinvpareto())
  mlinvpareto(x)

group: Pareto and Lomax
construction:
- base
relations:
  special_case_of:
  - family: invburr
    at:
      shape1: shape
      shape2: 1
      scale: scale
  - family: betapr
    at:
      shape1: shape
      shape2: 1
      scale: scale
  transform_of:
  - family: lomax
    at:
      lambda: scale
      kappa: shape
    transform: inverse
fit:
  method: delegation
  certified: partial
  note: 'invpareto/README.md, Details; invpareto/mle.hpp (public fitter and held overloads); invpareto/test.cpp:
    Either held coefficient gives a unique finite fit; joint scale profile and exponential-face comparison remain
    numerical rather than a global certificate.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model positive loss sizes using the reciprocal Lomax family.
summary: Inverse Pareto provides a model for positive loss sizes using the reciprocal Lomax family.
aliases: []
key_references:
- 5
existence:
  uniqueness: unknown
  statement: >-
    With both coefficients free, a finite MLE exists exactly when the
    Lomax MLE for the reciprocals $1/x_i$ exists. Otherwise the supremum
    is the inverse exponential limit. Joint uniqueness is not established.
    Holding either shape or scale gives a unique finite MLE for every
    nonempty positive sample, including constant data.
  reason: >-
    The transformation $z_i=1/x_i$ gives a Lomax likelihood with
    $\lambda=\theta$ and $\kappa=\alpha$, plus the parameter-independent
    log-Jacobian $-2\sum_i\log x_i$. It preserves existence and uniqueness
    for every held subset. The Lomax profile comparison below gives the
    exact existence criterion.
---

The inverse Pareto distribution models positive heavy-tailed data as the reciprocal of a Lomax variable.

## Details

The coefficient $\theta$ is a scale for the observations, and $\alpha$ controls their shape. Taking reciprocals gives a Lomax distribution with rate $\theta$ and shape $\alpha$. The fit uses this transformation and includes its Jacobian in the likelihood.

Put $y_i=(1/x_i)/\overline{1/x}$, $\psi(u)=n^{-1}\sum_i\log(e^{-u}+uy_i)$ and $\bar u=\overline{1/x}(\prod_i x_i)^{1/n}$. The MLE exists exactly when $\psi(u)\le0$ for some $0<u<\bar u$. The Lomax argument compares the profiled likelihood with its exponential limit; reciprocation turns that limit into the inverse exponential. The bounded profile search is numerical and has no global certificate or proof of joint uniqueness.

The condition $\overline{1/x^2}<2\overline{1/x}^{\,2}$ decides only local behaviour at the limit. The sample $(1,0.01)$ satisfies it but has a finite MLE. For two observations, the numerical transition is at a larger-to-smaller ratio of approximately 28.81; the exact criterion above handles equality.

Holding either shape or scale removes the inverse exponential escape and gives a unique finite fit, even for constant data. Holding both evaluates the likelihood at the supplied coefficients.

## R notes

The implementation prefers the inverse exponential limit when the largest profile improvement it finds is at most $10^{-8}n$ log-likelihood units and stops with a recommendation to use `mlinvexp`. This tolerance can reject a mathematical MLE that ties or only slightly improves on the limit.
