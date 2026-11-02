---
id: pareto2
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape > 0, scale > 0 and min; min held; shape and/or scale free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support"
name: Pareto type II
support: {type: real, lower: "min", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "log(shape) - log(scale)"}}
params:
- name: shape
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: min
  symbol: '\mu'
  role: lower endpoint
  domain: (-inf, inf)
  simulate: 0
  advice: 'With all coefficients free, letting min approach the sample minimum while scale shrinks can make the likelihood unbounded. Hold min, for example min = 0, to fit the shifted lomax family. Estimating min with other coefficients held is not supported.'
  kind: threshold
r:
  density: dpareto2
  sampler: rpareto2
  control:
    min: Known finite lower endpoint.
    reltol: Delegate fitting tolerance.
    iterlim: Maximum delegate iterations.
    shape: Known positive finite coefficient; NULL estimates it.
    scale: Known positive finite coefficient; NULL estimates it.
log_density: log((1/scale)) + log(shape) - (shape + 1)*log(1 + (1/scale)*(x-min))
limits:
- when: The shape $\alpha$ and scale $\sigma$ tend to infinity with $\alpha/\sigma$ fixed at a positive rate.
  firmness: proved
  detect: With threshold held and both other coefficients free, the numerical profile search finds no improvement over the best shifted exponential greater than $10^{-8}n$ log-likelihood units.
  status: ExponentialLimit
  name: shifted exp; subtract min before fitting
  numerical: true
holds:
- held: []
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape
  status: not-implemented
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - scale
  status: not-implemented
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape
  - scale
  status: not-implemented
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
fit_args:
  min: 0
examples: |
  set.seed(1)
  x <- rml(200, mlpareto2())
  mlpareto2(x, min = 0L)

group: Pareto and Lomax
construction:
- base
- generalisation
relations:
  special_case_of:
  - family: pareto4
    at:
      shape1: shape
      shape2: 1
      scale: scale
      min: min
  - family: gpd
    at:
      mu: min
      sigma: scale/shape
      xi: 1/shape
  transform_of:
  - family: lomax
    transform: shift
    at:
      kappa: shape
      lambda: 1/scale
    shift: min
fit:
  method: delegation
  certified: partial
  note: 'pareto2/README.md, Details; pareto2/mle.hpp (public fitter and held overloads); pareto2/test.cpp: Held
    threshold transfers to Lomax, log-logistic or Burr; conditional monotone subsets are certified but joint excess
    profiles inherit numerical-search qualifications. Free thresholds stop.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model thresholded income or loss sizes with a Lomax excess distribution.
summary: Pareto type II provides a model for thresholded income or loss sizes with a Lomax excess distribution.
aliases: []
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, the likelihood is unbounded for every nonempty
    sample. With $\mu$ held and all observations strictly above it, existence
    is exactly the Lomax existence criterion on the excesses. The exponential
    limit may supply the supremum, and joint uniqueness is not established.
    Holding either shape or scale as well gives a unique finite estimate of
    the remaining coefficient. Free-threshold fits with held coefficients are
    not supported, although some have finite maxima. Holding all coefficients
    leaves likelihood evaluation.
  reason: >-
    If the sample minimum $m$ occurs $k$ times, set $\mu=m-\varepsilon$
    and $\sigma=\varepsilon$. The likelihood has order
    $\varepsilon^{\alpha(n-k)-k}$, which diverges for sufficiently small
    positive $\alpha$. With $\mu$ held, subtracting it gives precisely a
    Lomax likelihood with $\kappa=\alpha$ and $\lambda=1/\sigma$.
    With scale held the shape likelihood is strictly concave. With shape
    held the score in $\log\lambda$ decreases strictly from $n$ to $-n\alpha$.
---

Pareto type II models sizes above a lower endpoint $\mu$ with Lomax-distributed excesses. The scale $\sigma$ sets the size unit and the shape $\alpha$ controls tail decay.

## Details

Fitting holds the threshold fixed and requires finite observations strictly above it. Subtracting the threshold reduces the fit to [Lomax](../lomax/README.md), with shape $\kappa=\alpha$ and rate $\lambda=1/\sigma$.

For the joint excess fit, put $y_i=x_i-\mu$, $z_i=y_i/\bar y$, $\bar u=\bar y/(\prod_i y_i)^{1/n}$ and $\psi(u)=n^{-1}\sum_i\log(e^{-u}+uz_i)$. A finite maximum exists exactly when $\psi(u)\le0$ somewhere in $0<u<\bar u$. Otherwise the supremum is the shifted exponential limit. This profile comparison does not establish uniqueness. The bounded numerical search refines candidate maxima without a global certificate.

Holding scale gives $\hat\alpha=n/\sum_i\log(1+y_i/\sigma)$. Holding shape gives a unique scale-score root. Both cases permit constant positive excesses. If shape and scale are held while the threshold is free, the likelihood increases up to the sample minimum, giving a finite endpoint maximum. The interface nevertheless requires a held threshold for every subset.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md) gives the endpoint-collapse argument and boundary comparisons.

## R notes

With `min = 0`, the fit agrees with `mllomax(x)` after reciprocal conversion of the rate. Numerical comparisons prefer the exponential face within $10^{-8}n$ log-likelihood units, so a tied or slightly better finite maximum can be rejected. A detected face stops with a recommendation to use `mlexp(x - min)`. Unresolved searches stop.
