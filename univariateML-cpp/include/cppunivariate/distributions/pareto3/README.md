---
id: pareto3
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape > 0, scale > 0 and min; min held; shape and/or scale free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Pareto type III
support: {type: real, lower: "min", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "if (shape < 1) Inf else if (shape > 1) -Inf else -log(scale)"}}
params:
- name: shape
  symbol: '\gamma'
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
  advice: 'With all coefficients free, shape below 1 makes the density singular as min approaches the sample minimum. The fully free likelihood is unbounded. Hold min, for example min = 0, to fit the shifted llogis family. Estimating min with other coefficients held is not supported.'
  kind: threshold
r:
  density: dpareto3
  sampler: rpareto3
  control:
    min: Known finite lower endpoint.
    reltol: "Relative accuracy of every free coefficient (default 1e-10)."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
    shape: Known positive finite coefficient; NULL estimates it.
    scale: Known positive finite coefficient; NULL estimates it.
limits:
  - name: unbounded likelihood
    firmness: proved
    when: The shape $\gamma$ tends to infinity while $\sigma$ equals the common excess above $\mu$.
    detect: The threshold is held, shape is free, and the sample is constant. Scale must be free or held at that common excess.
    status: Unbounded
    message: unbounded
log_density: log(shape) + log((1/scale)) + (shape - 1)*log((1/scale)*(x-min)) - 2*log(1 + ((1/scale)*(x-min))**shape)
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
  status: no-mle
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
  x <- rml(200, mlpareto3())
  mlpareto3(x, min = 0L)

group: Pareto and Lomax
construction:
- base
- generalisation
relations:
  special_case_of:
  - family: pareto4
    at:
      shape1: 1
      shape2: shape
      scale: scale
      min: min
  transform_of:
  - family: llogis
    transform: shift
    at:
      shape: shape
      scale: scale
    shift: min
fit:
  method: delegation
  certified: partial
  note: 'pareto3/README.md, Details; pareto3/mle.hpp (public fitter and held overloads); pareto3/test.cpp: Every supported subset with min held delegates to log-logistic on x-min, preserving its represented-likelihood certification flag, including held shape and held scale.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model thresholded sizes with a log-logistic excess distribution.
summary: Pareto type III provides a model for thresholded sizes with a log-logistic excess distribution.
aliases: []
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    With all coefficients free, every nonempty sample has unbounded likelihood.
    With $\mu$ held, a unique finite MLE exists for nonconstant observations
    strictly above $\mu$. Constant excesses have unbounded likelihood.
    Holding shape gives a unique scale estimate even for constant data.
    Holding scale gives a unique shape estimate unless every excess equals
    that scale, when the likelihood is unbounded. Free-threshold subsets
    are not supported; their existence depends on the held values.
    Holding all coefficients leaves likelihood evaluation.
  reason: >-
    For $0<\gamma<1$ the density near $\mu$ is proportional to
    $(x-\mu)^{\gamma-1}$, so moving $\mu$ up to the sample minimum makes
    the unrestricted likelihood unbounded. With $\mu$ held, put
    $z_i=\log(x_i-\mu)$, $b=\gamma$ and $a=\gamma\log\sigma$.
    Apart from a data-only term the likelihood is
    $n\log b-2\sum_i\log\{2\cosh((bz_i-a)/2)\}$.
    Its Hessian is negative definite. For nonconstant data its profiled
    maximum tends to $-\infty$ as $b$ tends to zero or infinity.
    Holding shape fixes $b$; holding scale restricts $a=b\log\sigma$.
    Strict concavity gives the stated conditional results.
---

Pareto type III models sizes above a threshold $\mu$ with log-logistic excesses. The scale $\sigma$ is the median excess. The shape $\gamma$ controls the spread of log excesses and the polynomial tail.

## d/p/q/r

### Concentrating shifted probabilities

When min=-scale, rounding an unshifted quantile and then adding min loses its leading displacement. The inverse uses the centered log odds and transports expm1(log-odds/shape) directly. Forward evaluation preserves the addition residual of min+scale before multiplying the log ratio by the shape.

## Details

Fitting requires finite observations strictly above a held threshold. On $\log(x_i-\mu)$ the problem is a logistic location-scale fit, so existence and uniqueness transfer from [log-logistic](../llogis/README.md). The log-logistic shape and scale are unchanged; in rate coordinates the rate is $1/\sigma$.

For constant observations at $x_0>\mu$, taking $\sigma=x_0-\mu$ and $\gamma\to\infty$ gives density $\gamma/[4(x_0-\mu)]$ at every observation. Holding shape removes this concentration path and gives one log-scale score root. Holding scale leaves unbounded likelihood only when all excesses equal it; otherwise there is a unique finite shape maximum. These conditional fits use the corresponding Burr submodel.

With a free threshold and $\gamma<1$, each of the $k$ observations at the sample minimum contributes an endpoint factor proportional to $\varepsilon^{\gamma-1}$ as the threshold approaches from below. The other factors tend to positive constants, so the product diverges. This is a proof for the fully free likelihood, not for every held-shape submodel. For example, with shape equal to one and scale held the threshold has a finite maximum at the sample minimum. The interface still requires the threshold to be held.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md) describes the limiting paths and boundary comparisons.

For an uncertified log-logistic parent, the local correction uses centered
log ratios of the original excesses. A rounded likelihood comparison cannot
undo that correction: adjacent scales can have indistinguishable likelihoods
while the shape error remains resolvable through the information matrix.

## R notes

With `min = 0`, the joint fit agrees with `mlllogis(x)`. Failed conditional searches stop rather than return an unresolved estimate.

The logarithm of the distance from the held minimum converts scale into a
logistic location. Coefficient accuracy must therefore account for the coupled
shape/location errors and the exponential map back to the public scale.
