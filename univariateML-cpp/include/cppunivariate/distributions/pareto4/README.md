---
id: pareto4
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0, shape2 > 0, scale > 0 and min; min held; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Pareto type IV
support: {type: real, lower: "min", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "if (shape2 < 1) Inf else if (shape2 > 1) -Inf else log(shape1) - log(scale)"}}
params:
- name: shape1
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape2
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
  advice: 'With all coefficients free, shape2 below 1 makes the density singular as min approaches the sample minimum. The fully free likelihood is unbounded. Hold min, for example min = 0, to fit the shifted burr family. Estimating min with other coefficients held is not supported.'
  kind: threshold
r:
  density: dpareto4
  sampler: rpareto4
  control:
    min: Known finite lower endpoint.
    reltol: Delegate fitting tolerance.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
    shape1: Known positive finite coefficient; NULL estimates it.
    shape2: Known positive finite coefficient; NULL estimates it.
log_density: log(shape1) + log(shape2) - log((x-min)) - shape2*log(scale/(x-min)) - (shape1+1)*log(1+((x-min)/scale)**shape2)
limits:
- name: unbounded likelihood
  firmness: proved
  when: A free power $\gamma$ tends to infinity while the other free coefficients concentrate the excess distribution at its common observed value.
  detect: With threshold held, the excesses are constant and the held coefficients permit concentration. In particular, this occurs with both shapes and scale free.
  status: Unbounded
  message: unbounded
- when: The shape $\alpha$ tends to infinity with $\gamma$ fixed and $\sigma\alpha^{-1/\gamma}$ fixed at a positive scale.
  firmness: proved
  detect: With all regular coefficients free, the Burr certificate selects this face. Held power uses the certified Lomax comparison; held scale with both shapes free uses a strictly concave scalar certificate.
  status: WeibullBoundary
  name: shifted weibull; subtract min before fitting
  numerical: true
- when: The power $\gamma$ tends to infinity with $\alpha\gamma$ fixed at a positive tail exponent and $\sigma$ fixed.
  firmness: proved
  detect: With all regular coefficients free, the Burr certificate selects this face. Held power uses the certified Lomax comparison; held scale with both shapes free uses a strictly concave scalar certificate.
  status: ParetoBoundary
  name: shifted pareto; subtract min before fitting
  numerical: true
- when: The power $\gamma$ tends to infinity with $\alpha\gamma$ fixed at a positive tail exponent and $\sigma$ fixed.
  firmness: proved
  detect: Scale and threshold are held, both shapes are free, and every excess is at least $\sigma$. If all excesses equal $\sigma$, the likelihood is instead unbounded.
  status: AllGeOne
  fixed: [scale]
  name: shifted Pareto with its endpoint held at min + scale
holds:
- held: []
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape1
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape2
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape1
  - shape2
  status: not-implemented
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - scale
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape1
  - scale
  status: no-mle
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape2
  - scale
  status: not-implemented
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
- held:
  - shape1
  - shape2
  - scale
  status: not-implemented
  reason: 'Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.'
fit_args:
  min: 0
examples: |
  set.seed(1)
  x <- rml(200, mlpareto4())
  mlpareto4(x, min = 0L)

group: Pareto and Lomax
construction:
- base
- generalisation
relations:
  special_case_of:
  - family: fpareto
    at:
      shape1: shape1
      shape2: shape2
      shape3: 1
      scale: scale
      min: min
  transform_of:
  - family: burr
    transform: shift
    at:
      shape1: shape1
      shape2: shape2
      scale: scale
    shift: min
fit:
  method: delegation
  certified: partial
  note: 'Joint and held-scale excess fits transport the Burr certificate or its uncertified search through the held threshold. Concentrated held-first-shape fits preserve centered excess spacings. Subtraction overflow and artificial ties remain representation failures.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model thresholded income and losses with a Burr excess distribution.
summary: Pareto type IV provides a model for thresholded income and losses with a Burr excess distribution.
aliases: []
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, every nonempty sample has unbounded likelihood.
    Holding $\mu$ gives the Burr likelihood on the excesses. Constant excesses
    have unbounded joint likelihood, while nonconstant samples may favour
    Weibull or Pareto limits. The finite/face existence criterion is inherited from Burr; general joint
    uniqueness is not established, but accepted finite certificates prove it for the sample. Holding $\alpha$ gives a unique finite
    fit on nonconstant excesses. Holding $\gamma$ reduces existence to Lomax
    on powered excesses. Holding both shapes gives a unique scale estimate.
    Holding scale alone gives a unique finite maximum for nonconstant excesses if
    some excess is below it, by strict concavity of the shape profile. Other conditional
    cases are described below. Free-threshold fits are not supported.
    Holding all coefficients leaves likelihood evaluation.
  reason: >-
    A free threshold approaching the sample minimum with $\gamma<1$ creates
    a density singularity of order $(x-\mu)^{\gamma-1}$.
    With $\mu$ held, subtracting it gives Burr exactly. With $\alpha$ held,
    the log-excess likelihood is strictly concave in $\gamma$ and
    $\gamma\log\sigma$ and has coercive profiles for nonconstant data.
    With $\gamma$ held, the transformation $(x_i-\mu)^\gamma$ gives Lomax.
    The single-coordinate scores decrease strictly in suitable coordinates.
    The separate Burr certificate certifies accepted joint regular fits.
---

Pareto type IV models sizes above a threshold $\mu$ with Burr-distributed excesses. The scale $\sigma$ sets the size unit. The shapes $\alpha$ and $\gamma$ give upper-tail exponent $\alpha\gamma$ and lower-end power $\gamma$.

## d/p/q/r

At shape2 $10^8$, density evaluation near the transition at `min + scale`
must retain the displacement from that transition. Subtracting two logarithms
loses accuracy even at scale 1000; a centered log ratio resolves the
high-precision regression at x = 999.9976974175579.

## Details

Fitting requires finite observations strictly above a held threshold. Subtracting that threshold gives [Burr](../burr/README.md) with unchanged shapes and scale. The joint fit uses the Burr global certificate, including root isolation, negative profile curvature, dual/chord envelopes and both tail exclusions. Certified finite results have a sample-specific unique global maximum certificate; unresolved comparisons retain a successful finite search as uncertified.

With only scale held, nonconstant excesses all at least $\sigma$ have a supremum on the endpoint-$\sigma$ Pareto path. Its log-likelihood is the held-endpoint Pareto log-likelihood minus $Z\log2$, where $Z$ counts excesses equal to $\sigma$. The endpoint remains $\mu+\sigma$, so estimating it anew would change the constrained model. For nonconstant data with an excess below $\sigma$, a finite maximum exists. Constant excesses at or below $\sigma$ instead have unbounded likelihood with both shapes free. Constant excesses above it have a finite unattained Pareto path limit.

Holding $\alpha$ permits a unique joint scale/power fit for nonconstant excesses, but constant data remain unbounded when power and scale are free. Holding $\gamma$ reduces to Lomax on powered excesses, whose exponential limit becomes a Weibull limit with that power held. Holding both shapes gives a unique scale maximum, even for constant data. Holding $\alpha$ and scale gives a unique power maximum unless every excess equals scale. Holding $\gamma$ and scale gives an explicit unique estimate of $\alpha$.

For the fully free model, choose $\gamma<1$ and let the threshold approach the sample minimum. The $k$ minimum factors diverge as $\varepsilon^{k(\gamma-1)}$ while all other density factors have positive limits. Holding some coefficients can remove this path. For example, $\gamma=1$ with both other coefficients held gives a finite endpoint maximum. The interface nevertheless requires a held threshold for every subset.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md) gives the boundary arguments and their held-parameter conditions.

For tightly clustered excesses with $\alpha$ held and scale and power free, the location/power profile uses centered $\log(1+(x_i-x_0)/(x_0-\mu))$ values, scaled by their range. This retains raw spacings when separate log-excess values would round them away. Strict concavity in power and power times log scale identifies the unique finite root. The likelihood is reevaluated at the represented scale using the same centered differences; restoration of a latent profile root alone does not determine the returned likelihood. A distant held threshold can make the optimal scale unrepresentable at the data’s resolution. A separated likelihood loss after restoring that scale gives a numerical failure. The [concentrated held-shape checks](../../../../../dev/research/evaluations/2026-11-09-audit-beta-data/reference.json) distinguish power accuracy from rounding the restored scale.

## R notes

With `min = 0`, the model is Burr. The concentrated held-first-shape profile retains centered raw-excess spacings. The joint regular fit attempts to certify the finite winner or limiting face; numerical near-ties leave a successful search uncertified. Held power inherits the certified Lomax face comparison; held scale with both shapes free uses a strictly concave scalar certificate. Detected faces stop with a recommendation to use `mlweibull(x - min)` or `mlpareto(x - min)`, as applicable. Held-scale endpoint faces preserve `min + scale`. Unresolved searches stop.

## Decision history

2026-10-31 (golden comparison precision):


The double-only baseline and TASK-132 headers return bit-identical estimates;
the largest relative coefficient difference from the extended-precision golden
is 4.18e-7. The double-only comparison uses 5e-7 (also used for wasm), retaining
the 1e-8 native tolerance. An independent analytic total score in log(shape1),
log(shape2), log(scale) has norm 2.02e-5 and independently summed likelihood is
within 1.1e-12 of the golden. These are numerical stationarity/regression checks,
not a global optimality proof. The appended precision case retains a 1e-3 score
gate and 1e-8 absolute likelihood checks. Reproducer:
[pareto4-precision.cpp](../../../../../dev/validation/pareto4-precision.cpp).
The fitter and golden are unchanged.

## Certified excess default (2026-11-04, TASK-116)

The explicit C++ `pareto4_mle_certified(x, min, tol, maxit)` entrypoint
transports the Burr certificate to excesses. For $y_i=x_i-\mu$ with
$\mu$ held, both shapes and scale are unchanged and the Jacobian is one:
$\ell_x=\ell_y$. Thus every global exclusion, unique finite root and
Weibull/Pareto face certificate transfers unchanged. The two faces are
shifted Weibull and shifted Pareto; subtract `min` to fit those models.
A retained Burr search transports its per-fit certification flag. Remaining
failed or refuted searches preserve the Burr stop statuses and NaN estimates. This entrypoint never fits a free threshold or alters
held-subset algorithms.

The exact-arithmetic guarantee is conditional on computed comparisons,
with the practical floating-point margin and libm assumptions of the
[Burr dossier](../burr/README.md) and
[production certificate report](../../../../../dev/research/evaluations/2026-11-04-burr-port-report.md).
It is not an outward-rounded interval guarantee. The transformed observations
must be representable in binary64. A universal joint uniqueness theorem is
not claimed; an accepted finite outcome has a sample-specific proof.

The public default and R fit now use this certified entrypoint when all three
regular coefficients are free. The Pareto IV golden alone was regenerated with
author authorization; the independent 80-digit root confirms the corrected
coefficients. The [switch report](../../../../../dev/research/evaluations/2026-11-04-pareto4-switch-report.md)
records validation. The [dependency report](../../../../../dev/research/evaluations/2026-11-04-burr-deps-report.md)
retains the historical compatibility conflict and transformation evidence.

The certified transformation rejects arithmetic overflow and any merging of
distinct observations into equal transformed values with `NumericalFailure`.
This prevents rounded transformations from supplying artificial constant-sample
or tied-endpoint certificates. Genuine data ties remain allowed.


The held-scale two-shape reduction now inherits the
[Burr scalar concavity certificate](../burr/README.md#held-scale-certificate-task-1164),
including strict endpoint score comparisons, coefficient accuracy checks,
practical floating-point margins and uncertified fallback. Both joint and
held-scale excess fits transport Burr's per-fit flag with the held threshold.
Subtracting the held threshold transports the certificate to excess data with Jacobian one; artificial ties or overflow in this subtraction stop.

Arithmetic that rounds a distinct observation onto a held scale stops numerically: reciprocation checks equality against the original scale, and threshold subtraction checks its error-free sum residual. This protects the exact endpoint classification and its tied-observation penalty.

### Extreme d/p/q arithmetic

At shape1 one the inverse uses the compensated Pareto III inverse; centering before exponentiation preserves answers close to a cancelling location and scale.
