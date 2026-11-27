---
id: fpareto
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0, shape2 > 0, shape3 > 0 and scale > 0; finite min held; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Feller-Pareto
support: {type: real, lower: "min", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "if (shape2*shape3 < 1) Inf else if (shape2*shape3 > 1) -Inf else log(shape2) - log(scale) - lbeta(shape1, shape3)"}}
params:
- name: shape1
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
- name: shape2
  symbol: '\gamma'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape3
  symbol: '\tau'
  role: shape
  domain: (0, inf)
  simulate: 2
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
  advice: 'The fully free likelihood is unbounded. Omitting min is not supported. Supply a finite threshold to hold it or NULL to estimate it.'
  kind: threshold
r:
  density: dfpareto
  sampler: rfpareto
  control:
    min: Known finite lower endpoint.
    tol: "Requested relative accuracy of the free coefficients (default 1e-10), passed to the GB2 delegate."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
log_density: log(shape2)-log((x-min))-loggamma(shape3)-loggamma(shape1)+loggamma(shape3+shape1)+shape3*shape2*log((x-min)/scale)-(shape3+shape1)*log(1+((x-min)/scale)**shape2)
limits:
- {firmness: proved, model: weibull, when: 'With shape3 held at one, the Burr submodel tends to its Weibull face.', status: WeibullBoundary, detect: 'The delegate closes the signed Lomax comparison at held power or the signed exterior and compact profile comparisons at free power. With inexact excess subtraction the reported comparison is uncertified.', detect_firmness: proved, detect_proof: ["univariateML-cpp/include/cppunivariate/distributions/burr/README.md#details"], fixed: [min, shape3]}
- {firmness: proved, model: invweibull, when: 'With shape1 held at one, the inverse Burr submodel tends to its inverse-Weibull face.', status: InvWeibullBoundary, detect: 'The delegate closes the signed Lomax comparison at held power or the signed exterior and compact profile comparisons at free power. With inexact excess subtraction the reported comparison is uncertified.', detect_firmness: proved, detect_proof: ["univariateML-cpp/include/cppunivariate/distributions/invburr/README.md#details"], fixed: [min, shape1]}
- name: unbounded likelihood
  firmness: proved
  when: With $\alpha$ and $\tau$ positive and fixed, the power $\gamma$ tends to infinity and scale places the concentrating distribution at the common excess.
  detect: The threshold is held and the excesses are constant, with both power and scale free. Other held subsets require their own concentration check.
  status: InvalidData
  message: unbounded
  detect_firmness: proved
  proof: ["univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md#details"]
  detect_proof: ["univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md#details"]
  fixed: [min]
- when: The upper beta shape $\alpha$ tends to infinity with $\gamma$ and $\tau$ fixed and $\sigma\alpha^{-1/\gamma}$ fixed at a positive scale.
  firmness: proved
  detect: With only power held on the excess scale, the powered log span $D$ is at most $0.01$ and the normalized third central moment is negative with magnitude exceeding $69D$ by a floating-point margin. An exact excess transform can also carry the signed beta-prime ordinary gamma comparison at held power and held beta shape. The exploratory comparison uses a numerical likelihood margin. Exploratory selection and inexact transport report uncertified limits.
  status: TrGammaBoundary
  name: shifted trgamma; subtract min before fitting
  detect_firmness: proved
  detect_proof: ["dev/research/evaluations/2026-11-25-fpareto-math-report.md#from-stationary-exclusion-to-a-global-face-bound", "UnivariateML.ShapeFacesFellerPareto.narrow_span_stationary_bound", "univariateML-cpp/include/cppunivariate/distributions/betapr/README.md#non-unit-held-shape-certificate"]
  fixed: [min]
- when: The lower beta shape $\tau$ tends to infinity with $\alpha$ and $\gamma$ fixed and $\sigma\tau^{1/\gamma}$ fixed at a positive scale.
  firmness: proved
  detect: With only power held on the excess scale, the powered log span $D$ is at most $0.01$ and the normalized third central moment is positive with magnitude exceeding $69D$ by a floating-point margin. An exact excess transform can also carry the signed beta-prime inverse gamma comparison at held power and held beta shape. The exploratory comparison uses a numerical likelihood margin. Exploratory selection and inexact transport report uncertified limits.
  status: InvTrGammaBoundary
  name: shifted invtrgamma; subtract min before fitting
  detect_firmness: proved
  detect_proof: ["dev/research/evaluations/2026-11-25-fpareto-math-report.md#from-stationary-exclusion-to-a-global-face-bound", "UnivariateML.ShapeFacesFellerPareto.narrow_span_stationary_bound", "univariateML-cpp/include/cppunivariate/distributions/betapr/README.md#non-unit-held-shape-certificate"]
  fixed: [min]
- when: Both beta shapes equal $K\to\infty$, the power is $\gamma=\sqrt{2\psi_1(K)}/s$, and scale is $\sigma=e^m$, with lognormal log-scale $s>0$ and log-location $m$ fixed. Here $\psi_1$ is the trigamma function.
  firmness: proved
  detect: The held coefficients permit this path, and the numerical comparison selects this limiting model within $10^{-8}n$ log-likelihood units of the best interior candidate found.
  status: LognormalBoundary
  name: shifted lnorm; subtract min before fitting
  detect_firmness: numerical
  fixed: [min]
- when: The power $\gamma$ tends to infinity with $\alpha\gamma$ positive and fixed, while $\tau$ and $\sigma$ stay fixed.
  firmness: proved
  detect: A subunit held beta shape and held scale can give a strict endpoint density bound. Unit-shape submodels can use a Burr signed global or held-scale comparison. Delegate certification requires exact excess subtraction. The exploratory comparison reports an uncertified limit.
  status: ParetoBoundary
  name: shifted pareto; subtract min before fitting
  detect_firmness: proved
  detect_proof: ["univariateML-cpp/include/cppunivariate/distributions/gb2/README.md#details", "univariateML-cpp/include/cppunivariate/distributions/burr/README.md#held-scale-certificate-task-1164"]
  fixed: [min]
- when: The power $\gamma$ tends to infinity with $\tau\gamma$ positive and fixed, while $\alpha$ and $\sigma$ stay fixed.
  firmness: proved
  detect: A subunit held beta shape and held scale can give a strict endpoint density bound. Unit-shape submodels can use a Burr signed global or held-scale comparison. Delegate certification requires exact excess subtraction. The exploratory comparison reports an uncertified limit.
  status: PowerBoundary
  name: shifted power; subtract min before fitting
  detect_firmness: proved
  detect_proof: ["univariateML-cpp/include/cppunivariate/distributions/gb2/README.md#details", "univariateML-cpp/include/cppunivariate/distributions/burr/README.md#held-scale-certificate-task-1164"]
  fixed: [min]
- model: dblpareto
  name: shifted double Pareto; subtract min before fitting
  firmness: proved
  when: The power $\gamma$ tends to infinity with both $\alpha\gamma$ and $\tau\gamma$ positive and fixed, while $\sigma$ stays fixed.
  detect: The held coefficients permit this path, and the numerical comparison selects this limiting model within $10^{-8}n$ log-likelihood units of the best interior candidate found.
  status: DoubleParetoBoundary
  message: GB2 selects the double Pareto limit
  detect_firmness: numerical
  fixed: [min]
- name: critical scale collapse
  status: Unattained
  message: 'finite unattained supremum'
  fixed: [shape1, shape2, shape3]
  when: 'The scale decreases to zero and the threshold gap stays at its density-maximizing ratio to scale.'
  detect: 'With a free threshold and scale, the lower power is at least one and the collapse exponent is exactly zero.'
  firmness: proved
  detect_firmness: proved
  proof: ["dev/research/evaluations/2026-11-25-thresh-math-report.md#fixed-shapes-and-free-scale"]
  detect_proof: ["dev/research/evaluations/2026-11-25-thresh-math-report.md#fixed-shapes-and-free-scale"]
- kind: nonexistence
  name: unbounded free-threshold likelihood
  status: Unbounded
  message: unbounded
  when: 'A free-endpoint spike or an allowed scale collapse increases the likelihood without bound.'
  detect: 'The held mask permits a lower power below one, simultaneous free scale and upper shape, negative collapse exponent, or a constant sample with free upper shape.'
  firmness: proved
  detect_firmness: proved
  proof: ["dev/research/evaluations/2026-11-25-thresh-math-report.md#fixed-shapes-and-free-scale", "dev/research/evaluations/2026-11-25-thresh-math-report.md#structural-spikes", "dev/research/evaluations/2026-11-25-thresh-math-report.md#fixed-scale-and-free-upper-tail-shape"]
  detect_proof: ["dev/research/evaluations/2026-11-25-thresh-math-report.md#fixed-shapes-and-free-scale", "dev/research/evaluations/2026-11-25-thresh-math-report.md#structural-spikes", "dev/research/evaluations/2026-11-25-thresh-math-report.md#fixed-scale-and-free-upper-tail-shape"]
holds:
- held: []
  status: no-mle
  reason: 'With these holds, a free endpoint permits likelihood divergence.'
- held:
  - shape1
  status: no-mle
  reason: 'With these holds, a free endpoint permits likelihood divergence.'
- held:
  - shape2
  status: no-mle
  reason: 'With these holds, a free endpoint permits likelihood divergence.'
- held:
  - shape1
  - shape2
  status: no-mle
  reason: 'With these holds, a free endpoint permits likelihood divergence.'
- held:
  - shape3
  status: no-mle
  reason: 'With these holds, a free endpoint permits likelihood divergence.'
- held:
  - shape1
  - shape3
  status: no-mle
  reason: 'With these holds, a free endpoint permits likelihood divergence.'
- held:
  - shape2
  - shape3
  status: no-mle
  reason: 'With these holds, a free endpoint permits likelihood divergence.'
- held:
  - scale
  status: no-mle
  reason: 'With these holds, a free endpoint permits likelihood divergence.'
- held:
  - shape1
  - scale
  status: no-mle
  reason: 'With these holds, a free endpoint permits likelihood divergence.'
- held:
  - shape2
  - scale
  status: no-mle
  reason: 'With these holds, a free endpoint permits likelihood divergence.'
- held:
  - shape1
  - shape2
  - scale
  status: no-mle
  reason: 'With these holds, a free endpoint permits likelihood divergence.'
- held:
  - shape3
  - scale
  status: no-mle
  reason: 'With these holds, a free endpoint permits likelihood divergence.'
- held:
  - shape1
  - shape3
  - scale
  status: no-mle
  reason: 'With these holds, a free endpoint permits likelihood divergence.'
fit_args:
  min: 0
  shape1: 0.3
  shape3: 0.4
examples: |
  set.seed(1)
  x <- rml(200, mlfpareto())
  mlfpareto(x, min = 0L, shape1 = 0.3, shape3 = 0.4)

group: Pareto and Lomax
construction:
- base
- generalisation
relations:
  transform_of:
  - family: gb2
    transform: shift
    at:
      shape1: shape1
      shape2: shape2
      shape3: shape3
      scale: scale
    shift: min
fit:
  method: delegation
  certified: partial
  note: 'With min held, GB2 certificates transfer to positive excesses, including the non-unit one-held-beta-shape, held-power, free-scale certificate. Finite witnesses retain min; face witnesses remain in excess-data coordinates. Held beta shapes additionally use the power/scale strict-concavity coefficient enclosure. The held-scale one-free-beta-shape, free-power global profile certificates also transfer. Five larger mixed subsets still use numerical outer searches. Free-threshold fits are certified on unique endpoint and interior routes when their coefficient brackets close.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model insurance loss severity with independently adjustable tails above a supplied
  threshold.
summary: Feller-Pareto provides a model for insurance loss severity with independently adjustable tails above a
  supplied threshold.
aliases: []
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
- 'Dutang, C., Goulet, V. and Langevin, N. (2022). Feller-Pareto and Related Distributions: Numerical Implementation
  and Actuarial Applications. Journal of Statistical Software, 103(6), 1–22. doi:10.18637/jss.v103.i06.'
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, every nonempty sample has unbounded likelihood.
    Holding $\mu$ gives the generalized beta of the second kind (GB2) on
    excesses. Its joint likelihood can escape to several proper limit models;
    a complete joint existence criterion and joint uniqueness are not established.
    Holding both beta shapes $\alpha$ and $\tau$ gives a unique finite
    power/scale fit for nonconstant excesses, while constant data remain unbounded
    if both power and scale are free. With power and scale held, the free
    beta shapes have a unique finite fit for nonconstant excesses, but both
    free beta shapes can concentrate on constant data. Further holds can
    remove these concentration paths. With a free threshold and the lower shape and power held, the collapse exponent or held-scale profile decides the remaining cases.
    Holding all coefficients leaves likelihood evaluation.
    With power held and both beta shapes and scale free, a sufficiently
    narrow and skewed powered log sample has no finite maximum.
    Positive skew selects the inverse transformed-gamma limit, and
    negative skew selects the transformed-gamma limit.
    A locally checked proposal from the general GB2 search is uncertified
    unless the conditional beta enclosure applies.
  reason: >-
    With $\gamma\tau<1$, moving a free threshold to the sample minimum makes
    the density factors there diverge as $(x-\mu)^{\gamma\tau-1}$.
    With the threshold held, put $y_i=x_i-\mu$. These excesses have a GB2
    likelihood. For held beta
    shapes, the log-excess likelihood in $(\gamma,\gamma\log\sigma)$ is
    strictly concave, with coercive profiles for nonconstant data. For held
    power and scale, transforming to $T_i=(y_i/\sigma)^\gamma/
    \{1+(y_i/\sigma)^\gamma\}$ gives a beta likelihood. Its log normalizer
    has a positive definite Hessian, and nonconstant interior observations
    place the mean sufficient statistic inside its convex support.
    These conditional results do not settle the outer joint search.
  equivariance:
    group: power-scale
    map: {shape2: "shape2/B", scale: "A*scale**B"}
    held: {min: 0}
    evidence: >-
      The density depends on $x$ (with min held at 0) only through
      $(x/\mathrm{scale})^{\mathrm{shape2}}$ and the factor $1/x$.
      dev/validation/equivariance.py checks the density identity at 24 seeded
      points on the dossier density (40 digits, largest relative difference
      below $10^{-39}$) and dpqr.hpp (binary64); SymPy did not simplify it
      within its guard.
---

The Feller–Pareto distribution models sizes above a threshold $\mu$ with independently adjustable lower and upper tails. The scale $\sigma$ sets the size unit, the power $\gamma$ controls the transformation of excesses, and the beta shapes $\tau$ and $\alpha$ give lower and upper tail powers $\gamma\tau$ and $\gamma\alpha$.

## Details

With threshold free and the lower power held, the scale-zero collapse exponent
depends on the multiplicity of the sample minimum. With scale held and the upper
shape free, an interior fit above lower power one can have competing local
maxima. The bounded profile search reports a stationary candidate as uncertified.
The endpoint fit at lower power one has a unique maximum.
[Threshold analysis](../../../../../dev/research/evaluations/2026-11-25-thresh-math-report.md).

Fitting requires finite observations strictly above a held threshold. Subtracting the threshold gives [GB2](../gb2/README.md). In the usual GB2 coordinates $(a,b,p,q)$, the mapping is $(\gamma,\sigma,\tau,\alpha)$. Identification and conditional existence results transfer through this shift.

Outside the certificates below, the fit profiles power and log scale for each pair of beta shapes. The inner problem is strictly concave; the outer search explores and refines several numerical basins. It compares transformed gamma, inverse transformed gamma, lognormal, Pareto, power and double-Pareto faces as allowed by the held coefficients. Neither global optimality nor exhaustion of all possible faces is proved. A small score and an interior location are required for a returned interior candidate; unresolved searches stop.

Holding both beta shapes gives a unique power/scale maximum on nonconstant excesses. Constant excesses allow power to grow and scale to concentrate at the common value. Holding power too gives a unique scale maximum even for constant data. With power and scale held, both beta shapes have a finite unique maximum for nonconstant excesses. If only one beta shape remains free, its strictly decreasing score also permits constant excesses.

With a free threshold, choose $\gamma\tau<1$. As the threshold approaches a minimum tied $k$ times, the minimum factors diverge as $\varepsilon^{k(\gamma\tau-1)}$ and the remaining factors have positive limits. This proves unboundedness for the fully free likelihood. It does not prove non-existence for every held-coefficient submodel. For example, $\gamma=\tau=1$ with the other coefficients held reduces to the monotone Pareto-II endpoint problem. The interface still requires a held threshold.

When scale is held, a selected endpoint face retains $\mu+\sigma$ as its Pareto endpoint or power upper endpoint. Refitting that endpoint freely changes the model. Some held-scale searches remain unresolved, including deterministic Pareto and power samples, and stop without returning a fitted model.

The [family analysis](../../../../../dev/research/evaluations/2026-10-28-gb2.md) gives the limiting paths and boundary comparisons.

## R notes

With `min = 0`, estimates agree with `mlgb2(x)` for the same holds.
A stalled held-power fit on concentrated excesses can retain the
conditional two-shape solution at a trial scale as a finite likelihood
witness while the joint search remains unresolved. Faces are compared within $10^{-8}n$ log-likelihood units; this numerical tolerance can reject a tied or slightly better finite fit. Errors name the shifted limiting fit where available. All-held fits evaluate the supplied density.

## Decision history

The 2026-10-31 GB2 member-seed subsampling for samples larger than 1000
also applies through this delegate when all four GB2 coefficients are free.
The held-threshold shift, validation and full-data final checks are unchanged;
see [GB2](../gb2/README.md) and the
[paired evidence](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).


The held-scale two-shape reduction now inherits the
[Burr scalar concavity certificate](../burr/README.md#held-scale-certificate-task-1164),
including strict endpoint score comparisons, coefficient accuracy checks,
practical floating-point margins and explicit unresolved comparisons, budget exhaustion and numerical failures.
These unit-shape reductions do not cover every subset; the held-power route below adds the two non-unit conditional shape/scale subsets.

Arithmetic that rounds a distinct observation onto a held scale stops numerically: reciprocation checks equality against the original scale, and threshold subtraction checks its error-free sum residual. This protects the exact endpoint classification and its tied-observation penalty.

## Held-power certificate transport

With threshold held, the [GB2 held-power certificate](../gb2/README.md)
applies to one free beta shape and free scale on positive excesses. Translation
has unit Jacobian. Finite family witnesses append held `min`; transformed-gamma
and inverse transformed-gamma witnesses retain three
coordinates in excess-data units. Limit advice must retain held power and
held shape. Nonfinite subtraction or subtraction that merges distinct
observations stops numerically before invoking the parent. The existing
held-scale endpoint subtraction guard is preserved.

The remaining open mixed GB2 regimes stay open after translation.
Nonuniqueness of the conditional shape/scale
profile transfers through the same fixed translation.

The held-beta-shapes [GB2 coefficient enclosure](../gb2/README.md)
also transfers with unit translation Jacobian. Finite witnesses append held
`min`, including numerical-stop witnesses. Nonfinite or collapsing excess subtraction
stops numerically before this certificate is called.

The GB2 one-sided endpoint subunit-shape density domination theorem transfers
on excesses, with the face endpoint held at `min + scale`. Its power
witness has `alpha = scale` in excess-data coordinates; its Pareto
witness has `b = scale`. Refitting the endpoint changes the held model.

Held-endpoint ties retain the GB2 pointwise likelihood penalty in excess coordinates; the weak limiting power/Pareto law has the usual endpoint density when refitted.

The [held-scale mixed-power global certificate](../gb2/README.md)
transfers on positive excesses, including finite constant-excess maxima,
upper-side concentration, endpoint ties and above-one held-shape endpoint
domination. Finite witnesses append held `min`; face witnesses retain their
excess-data coordinates.

### Concentrating probability coordinates

At large power, forming x-min first can erase the displacement from min+scale. Evaluation retains that center and its addition residual before forming the log ratio. The density groups the two beta-tail penalties separately, avoiding cancellation between the power term and their shared softplus. Inversion transports the log displacement through expm1 before adding the location; otherwise min=-scale can lose the entire quantile.

The concentrating symmetric tail uses t=shape2*log((x-min)/scale), with a=shape1=shape3. Rounding its two beta coordinates can erase a resolved displacement. This probability bridge returns NaN for sqrt(a)*E > 2.5e-11 and 0 < |t| < 2E/1e-10, where E is eight binary64 machine epsilons. At t=0 the exact symmetric half-probability remains available; the stated margin is practical floating-point evidence.

The shape1 one/two reductions use the closed beta-coordinate tails rather than a promoted backend normalizer. At shape1 two, the positive-deviance hazard described in [the GB2 dossier](../gb2/README.md#probability-coordinates-at-large-power) is applied after the translated power coordinate; both translation and the tail calculation must retain their low-order terms.

### Unequal concentrating beta shapes

The density normalizer is centered at the beta mean and combined with two nonnegative Bernoulli deviances. In the unit-power submodel, a compensated product expansion retains shape2 times the physical excess minus shape1 times scale before either ratio rounds; a rounded beta coordinate can lose the entire standardized displacement. For other powers, the log-odds kernel returns NaN near concentration when its practical coordinate-error margin exceeds 1e-12 times the log-density scale. This named precision region requires more accurate transcendental coordinates to close.

If the ordinary log-odds margin fails, two-word logarithms recompute the centered coordinate before rounding. The beta mean is subtracted in the same arithmetic. NaN remains confined to cases where this practical two-word margin also fails; the ordinary margin selects the recomputation rather than a refusal.

Subtracting a held minimum can round even when the original observations are
represented exactly. Uncertified GB2 transport checks the TwoSum residual of
each subtraction. A nonzero residual is carried into the original-excess log
ratios before the information check; exact subtractions retain the parent fit.

Extreme excess-data units can magnify rounding differences in GB2's nearly
singular double-Pareto power/scale profiles even for a small sample. These
searches retain a finite excess-data witness with the held minimum appended;
the witness likelihood is in the original observation units.

The double-Pareto face is asymmetric Laplace in $z=\log(x-\min)$.
For a fixed log-kink $c$, write $L=\sum(c-z_i)_+$ and
$R=\sum(z_i-c)_+$. Its rates maximize the concave rate likelihood at
$n/[\sqrt L(\sqrt L+\sqrt R)]$ and
$n/[\sqrt R(\sqrt L+\sqrt R)]$. The free-kink profile is compared at the
observed log excesses; its one-sided endpoints are the power and Pareto faces.
The returned double-Pareto payload retains the physical kink, both rates and
the held minimum. Selection by the finite search remains uncertified.

A selected lognormal face can be refuted by feasible beta-log-odds members.
For power $a\downarrow0$, kink $b$, $r=\exp[a(m-\log b)]$ and target log
variance $v$, the beta shapes $(1+r)/(a^2v)$ and
$(1+1/r)/(a^2v)$ approach log mean $m$ and variance $v$.
A strictly higher evaluated density sum supplies a feasible lower bound and
rejects that face; these comparison points are not returned as estimates.
The comparison uses a practical $4096\epsilon(n+|\ell|+|\ell_{\rm face}|)$
margin. Failure to separate values leaves the selected face uncertified.

Even with both beta shapes held, constant data have unbounded likelihood
when power and scale are free: choose scale equal to the common excess and
let power increase. The density there is power times a fixed positive
beta-log-odds density divided by the common excess. With both beta shapes
free, concentration also occurs at fixed positive power and scale by
increasing the shapes together at their prescribed log-odds ratio.
