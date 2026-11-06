---
id: ged
inference_numeric:
  - regime: "cusp moving with smooth exponent"
    available: true
    condition: "finite mean, sd > 0 and nu > 2; any regular subset free"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "held location"
    available: true
    condition: "finite mean held and sd > 0, nu > 0; if nu is free require nu > 1"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "normal member with shape held"
    available: true
    condition: "finite mean and sd > 0, nu = 2 held; mean and/or sd free"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "moving location with smooth or held shape"
    available: false
    condition: "finite mean free, sd > 0; either nu > 2, or nu > 1 is held; sd may be held or fitted; excluding the preceding available submodels"
    reason: "DQM and full-rank information are established, but support/join-aware numerical derivatives and fitted covariance validation for this expanded scope are not yet implemented"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "joint fitted cusp Hessian open"
    available: false
    condition: "finite mean free, sd > 0 and 1 < nu <= 2 with nu free"
    reason: "the model is DQM with full-rank information and an integrable correct a.e. Hessian at the truth; consistency of the joint fitted Hessian and cusp-crossing numerical derivatives remains unvalidated"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "regular location with unsuitable ordinary Hessian"
    available: false
    condition: "finite mean free, sd > 0 and 1/2 < nu <= 1 held"
    reason: "the model is DQM with full rank, but the ordinary location Hessian omits the Laplace kink at nu = 1 and is nonintegrable below 1; below 1 the location MLE also makes raw fitted OPG singular"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "critical or fractional cusp location"
    available: false
    condition: "finite mean free, sd > 0 and 0 < nu <= 1/2 held"
    reason: "location uses sqrt(n log n) at nu = 1/2 or an fBm likelihood at rate n^(1/(1+2*nu)) below it; ordinary Hessian inference is invalid and raw critical OPG has the wrong information constant"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "active fitted Laplace shape bound"
    available: false
    condition: "nu = 1 free, finite mean held or fitted, sd > 0"
    reason: "the regular underlying model has a constrained Gaussian shape tangent; moving-location ordinary Hessian inference additionally misses the cusp contribution"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
name: Generalized Error
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mean
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sd
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: nu
  symbol: '\nu'
  role: shape
  domain: '(0, inf)'
  simulate: 2
  fit_domain:
    lower: 1.0
    restriction:
      mechanism: spike
      exempt_when_held: []
      reason: Estimating nu is restricted to nu >= 1 to exclude the small-shape spike.
        Holding sd alone does not exclude that mechanism.
  kind: regular
r:
  density: dged
  sampler: rged
  control:
    nu: Optional held nu; NULL estimates it.
    sd: Optional held sd; NULL estimates it.
    mean: Optional held mean; NULL estimates it.
    gradtol: Tolerance on the norm of the finite-difference gradient of the total negative log-likelihood in transformed coordinates.
    iterlim: Maximum number of iterations.
log_density: -log(sd) - Abs(x-mean)**nu/(2*(sd*sqrt(2**(-2/nu)*gamma(1/nu)/gamma(3/nu)))**nu) + log(nu) - 3*loggamma(1/nu)/2 + loggamma(3/nu)/2 - log(2)
limits:
  - name: scale collapse
    firmness: proved
    when: The free scale tends to zero at the fitted or held location.
    detect: Every observation equals the fitted or held location.
    status: InvalidStart
    message: "unbounded"
  - model: unif
    firmness: proved
    when: 'The shape $\nu$ tends to infinity, retaining finite location and scale.'
    detect: The numerically compared uniform log likelihood exceeds all retained finite candidates; this comparison does not certify the global supremum.
    status: UniformBoundary
    numerical: true
references:
- Nelson, D. B. (1991). Conditional Heteroscedasticity in Asset Returns, A New Approach. Econometrica, 59, 347–370.
- Tumlinson, S. E. (2015). On the non-existence of maximum likelihood estimates for the extended exponential power
  distribution and its generalizations. Statistics & Probability Letters, 107, 111–114. doi:10.1016/j.spl.2015.08.009.
- 'fGarch documentation (accessed 2026-10-31). fGarch: Rmetrics - Autoregressive Conditional Heteroskedastic Modelling. R package reference
  manual. https://cran.r-project.org/web/packages/fGarch/fGarch.pdf.'
holds: []
examples: |
  x <- diff(log(datasets::EuStockMarkets[, "DAX"]))
  mlged(x)

group: skewed and robust location-scale
construction:
- location-scale
- generalisation
relations:
  special_case_of:
  - family: sged
    at:
      mean: mean
      sd: sd
      nu: nu
      xi: 1
fit:
  method: multistart
  certified: partial
  note: 'ged/README.md, Details; ged/mle.hpp (public fitter and held overloads); ged/test.cpp: Held shape at least
    one gives a convex location problem and analytic scale profile; free shape search is numerical, including uniform-face
    comparisons.'
areas:
- robust modelling
classification: specialist
classification_reason: Used to model symmetric financial errors with exponential-power rather than Gaussian tails.
summary: Generalized Error provides a model for symmetric financial errors with exponential-power rather than Gaussian
  tails.
aliases:
- "exponential power"
- "symmetric generalized normal"
- "Subbotin"
- "generalized Gaussian"
key_references:
- 3
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, fitting restricts $\nu\ge1$; constant data have unbounded scale likelihood, and nonconstant data may select the unattained uniform limit. Without the fitting restriction, the all-free likelihood is unbounded for every sample. Global uniqueness with free shape in the restricted domain is unknown. For held $\nu>1$, nonconstant data have a unique location-scale maximum; at $\nu=1$, median intervals can give nonunique locations. With shape held, holding $\mu$ gives a unique scale maximum unless all residuals vanish. Holding $\sigma$ removes scale collapse; with shape held above one its location maximum is unique, while shape one permits median intervals. Free shape retains its fitting restriction and conditional uniform comparison. Held positive shapes below one also admit a location maximum at an observation and an explicit scale profile when residual spread is positive, but locations can tie. Holding all coefficients leaves likelihood evaluation.
  reason: >-
    At fixed shape, location minimizes $\sum_i|x_i-\mu|^\nu$, strictly convex for $\nu>1$ and minimized by medians for $\nu=1$. The free scale is an explicit positive multiple of the residual power mean. For nonconstant data this yields a unique location and scale when $\nu>1$. It does not prove uniqueness in shape.
---

The generalized error (exponential power) distribution is a symmetric location-scale family whose shape controls tail thickness.

## Details

The coefficients $\mu$ and $\sigma$ are the mean and standard deviation. Shape $\nu=1$ gives Laplace tails and $\nu=2$ gives the normal distribution. Larger shape gives lighter tails and approaches a uniform distribution. Holding a coefficient preserves its value while the remaining coefficients are estimated.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) describes the limiting paths and the scope of the boundary comparisons.

Fitting restricts the shape to $\nu \ge 1$, the log-concave GED range,
including the attained Laplace member at $\nu=1$. Without this restriction
the likelihood is unbounded as $\nu \to 0$ for nonconstant samples
(Tumlinson, 2015). Estimated shapes below one are excluded; held positive shapes below one are accepted in conditional fits.
Density, probability, quantile and random generation functions
retain the full $\nu>0$ domain. Coefficients follow fGarch's mean and
standard deviation parameterisation.

The fitter compares several interior starts with the exact Laplace fit
and the uniform limit. Total log-likelihood differences of at most
$10^{-8}n$ are ties, with an attained member preferred to a limit.
A winning uniform limit is reported rather than returned as a finite shape estimate.

With location and scale free, at the Laplace face $\mu$ is a sample median and $\sigma$ is $\sqrt{2}$ times
the mean absolute deviation from that median.


For each held or profiled shape, location minimizes the sum of absolute residuals to that power (a median at shape one, or an observation for held shape below one); the free scale has an exact power-mean profile. A held location collapses only when all residuals vanish. Held standard deviation removes scale collapse. Free shape retains the existing restriction $\nu\ge1$ and is compared with the conditional uniform face. With both location and standard deviation held, observations at a uniform endpoint contribute their limiting GED penalty. Shape search is numerical.

For fixed $\nu\ge1$, the standardized log-density is a constant minus a
positive multiple of $|z|^\nu$, hence is concave. Consequently the
location-scale log-likelihood is concave in $(u,b)=(\mu/\sigma,1/\sigma)$:
it is $n\log b$ plus concave terms evaluated at $bx_i-u$. At $\nu=1$
the kernel has a kink and a median need not be unique; as $\nu\to\infty$
the uniform is a separate limiting face. For $0<\nu<1$ the kernel is
not log-concave. The default free fit currently uses multistart optimization,
not a certified concave inner solve; the conditional power-mean profile
and the outer numerical search do not certify the default free fit.

| TASK-116 property | Evidence |
|---|---|
| Fixed $\nu\ge1$ | Concavity in inverse-scale coordinates, proved as above. |
| Conditional location and scale | Median or numerical monotone-score root and analytic scale profile. |
| Default free shape and uniform face | Multistart and face comparison; numerical, with no global shape certificate. |

With standard deviation held and a limiting uniform endpoint forced onto an observation, each such observation has limiting log-density penalty $e^{-\gamma}$, where $\gamma$ is Euler's constant. If mean is free and both endpoints are forced, with endpoint counts k and l the total penalty is $2e^{-\gamma}\sqrt{kl}$ after profiling the location shift of order $1/\nu$.

## R notes

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
optimizers return a failure status unless the score norm in fitting
coordinates is below `1e-3`; unresolved scalar searches stop.

The fGarch mean and standard deviation convention is used. Its `gedFit` permits positive shapes below one and returns local solutions. A winning uniform limit here stops with advice to use `mlunif`.

Face comparisons retain the family tolerance but stop with the existing numerical-failure status when `core/face_compare.hpp` cannot resolve the decision after its floating-point rounding allowance. This allowance concerns computed likelihood comparisons, not optimizer convergence or global optimality; see the [comparison analysis](../../../../../dev/research/evaluations/2026-11-02-face-compare-report.md).
