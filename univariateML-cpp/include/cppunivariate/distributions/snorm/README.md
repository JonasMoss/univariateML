---
id: snorm
inference_numeric:
  - regime: "symmetric member with skew held"
    available: true
    condition: "finite mean and sd > 0; xi = 1 held; mean and/or sd free"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#two-piece-normal-snorm"
  - regime: "regular two-piece normal interior"
    available: false
    condition: "finite mean, sd > 0 and finite xi > 0; any nonempty free subset, including xi = 1 with xi free; observed information uses the a.e. piecewise Hessian with defined join handling; excluding the preceding available submodels"
    reason: "DQM and full-rank information are established, but support/join-aware numerical derivatives and fitted covariance validation for this expanded scope are not yet implemented"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#two-piece-normal-snorm"
name: Skew Normal
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
- name: xi
  symbol: '\xi'
  role: skew
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dsnorm
  sampler: rsnorm
  control:
    mean: "Optional held mean; NULL estimates it where the hold subset is supported."
    sd: "Optional held sd; NULL estimates it where the hold subset is supported."
    xi: "Optional held xi; NULL estimates it where the hold subset is supported."
log_density: log(sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)) - log(sd) + log(2/(xi+1/xi)) - log(2*pi)/2 - Piecewise(((sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)*(x-mean)/sd + sqrt(2/pi)*(xi-1/xi))**2/xi**2, (sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)*(x-mean)/sd + sqrt(2/pi)*(xi-1/xi))>0), ((sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)*(x-mean)/sd + sqrt(2/pi)*(xi-1/xi))**2*xi**2, True))/2
limits:
  - firmness: proved
    name: scale collapse
    when: 'The free standard deviation $\sigma$ tends to zero at the fitted or held mean.'
    detect: 'Every observation equals the fitted or held mean and scale is free.'
    status: Unbounded
    message: "Scale collapse"
    kind: nonexistence
  - model: hnorm
    firmness: proved
    when: 'As $\xi\to\infty$, the distribution tends to a translated right half-normal with the limiting mean and standard deviation.'
    status: 'RightHalfNormalBoundary'
    numerical: true
  - name: 'reflected translated half-normal'
    firmness: proved
    when: 'As $\xi\to0$, the distribution tends to a reflected translated half-normal with the limiting mean and standard deviation.'
    status: 'LeftHalfNormalBoundary'
    numerical: true
references:
- Fernandez, C. and Steel, M. F. J. (1998). On Bayesian modeling of fat tails and skewness. Journal of the American
  Statistical Association, 93(441), 359–371.
- 'fGarch documentation (accessed 2026-10-31). fGarch: Rmetrics - Autoregressive Conditional Heteroskedastic Modelling. R package reference
  manual. https://cran.r-project.org/web/packages/fGarch/fGarch.pdf.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlsnorm())
  mlsnorm(x)

group: skewed and robust location-scale
construction:
- skewing
- location-scale
relations:
  special_case_of:
  - family: sged
    at:
      mean: mean
      sd: sd
      nu: 2
      xi: xi
fit:
  method: profile-search
  certified: partial
  note: 'snorm/README.md, Details; snorm/mle.hpp (public fitter and held overloads); snorm/test.cpp: Held shape
    gives a concave location/inverse-scale problem; free skewness uses finite scans and local refinement, not a
    global certificate.'
areas:
- robust modelling
classification: specialist
classification_reason: Used to model two-piece Gaussian errors with unequal scales on the two sides.
summary: Skew Normal provides a model for two-piece Gaussian errors with unequal scales on the two sides.
aliases:
- "two-piece normal"
- "split normal"
- "Fernández–Steel skew-normal"
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, nonconstant samples have a finite MLE exactly
    when the two-piece profile described below attains its global minimum at
    an interior mode. Otherwise the supremum is a translated half-normal
    limit. Constant samples have unbounded likelihood. Joint uniqueness is
    not established. Holding $\xi=1$ gives the usual unique normal MLE
    whenever scale does not collapse. For other held subsets, half-normal
    limits must respect the held moments, and no general existence or
    uniqueness classification is asserted here.
  reason: >-
    For a proposed mode $c$, let $A(c)=\sum_{x_i<c}(x_i-c)^2$ and
    $B(c)=\sum_{x_i\ge c}(x_i-c)^2$. Profiling the two side scales
    makes maximization equivalent to minimizing
    $Q(c)=(A(c)^{1/3}+B(c)^{1/3})^3$ on the interval between the sample
    extremes. An interior minimizer gives positive side scales. Endpoint
    minimizers give half-normal limits. This compact profile establishes
    attainment in the closure, but does not establish uniqueness.
---

The Fernández–Steel skew-normal distribution models skewness through different Gaussian scales on the two sides of a mode.

## Details

Fully free constant finite samples, including a singleton, report unbounded likelihood before profile construction: the normal member with mean at the observation has density proportional to $1/\sigma$. Empty or nonfinite samples remain invalid.

The coefficients $\mu$ and $\sigma$ are the distribution's mean and standard deviation. The positive coefficient $\xi$ controls the side-scale ratio, and $\xi=1$ gives a normal distribution. The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-skewreal.md) describes the limiting paths and boundary comparisons.

With all coefficients free, the fitter profiles the two side scales analytically and searches the mode between the sample extremes. Between consecutive observations the stationary candidates satisfy a polynomial equation of degree at most seven. Comparing these candidates and the endpoints gives a global comparison in exact arithmetic, conditional on the computed root comparisons. It does not imply that a finite maximizer is unique. The right and left half-normal limits have their endpoints at the sample minimum and maximum respectively.

Any subset of the three coefficients can be held. With skewness held,
location and inverse scale have a concave likelihood. With skewness free,
both half-normal faces are compared under the held moment constraints.
The conditional search uses nested one-dimensional minimizations and scans
log skewness; this numerical search is not a global certificate. Constant
data at a held mean have unbounded likelihood when scale is free.

With both mean and standard deviation held, an observation at the limiting half-normal
endpoint retains a finite residual on the shrinking side. Each such
observation contributes the penalty $m^2/(8h^4)$ to the limiting log
likelihood, where $m=\sqrt{2/\pi}$ and $h=\sqrt{1-2/\pi}$.
When either moment is free its vanishing adjustment removes this penalty.

A conditional search that reaches a finite numerical tail guard without a
winning analytic face stops with a convergence reason. A guard value is
never returned as an estimated coefficient.


## R notes

This is the two-piece normal family. `mlsn` fits the Azzalini skew-normal family. Any subset of `mean`, `sd`, and `xi` can be held. Evaluation uses the fGarch mean and standard deviation parameterization.

Use `mlhnorm(x, mu = NULL)` for the right half-normal boundary. For the left boundary, fit `mlhnorm(-x, mu = NULL)` and reflect the resulting distribution back to the original axis.

Face comparisons retain the family tolerance but stop with the existing numerical-failure status when `core/face_compare.hpp` cannot resolve the decision after its floating-point rounding allowance. This allowance concerns computed likelihood comparisons, not optimizer convergence or global optimality; see the [comparison analysis](../../../../../dev/research/evaluations/2026-11-02-face-compare-report.md).
