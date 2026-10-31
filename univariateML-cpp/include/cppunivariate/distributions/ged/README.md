---
id: ged
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
      reason: nu >= 1 selects the Laplace member; spikes persist with sd held (P0
        locscale).
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
    when: Scale tends to zero at the fitted or held location.
    detect: Every observation equals the fitted or held location.
    status: InvalidStart
    message: "unbounded"
  - model: unif
    when: '$\nu \to \infty$'
    detect: Uniform log-likelihood exceeds all finite candidates.
    status: UniformBoundary
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
- Exponential power
key_references:
- 3
---

The generalized error (exponential power) distribution is a symmetric location-scale family whose shape controls tail thickness.

## Details

Fitting restricts the shape to $\nu \ge 1$, the log-concave GED range,
including the attained Laplace member at $\nu=1$. Without this restriction
the likelihood is unbounded as $\nu \to 0$ for nonconstant samples
(Tumlinson, 2015). Very sharp-peaked shapes below one are not fitted.
fGarch's own `gedFit` allows positive shapes below one and returns local
solutions. Density, probability, quantile and random generation functions
retain the full $\nu>0$ domain. Coefficients follow fGarch's mean and
standard deviation parameterisation.

The fitter compares several interior starts with the exact Laplace fit
and the uniform limit. Total log-likelihood differences of at most
$10^{-8}n$ are ties, with an attained member preferred to a limit.
A winning uniform limit is reported with a recommendation to use `mlunif`.

With location and scale free, at the Laplace face mean is a sample median and sd is $\sqrt{2}$ times
the mean absolute deviation from that median.


For each held or profiled nu, the location minimizes the sum of absolute residuals to that power (a median at nu=1); the free scale has an exact power-mean profile. A held location collapses only when all residuals vanish. Held sd removes scale collapse. Free nu retains the existing restriction nu>=1 and is compared with the conditional uniform face. With both location and sd held, observations at a uniform endpoint contribute their limiting GED penalty. Shape search is numerical.

With sd held and a limiting uniform endpoint forced onto an observation, each such observation has limiting log-density penalty $e^{-\gamma}$, where $\gamma$ is Euler's constant. If mean is free and both endpoints are forced, with endpoint counts k and l the total penalty is $2e^{-\gamma}\sqrt{kl}$ after profiling the location shift of order $1/\nu$.

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
optimizers return a failure status unless the score norm in fitting
coordinates is below `1e-3`; unresolved scalar searches stop.

## d/p/q/r

Uses the fGarch mean and standard deviation parameterisation.
