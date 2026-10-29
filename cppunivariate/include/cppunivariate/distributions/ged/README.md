---
id: ged
name: Generalized Error
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mean, role: location, domain: "(-inf, inf)", default: 0.0}
  - {name: sd, role: scale, domain: "(0, inf)", default: 1.0}
  - {name: nu, role: shape, domain: "[1, inf)", default: 3.0}
r:
  density: dged
  sampler: rged
  control:
    gradtol: Tolerance on the norm of the finite-difference gradient of the total negative log-likelihood in transformed coordinates.
    iterlim: Maximum number of iterations.
log_density: -log(sd) - Abs(x-mean)**nu/(2*(sd*sqrt(2**(-2/nu)*gamma(1/nu)/gamma(3/nu)))**nu) + log(nu) - 3*loggamma(1/nu)/2 + loggamma(3/nu)/2 - log(2)
limits:
  - model: unif
    when: '$\nu \to \infty$'
    detect: Uniform log-likelihood exceeds all finite candidates.
    status: UniformBoundary
references:
  - "Nelson, D. B. (1991). Conditional Heteroscedasticity in Asset Returns, A New Approach. Econometrica, 59, 347–370."
  - "Tumlinson, S. E. (2015). On the non-existence of maximum likelihood estimates for the extended exponential power distribution and its generalizations. Statistics & Probability Letters, 107, 111–114. doi:10.1016/j.spl.2015.08.009."
examples: |
  mlged(precip)
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

At the Laplace face, mean is a sample median and sd is $\sqrt{2}$ times
the mean absolute deviation from that median.
