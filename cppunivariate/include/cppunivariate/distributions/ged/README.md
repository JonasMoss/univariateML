---
id: ged
name: Generalized Error
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mean, role: location, domain: "(-inf, inf)", default: 0.0}
  - {name: sd, role: scale, domain: "(0, inf)", default: 1.0}
  - {name: nu, role: shape, domain: "(0, inf)", default: 3.0}
r:
  density: fGarch::dged
  sampler: fGarch::rged
  control:
    gradtol: Tolerance on the norm of the finite-difference gradient of the total negative log-likelihood in transformed coordinates.
    iterlim: Maximum number of iterations.
log_density: -log(sd) - Abs(x-mean)**nu/(2*(sd*sqrt(2**(-2/nu)*gamma(1/nu)/gamma(3/nu)))**nu) + log(nu) - 3*loggamma(1/nu)/2 + loggamma(3/nu)/2 - log(2)
references:
  - "Nelson, D. B. (1991). Conditional Heteroscedasticity in Asset Returns, A New Approach. Econometrica, 59, 347–370."
  - "Tumlinson, S. E. (2015). On the non-existence of maximum likelihood estimates for the extended exponential power distribution and its generalizations. Statistics & Probability Letters, 107, 111–114. doi:10.1016/j.spl.2015.08.009."
examples: |
  mlged(precip)
---

The generalized error (exponential power) distribution is a symmetric location-scale family whose shape controls tail thickness.

## Details

The likelihood is unbounded as the shape $\nu \to 0$ for every sample (Tumlinson, 2015). Thus `mlged` returns a local maximum near its starting value, rather than a global maximum likelihood estimate. The uniform limit as $\nu \to \infty$ describes local behaviour only and is not detected or reported as an escape. The coefficients `mean` and `sd` follow the fGarch mean and standard deviation parameterisation.
