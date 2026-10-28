---
id: std
name: Student-t
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mean, role: location, domain: "(-inf, inf)", default: 0.0}
  - {name: sd, role: scale, domain: "(0, inf)", default: 1.0}
  - {name: nu, role: df, domain: "(2, inf)", default: 3.0}
r:
  density: fGarch::dstd
  sampler: fGarch::rstd
  control:
    gradtol: Tolerance on the norm of the finite-difference gradient of the total negative log-likelihood in transformed coordinates.
    iterlim: Maximum number of iterations.
log_density: loggamma((nu+1)/2) - loggamma(nu/2) - log((nu-2)*pi)/2 - (nu+1)*log(1+((x-mean)/sd)**2/(nu-2))/2 - log(sd)
limits:
  - model: 'norm'
    when: '$\nu \to \infty$'
    status: 'NormalBoundary'
  - name: 'location-scale t with 2 degrees of freedom'
    when: '$\nu \to 2^+$'
    status: 'DfTwoBoundary'
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995). Continuous Univariate Distributions, Volume 1, Chapter 13. Wiley, New York."
examples: |
  mlstd(precip)
---

The Student-t distribution is a symmetric, heavy-tailed alternative to the normal distribution.

## Details

This is the fGarch parameterisation: `mean` and `sd` are the mean and standard deviation, and `nu` is greater than 2. A location-scale t distribution with 2 degrees of freedom has infinite variance and cannot be represented by finite coefficients in this parameterisation.
