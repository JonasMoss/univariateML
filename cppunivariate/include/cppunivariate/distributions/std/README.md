---
id: std
name: Student-t
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mean, role: location, domain: "(-inf, inf)", default: 0.0}
  - {name: sd, role: scale, domain: "(0, inf)", default: 1.0}
  - {name: nu, role: df, fixable: true, domain: "(2, inf)", default: 3.0}
r:
  density: fGarch::dstd
  sampler: fGarch::rstd
  control:
    gradtol: Tolerance on the norm of the finite-difference gradient of the total negative log-likelihood in transformed coordinates.
    iterlim: Maximum number of iterations.
log_density: loggamma((nu+1)/2) - loggamma(nu/2) - log((nu-2)*pi)/2 - (nu+1)*log(1+((x-mean)/sd)**2/(nu-2))/2 - log(sd)
limits:
  - name: scale collapse
    fixed: [nu]
    when: "tie multiplicity k > nu*(n-k)"
    status: Unbounded
  - name: tie equality boundary
    fixed: [nu]
    when: "tie multiplicity k = nu*(n-k)"
    status: ScaleBoundary
  - model: 'norm'
    when: '$\nu \to \infty$'
    status: 'NormalBoundary'
  - model: lst
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

With `nu` held fixed, only location and scale are estimated by monotone
EM, stopping when the norm of the scale-standardized location and log-scale
scores is below `gradtol`. Failure to reach that tolerance is an error.
The normal and df-2 escape does not apply to this subset.
Fixed `nu` must exceed 2.
For multiplicity `k` in a sample of size `n`, scale collapse has likelihood
proportional to `scale^(nu*(n-k)-k)`. A negative exponent is reported as
unbounded; equality is reported separately as a scale-collapse boundary,
without claiming unboundedness or certifying a regular MLE (Cauchy equality
can have nonunique attained fits). Strictly positive exponents for every tie
permit the regular fixed-parameter fit.
