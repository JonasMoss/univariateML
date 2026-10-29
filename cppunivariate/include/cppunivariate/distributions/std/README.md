---
id: std
name: Student-t
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mean
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sd
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: nu
  role: df
  domain: (2, inf)
  simulate: 3.0
  kind: regular
r:
  density: dstd
  sampler: rstd
  control:
    nu: Optional held nu; NULL estimates it.
    sd: Optional held sd; NULL estimates it.
    mean: Optional held mean; NULL estimates it.
    gradtol: Tolerance on the norm of the finite-difference gradient of the total negative log-likelihood in transformed coordinates after centering and scaling the sample to unit standard deviation.
    iterlim: Maximum number of iterations.
log_density: loggamma((nu+1)/2) - loggamma(nu/2) - log((nu-2)*pi)/2 - (nu+1)*log(1+((x-mean)/sd)**2/(nu-2))/2 - log(sd)
limits:
  - name: scale collapse
    when: Scale tends to zero at the fitted or held location.
    detect: Excessive mass at that location or vanishing residual spread.
    status: Unbounded
    message: "collapse"
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
  # Daily DAX log returns, scaled to percent.
  mlstd(100 * diff(log(EuStockMarkets[, "DAX"])))
holds: []
---

The Student-t distribution is a symmetric, heavy-tailed alternative to the normal distribution.

## Details

With free degrees of freedom, a tie of multiplicity $k>2(n-k)$ makes the likelihood unbounded; equality is reported as an uncertified scale-collapse boundary rather than as unbounded.

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


Holds use the ordinary t scale sd*sqrt((nu-2)/nu). With mean held, only its tie multiplicity drives scale collapse. With sd held, the nu-to-2 limit forces ordinary scale to zero, rather than giving a finite-scale t2 law. The conditional normal face remains eligible. When sd is free, the conditional ordinary t2 face is compared separately. Profile searches are numerical, not global certificates.
