---
id: sstd
name: Skew Student-t
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
- name: xi
  role: skew
  domain: (0, inf)
  simulate: 2.0
  kind: regular
r:
  export: true
  density: dsstd
  sampler: rsstd
  control:
    mean: "Optional held mean; NULL estimates it where the hold subset is supported."
    sd: "Optional held sd; NULL estimates it where the hold subset is supported."
    nu: "Optional held nu; NULL estimates it where the hold subset is supported."
    xi: "Optional held xi; NULL estimates it where the hold subset is supported."
    gradtol: "Tolerance on the Euclidean norm of the total negative log-likelihood gradient in transformed coordinates after centering and scaling the sample to unit standard deviation; fixed step and objective-decrease criteria can also stop the fit."
    iterlim: "Maximum number of optimisation iterations."
log_density: "log(sqrt((1 - (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2)*(xi**2 + xi**(-2)) + 2*(gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2 - 1)) - log(sd) + log(2/(xi + 1/xi)) + loggamma((nu + 1)/2) - loggamma(nu/2) - log((nu - 2)*pi)/2 - (nu + 1)*log(1 + (Piecewise(((sqrt((1 - (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2)*(xi**2 + xi**(-2)) + 2*(gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2 - 1)*(x - mean)/sd + (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))*(xi - 1/xi))/xi, (sqrt((1 - (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2)*(xi**2 + xi**(-2)) + 2*(gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2 - 1)*(x - mean)/sd + (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))*(xi - 1/xi)) > 0), ((sqrt((1 - (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2)*(xi**2 + xi**(-2)) + 2*(gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))**2 - 1)*(x - mean)/sd + (gamma((nu - 1)/2)/gamma(nu/2)*sqrt((nu - 2)/pi))*(xi - 1/xi))*xi, True)))**2/(nu - 2))/2"
limits:
  - {name: scale collapse, fixed: [mean, nu, xi], when: "sd tends to zero", status: Unbounded, message: "Likelihood is unbounded at a tie"}
  - {name: scale-collapse equality, fixed: [mean, nu, xi], when: "k = nu*(n-k)", status: ScaleBoundary, message: "Tie equality scale-collapse boundary"}
  - model: snorm
    when: '$\nu \to \infty$'
    status: SnormBoundary
  - model: hnorm
    when: '$\nu \to \infty$ and $\xi \to \infty$'
    status: RightHalfNormalBoundary
  - name: reflected translated half-normal
    when: '$\nu \to \infty$ and $\xi \to 0$'
    status: LeftHalfNormalBoundary
  - name: translated half-t
    when: '$\xi \to \infty$, endpoint at the sample minimum'
    status: RightHalfTBoundary
  - name: reflected translated half-t
    when: '$\xi \to 0$, endpoint at the sample maximum'
    status: LeftHalfTBoundary
  - name: two-piece t with 2 degrees of freedom
    when: '$\nu \to 2^+$'
    status: DfTwoBoundary
references:
  - "Fernandez, C. and Steel, M. F. J. (1998). On Bayesian modeling of fat tails and skewness. Journal of the American Statistical Association, 93(441), 359-371."
examples: |
  # Daily DAX log returns, scaled to percent.
  mlsstd(100 * diff(log(EuStockMarkets[, "DAX"])))
holds:
- {held: [mean], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [sd], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [nu], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [xi], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [mean, sd], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [mean, nu], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [mean, xi], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [sd, nu], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [sd, xi], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [nu, xi], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [mean, sd, nu], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [mean, sd, xi], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
---

The skew Student-t distribution combines heavy tails with asymmetry using the Fernandez-Steel construction. Its location and scale parameters are the mean and standard deviation.

## Details

A tie of multiplicity $k>2(n-k)$ makes the free-df likelihood unbounded even at fixed skewness (the two side scales change the coefficient, not the collapse exponent); equality is reported as an uncertified scale-collapse boundary, and a half-t face has a positive scale root only when $\nu(n-k)>k$.

This is the fGarch parameterisation: `mean` and `sd` are the mean and standard deviation, `nu` is greater than 2, and `xi = 1` gives the Student-t distribution.

The maximum likelihood estimate often does not exist, especially in small samples: the likelihood is then maximised on the boundary of the parameter space, and `mlsstd` stops with a message naming the limit. The fitter compares the best of several interior fits with the skew-normal limit ($\nu \to \infty$, fitted exactly), the translated half-t limits ($\xi \to \infty$ or $0$, with the endpoint at the sample minimum or maximum and the degrees of freedom searched over $[2, \infty]$), and the two-piece t with 2 degrees of freedom ($\nu \to 2$), which has infinite variance and no finite `sd`. A limit is reported when it is at least as good as the best interior fit, up to $10^{-8}$ per observation. This is a multistart comparison, not a certificate of the global maximum. In simulations about three in four samples of size 20 from a skew-t with 5 or 10 degrees of freedom have no interior maximum.


Holding `nu` and `xi` together with `sd` supports location estimation;
holding them together with `mean` supports scale estimation. All coefficients
may also be held for likelihood evaluation. The remaining partial holds
retain explicit implementation exceptions because their existence criteria
were unresolved in P0. Conditional searches scan and refine a one-dimensional
likelihood, including density kinks in location; numerical comparisons do
not certify global optimality.

For scale estimation with fixed mean and shapes, let k observations equal
the held mean. The likelihood is unbounded at scale zero when
$k>\nu(n-k)$. Equality is reported as an uncertified scale-collapse
boundary; with strict inequality reversed the likelihood vanishes at both
scale endpoints. With scale held, location maxima exist even with ties.
