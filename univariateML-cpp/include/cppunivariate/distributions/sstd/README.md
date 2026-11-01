---
id: sstd
inference_numeric:
  - regime: "symmetric member with skew held"
    available: true
    condition: "finite mean, sd > 0 and nu > 2; xi = 1 held; any other subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: unreviewed or unavailable
    available: false
    condition: "joint cusp-moving regime with xi != 1"
    reason: "the two-piece log density has a moving second-derivative kink"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Skew Student-t
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
  role: df
  domain: (2, inf)
  simulate: 5
  kind: regular
- name: xi
  symbol: '\xi'
  role: skew
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
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
  - {model: hnorm, name: right translated half-normal, fixed: [mean], when: "nu and xi tend to their half-normal corner with held moments retained", status: RightHalfNormalBoundary}
  - {model: hnorm, name: left translated half-normal, fixed: [mean], when: "nu and xi tend to their half-normal corner with held moments retained", status: LeftHalfNormalBoundary}
  - {model: hnorm, name: right translated half-normal, fixed: [sd], when: "nu and xi tend to their half-normal corner with held moments retained", status: RightHalfNormalBoundary}
  - {model: hnorm, name: left translated half-normal, fixed: [sd], when: "nu and xi tend to their half-normal corner with held moments retained", status: LeftHalfNormalBoundary}
  - {model: hnorm, name: right translated half-normal, fixed: [mean, sd], when: "nu and xi tend to their half-normal corner with held moments retained", status: RightHalfNormalBoundary}
  - {model: hnorm, name: left translated half-normal, fixed: [mean, sd], when: "nu and xi tend to their half-normal corner with held moments retained", status: LeftHalfNormalBoundary}
  - {name: tie collapse, fixed: [mean], when: "An achievable tie has k > nu*(n-k), using nu=2 when shape is free", status: Unbounded}
  - {name: uncertified tie equality, fixed: [mean], when: "An achievable tie has k = nu*(n-k), using nu=2 when shape is free", status: ScaleBoundary}
  - {model: snorm, fixed: [mean], when: "nu tends to infinity with the held moment constraints", status: SnormBoundary}
  - {name: two-piece t with 2 degrees of freedom, fixed: [mean], when: "nu tends to 2 with ordinary t scale finite and held mean retained", status: DfTwoBoundary}
  - {model: ht, name: right translated half-family, fixed: [mean], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfTBoundary}
  - {model: ht, name: left translated half-family, fixed: [mean], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfTBoundary}
  - {name: tie collapse, fixed: [sd], when: "An achievable tie has k > nu*(n-k), using nu=2 when shape is free", status: Unbounded}
  - {name: uncertified tie equality, fixed: [sd], when: "An achievable tie has k = nu*(n-k), using nu=2 when shape is free", status: ScaleBoundary}
  - {model: snorm, fixed: [sd], when: "nu tends to infinity with the held moment constraints", status: SnormBoundary}
  - {model: ht, name: right translated half-family, fixed: [sd], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfTBoundary}
  - {model: ht, name: left translated half-family, fixed: [sd], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfTBoundary}
  - {name: tie collapse, fixed: [nu], when: "An achievable tie has k > nu*(n-k), using nu=2 when shape is free", status: Unbounded}
  - {name: uncertified tie equality, fixed: [nu], when: "An achievable tie has k = nu*(n-k), using nu=2 when shape is free", status: ScaleBoundary}
  - {model: ht, name: right translated half-family, fixed: [nu], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfTBoundary}
  - {model: ht, name: left translated half-family, fixed: [nu], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfTBoundary}
  - {name: tie collapse, fixed: [xi], when: "An achievable tie has k > nu*(n-k), using nu=2 when shape is free", status: Unbounded}
  - {name: uncertified tie equality, fixed: [xi], when: "An achievable tie has k = nu*(n-k), using nu=2 when shape is free", status: ScaleBoundary}
  - {model: snorm, fixed: [xi], when: "nu tends to infinity with the held moment constraints", status: SnormBoundary}
  - {name: two-piece t with 2 degrees of freedom, fixed: [xi], when: "nu tends to 2 with ordinary t scale finite and held mean retained", status: DfTwoBoundary}
  - {name: tie collapse, fixed: [mean, sd], when: "An achievable tie has k > nu*(n-k), using nu=2 when shape is free", status: Unbounded}
  - {name: uncertified tie equality, fixed: [mean, sd], when: "An achievable tie has k = nu*(n-k), using nu=2 when shape is free", status: ScaleBoundary}
  - {model: snorm, fixed: [mean, sd], when: "nu tends to infinity with the held moment constraints", status: SnormBoundary}
  - {model: ht, name: right translated half-family, fixed: [mean, sd], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfTBoundary}
  - {model: ht, name: left translated half-family, fixed: [mean, sd], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfTBoundary}
  - {name: tie collapse, fixed: [mean, nu], when: "An achievable tie has k > nu*(n-k), using nu=2 when shape is free", status: Unbounded}
  - {name: uncertified tie equality, fixed: [mean, nu], when: "An achievable tie has k = nu*(n-k), using nu=2 when shape is free", status: ScaleBoundary}
  - {model: ht, name: right translated half-family, fixed: [mean, nu], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfTBoundary}
  - {model: ht, name: left translated half-family, fixed: [mean, nu], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfTBoundary}
  - {name: tie collapse, fixed: [mean, xi], when: "An achievable tie has k > nu*(n-k), using nu=2 when shape is free", status: Unbounded}
  - {name: uncertified tie equality, fixed: [mean, xi], when: "An achievable tie has k = nu*(n-k), using nu=2 when shape is free", status: ScaleBoundary}
  - {model: snorm, fixed: [mean, xi], when: "nu tends to infinity with the held moment constraints", status: SnormBoundary}
  - {name: two-piece t with 2 degrees of freedom, fixed: [mean, xi], when: "nu tends to 2 with ordinary t scale finite and held mean retained", status: DfTwoBoundary}
  - {model: ht, name: right translated half-family, fixed: [sd, nu], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfTBoundary}
  - {model: ht, name: left translated half-family, fixed: [sd, nu], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfTBoundary}
  - {name: tie collapse, fixed: [sd, xi], when: "An achievable tie has k > nu*(n-k), using nu=2 when shape is free", status: Unbounded}
  - {name: uncertified tie equality, fixed: [sd, xi], when: "An achievable tie has k = nu*(n-k), using nu=2 when shape is free", status: ScaleBoundary}
  - {model: snorm, fixed: [sd, xi], when: "nu tends to infinity with the held moment constraints", status: SnormBoundary}
  - {name: tie collapse, fixed: [nu, xi], when: "An achievable tie has k > nu*(n-k), using nu=2 when shape is free", status: Unbounded}
  - {name: uncertified tie equality, fixed: [nu, xi], when: "An achievable tie has k = nu*(n-k), using nu=2 when shape is free", status: ScaleBoundary}
  - {model: ht, name: right translated half-family, fixed: [mean, sd, nu], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfTBoundary}
  - {model: ht, name: left translated half-family, fixed: [mean, sd, nu], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfTBoundary}
  - {name: tie collapse, fixed: [mean, sd, xi], when: "An achievable tie has k > nu*(n-k), using nu=2 when shape is free", status: Unbounded}
  - {name: uncertified tie equality, fixed: [mean, sd, xi], when: "An achievable tie has k = nu*(n-k), using nu=2 when shape is free", status: ScaleBoundary}
  - {model: snorm, fixed: [mean, sd, xi], when: "nu tends to infinity with the held moment constraints", status: SnormBoundary}
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
  - "Fernandez, C. and Steel, M. F. J. (1998). On Bayesian modeling of fat tails and skewness. Journal of the American Statistical Association, 93(441), 359-371. doi:10.1080/01621459.1998.10474117."
examples: |
  x <- diff(log(datasets::EuStockMarkets[, "DAX"]))
  mlsstd(x)

summary: The moment-standardized Fernandez–Steel skew Student distribution allows asymmetry and heavy tails.
classification: specialist
classification_reason: Used to model financial returns requiring both asymmetry and polynomial tails.
aliases:
- "Fernández–Steel skew Student"
key_references:
- 1

group: skewed and robust location-scale
construction:
- skewing
- location-scale
relations: {}
fit:
  method: multistart
  certified: 'no'
  note: 'sstd/README.md, Details; sstd/mle.hpp (public fitter and held overloads); sstd/test.cpp: Default and nontrivial
    conditional fits use multistart, finite grids or local profile refinement without a global certificate; all-held
    density evaluation is not counted as certification.'
areas:
- finance
- measurement and errors
---

The skew Student-t distribution combines heavy tails with asymmetry using the Fernandez-Steel construction. Its location and scale parameters are the mean and standard deviation.

## Details

The 2026-10-31 fit-speed revision stops the fixed-shape scale bisection when its
midpoint rounds to an endpoint: the remaining iterations cannot change the
represented final midpoint or scale. The shared held-fit helper retains the same
grid and all refinements. In held location/scale fits with fixed shape and skew,
density normalizers are computed once while per-observation arithmetic grouping
is preserved. See the [paired evidence and rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

A tie of multiplicity $k>2(n-k)$ makes the free-df likelihood unbounded even at fixed skewness (the two side scales change the coefficient, not the collapse exponent); equality is reported as an uncertified scale-collapse boundary, and a half-t face has a positive scale root only when $\nu(n-k)>k$.

This is the fGarch parameterisation: `mean` and `sd` are the mean and standard deviation, `nu` is greater than 2, and `xi = 1` gives the Student-t distribution.

The maximum likelihood estimate often does not exist, especially in small samples: the likelihood is then maximised on the boundary of the parameter space, and `mlsstd` stops with a message naming the limit. The fitter compares the best of several interior fits with the skew-normal limit ($\nu \to \infty$, fitted exactly), the translated half-t limits ($\xi \to \infty$ or $0$, with the endpoint at the sample minimum or maximum and the degrees of freedom searched over $[2, \infty]$), and the two-piece t with 2 degrees of freedom ($\nu \to 2$), which has infinite variance and no finite `sd`. A limit is reported when it is at least as good as the best interior fit, up to $10^{-8}$ per observation. This is a multistart comparison, not a certificate of the global maximum. In simulations about three in four samples of size 20 from a skew-t with 5 or 10 degrees of freedom have no interior maximum.


Every coefficient may be held, singly or jointly; held values are returned
exactly. Conditional fits compare multistart interior searches with every
reachable half-family and shape boundary, retaining the held mean and sd
constraints on those faces. The fixed-shape, fixed-skew location-scale fit
profiles scale and scans location, including observation kinks. Scalar
searches scan before refining competing brackets. Numerical multistart and
finite scans provide evidence rather than a global optimality certificate.
A search reaching its numerical guard stops instead of returning a fit.

For scale estimation with fixed mean and shapes, let k observations equal
the held mean. The likelihood is unbounded at scale zero when
$k>\nu(n-k)$. Equality is reported as an uncertified scale-collapse
boundary; with strict inequality reversed the likelihood vanishes at both
scale endpoints. With scale and both shapes held, location maxima exist even with ties.

Holding sd does not prevent collapse when nu is free: as nu tends to 2,
the effective ordinary-t scale vanishes even at fixed sd. The same tie
threshold applies, at the held mean when mean is held and at any sample
tie otherwise. Holding both sd and nu prevents that collapse.

## d/p/q/r

Uses the fGarch mean and standard deviation parameterisation.

### Adapter independence (2026-10-30)

The unconditional BFGS failure rescue now lives in `sstd_mle`: accept only when the Euclidean norm of the finite-difference total score in (mu, log(sigma), log(nu-2), log(xi)) coordinates is below 1e-3. Conditional failures remain failures. This preserves the former adapter gate; it is a numerical stationarity check, not a global maximum proof.
