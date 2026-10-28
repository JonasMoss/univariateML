---
id: lst
name: Location-scale Student-t
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mu, role: location, domain: "(-inf, inf)", default: 0.0}
  - {name: sigma, role: scale, domain: "(0, inf)", default: 1.0}
  - {name: df, role: df, domain: "[1, inf)", default: 3.0}
r:
  density: extraDistr::dlst
  sampler: extraDistr::rlst
  control:
    gradtol: Tolerance on the analytic gradient norm of the total negative log-likelihood in standardized location, log scale and log(df - 1) coordinates; fixed step and objective backstops also apply.
    iterlim: Maximum iterations for each EM start and each BFGS refinement.
log_density: loggamma((df+1)/2) - loggamma(df/2) - log(df*pi)/2 - log(sigma) - (df+1)*log(1+((x-mu)/sigma)**2/df)/2
limits:
  - model: norm
    when: '$df \to \infty$'
    status: NormalBoundary
examples: |
  set.seed(3)
  mllst(extraDistr::rlst(200, df = 3, mu = 2, sigma = 1))
---

The location-scale Student t includes heavy-tailed distributions with infinite variance.

## Details

`sigma` is a scale, not a standard deviation. Fitting restricts `df` to be at least 1 and includes the attained Cauchy member at `df = 1`. Unrestricted positive degrees of freedom allow unbounded likelihood through scale collapse. The fitter requires at least three observations and rejects samples in which any value occurs at least half the time, including equality.

A multistart profile grid and joint refinement compare finite fits with Cauchy and normal fits. The joint problem need not be unimodal; the search does not certify a global maximum. Log-likelihood differences within `1e-8` per observation are treated as ties, preferring Cauchy and then other attained finite members. On the unresolved normal approach (`df >= 1e5`) a numerical tie selects the normal boundary.
