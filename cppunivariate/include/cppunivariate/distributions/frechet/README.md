---
id: frechet
name: Frechet
support: {type: real, lower: "mu", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "-Inf"}}
params:
- name: lambda
  role: shape
  domain: '(0, inf)'
  simulate: 1
  fit_domain:
    lower: 1.0
    restriction:
      mechanism: collapse
      exempt_when_held:
      - - mu
      reason: lambda >= 1 prevents free-threshold collapse; held mu is inverse Weibull
        on x - mu.
  kind: regular
- name: mu
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: threshold
  advice: Holding mu fits inverse Weibull on x - mu; NULL estimates mu with lambda
    >= 1.
- name: sigma
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dfrechet
  sampler: rfrechet
  control:
    mu: Threshold to hold fixed; NULL estimates it.
    lambda: Positive shape to hold fixed, or NULL to estimate.
    sigma: Positive scale to hold fixed, or NULL to estimate.
    reltol: Profile score and root bracket tolerance.
    iterlim: Maximum iterations per scalar solve.
log_density: log(lambda)-log(sigma)-(lambda+1)*log((x-mu)/sigma)-((x-mu)/sigma)**(-lambda)
limits:
  - {name: fixed-shape collapse, fixed: [lambda], status: InvalidData, message: "unattained collapse face", when: "With free mu and sigma, lambda*(n-k) <= k at minimum multiplicity k."}
  - {name: shape divergence, fixed: [sigma], status: InvalidData, message: "unbounded shape likelihood", when: "Constant data with free mu, or all x-mu equal held sigma."}
  - {model: gumbel, when: 'lambda tends to infinity with free location', status: GumbelLimit}
  - {name: free collapse, status: NoMLE, message: "minimum multiplicity permits scale collapse", when: "The sample minimum's multiplicity equals n/2 exactly (even n only) and no interior (mu, sigma, lambda) point beats the lambda -> 1 collapse face's limiting likelihood."}
fit_args:
  mu: ~
examples: |
  set.seed(1)
  x <- rml(200, mlfrechet())
  mlfrechet(x, mu = NULL)
---

The Frechet distribution models heavy right tails and extreme maxima.

## Details

When the location is free, the minimum multiplicity controls the positive-shape collapse; ties above the minimum do not cause it.

With shape and scale both free, a held-location fit requires at least two finite, nonconstant observations strictly above mu. Its identification and existence are exactly those of inverse Weibull after subtracting mu.

The shape restriction in the parameter table applies when shape is free
and location is free.
With location held, inverse Weibull fitting on x-mu permits every positive
lambda. With location and shape free, lambda is restricted to [1, infinity), and
the attained lambda=1 member is returned. The free fit requires at least
three finite observations. Minimum tie multiplicity strictly above half
the sample size always diverges, as for GEV; at exactly half (even n
only), the lambda -> 1 collapse face has a finite but unattained
supremum, and the fit compares the best interior candidate against it,
returning the interior fit when strictly higher or stopping with the
same collapse reason otherwise. As for GEV, no interior candidate ever
beat the limit in extensive testing at an exact half tie; no proof that
this holds for every sample is claimed. The Gumbel face wins ties within 1e-8*n.

For a minimum m repeated k times, set mu=m-epsilon and sigma=c*epsilon.
The minimum densities are proportional to epsilon^(-1), and all other
densities to epsilon^lambda. Thus likelihood diverges when
lambda*(n-k)-k<0, including every positive lambda for constant samples.
The tie restriction and lambda>=1 exclude this collapse, following the
GEV restriction xi=1/lambda<=1. These restrictions are part of the fitted
model. The GEV mapping is location=mu+sigma, scale=sigma/lambda and
shape=1/lambda. The profile scans score roots and compares both faces;
global root discovery is checked numerically rather than proved.

All hold combinations are supported. With lambda and mu held, the exact
scale is (n / sum((x-mu)^(-lambda)))^(1/lambda). With mu and sigma held,
the shape likelihood is strictly concave and its score is solved by
bisection; if every x-mu equals sigma it is unbounded. These cases allow
one observation and constants unless the stated shape divergence applies.
With lambda and sigma held, the scalar location fit permits every positive
lambda, including lambda below one. With only lambda held, a minimum of
multiplicity k requires lambda*(n-k)>k; equality is an unattained collapse
face. With only sigma held, free lambda is restricted to lambda>=1 and
constant samples have an unbounded shape likelihood. The location search
profiles the exact scale or concave shape solve and refines sampled local
maxima. Global search coverage is numerical; the P0 proof gaps remain.

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
optimizers return a failure status unless the score norm in fitting
coordinates is below `1e-3`; unresolved scalar searches stop.
