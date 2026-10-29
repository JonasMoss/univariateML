---
id: frechet
name: Frechet
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: lambda, role: shape, domain: "[1, inf)", default: 2}
  - {name: mu, role: location, domain: "(-inf, inf)", default: 0, fixable: true}
  - {name: sigma, role: scale, domain: "(0, inf)", default: 1}
r:
  density: extraDistr::dfrechet
  sampler: extraDistr::rfrechet
  control:
    reltol: Profile score and root bracket tolerance.
    iterlim: Maximum iterations per scalar solve.
log_density: log(lambda)-log(sigma)-(lambda+1)*log((x-mu)/sigma)-((x-mu)/sigma)**(-lambda)
limits:
  - {model: gumbel, when: 'lambda tends to infinity with free location', status: GumbelLimit}
examples: |
  set.seed(3)
  mlfrechet(extraDistr::rfrechet(100), mu = 0)
---

The Frechet distribution models heavy right tails and extreme maxima.

## Details

A held-location fit requires at least two finite, nonconstant observations strictly above mu. Its identification and existence are exactly those of inverse Weibull after subtracting mu.

The shape domain in the parameter table applies to free location.
With location held, inverse Weibull fitting on x-mu permits every positive
lambda. With location free, lambda is restricted to [1, infinity), and
the attained lambda=1 member is returned. The free fit requires at least
three finite observations and every tie multiplicity strictly below half
the sample size, as for GEV. The Gumbel face wins ties within 1e-8*n.

For a minimum m repeated k times, set mu=m-epsilon and sigma=c*epsilon.
The minimum densities are proportional to epsilon^(-1), and all other
densities to epsilon^lambda. Thus likelihood diverges when
lambda*(n-k)-k<0, including every positive lambda for constant samples.
The tie restriction and lambda>=1 exclude this collapse, following the
GEV restriction xi=1/lambda<=1. These restrictions are part of the fitted
model. The GEV mapping is location=mu+sigma, scale=sigma/lambda and
shape=1/lambda. The profile scans score roots and compares both faces;
global root discovery is checked numerically rather than proved.
