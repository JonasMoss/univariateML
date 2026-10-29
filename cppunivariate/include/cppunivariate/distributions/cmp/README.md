---
id: cmp
name: Conway--Maxwell--Poisson
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: lambda
  role: rate
  domain: '[0, inf)'
  simulate: 4
  kind: regular
- name: nu
  role: dispersion
  domain: '[0, inf)'
  simulate: 1.5
  kind: regular
r:
  density: dcmp
  sampler: rcmp
  control:
    lambda: "Optional fixed nonnegative finite rate; NULL estimates it."
    nu: "Optional fixed nonnegative finite dispersion; NULL estimates it. At zero lambda must be below one."
    reltol: "Tolerance on moment residuals."
    iterlim: "Maximum number of moment-solving iterations."
log_density: "x*log(lambda)-nu*loggamma(x+1)-log(Sum(lambda**k/factorial(k)**nu,(k,0,oo)))"
limits:
  - name: adjacent two-point law
    when: '$\nu \to \infty$ with the rate adjusted'
    detect: adjacent observations in a joint fit; observations in {0,1} with fixed lambda
    status: TwoPointLimit
  - name: point mass
    when: 'parameters diverge along an exposing direction'
    detect: constant positive observations in a joint fit
    status: PointMassLimit
examples: |
  mlcmp(c(0,1,1,2,2,2,3,3,4,5))
holds: []
---

The Conway--Maxwell--Poisson distribution extends Poisson counts with a dispersion coordinate.

## Details

The rate is generally not the mean. Dispersion one is Poisson; dispersion zero is geometric when the rate is below one. Either or both coordinates may be fixed. Rate zero is the attained zero point mass; its unidentified free dispersion is represented by one. Dispersion zero is attained when lambda is below one; the joint domain requires lambda < 1 whenever nu = 0. Adjacent two-point and positive point-mass escapes still stop with a named limit.

Probability evaluation uses the certified CMP normalizer, stable centered log weights and direct tail summation. A calculation exceeding the normalizer or tail work budget returns NaN with a warning. Quantile search is limited to counts below 9e15. Random generation uses inversion with R's RNG.
