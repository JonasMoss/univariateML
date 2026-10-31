---
id: cmp
name: Conway--Maxwell--Poisson
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: nu
  symbol: '\nu'
  role: dispersion
  domain: '[0, inf)'
  simulate: 1
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
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlcmp())
  mlcmp(x)

group: Poisson, binomial and negative binomial
construction:
- base
- generalisation
relations: {}
fit:
  method: multistart
  certified: unknown
  note: Primary route from the 2026-10-30 fitter inventory; guarantees across held subsets are not classified.
areas:
- counts and ecology
classification: specialist
classification_reason: Used within counts and ecology; classified by its specialised modelling role.
---

The Conway--Maxwell--Poisson distribution extends Poisson counts with a dispersion coordinate.

## Details

2026-10-30 (TASK-125): moment sums use compensated accumulation, the saddle-point quotient retains its division residual, and geometric tail bounds can be evaluated in log coordinates. For mode zero or one, the second-count weight uses `exp((2-mode)*eta)*exp2(-nu)`, avoiding amplification of rounding in `log(2)`. The 73-row independent reference grid measures errors; the truncation certificate excludes floating-point roundoff.

The rate is generally not the mean. Dispersion one is Poisson; dispersion zero is geometric when the rate is below one. Either or both coordinates may be fixed. Rate zero is the attained zero point mass; its unidentified free dispersion is represented by one. Dispersion zero is attained when lambda is below one; the joint domain requires lambda < 1 whenever nu = 0. Adjacent two-point and positive point-mass escapes still stop with a named limit.

Probability evaluation uses the certified CMP normalizer, stable centered log weights and direct tail summation. A calculation exceeding the normalizer or tail work budget returns NaN with a warning. Quantile search is limited to counts below 9e15. Random generation uses inversion with R's RNG.

## d/p/q/r

The Conway–Maxwell–Poisson normalizer and both tails are summed directly.
