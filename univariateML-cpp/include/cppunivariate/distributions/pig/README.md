---
id: pig
name: Poisson-inverse Gaussian
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: mean
  symbol: '\mu'
  role: mean
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape
  symbol: '\lambda'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dpig
  sampler: rpig
  control:
    reltol: "Absolute tolerance on the log parameter in the scalar search."
    iterlim: "Maximum refinement iterations per bracketed scalar peak."
log_density: "log(2*shape/pi)/2 - loggamma(x+1) + shape/mean + (x-1/2)*log(shape/(2+shape/mean**2))/2 + log(besselk(x-1/2, sqrt(shape*(2+shape/mean**2))))"
limits:
  - model: pois
    when: '$\mathrm{shape} \to \infty$ at finite mean'
    detect: Poisson maximised likelihood wins, including ties within 1e-8 times sample size
    status: PoissonLimit
  - model: pois
    when: 'mean tends to zero with positive shape held'
    detect: all observations are zero, mean is free, and positive shape is held
    fixed: [shape]
    status: ZeroLimit
references:
- 'Willmot, G. E. (1987). The Poisson-inverse Gaussian distribution as an alternative to the negative binomial.
  Scandinavian Actuarial Journal, 1987(3-4), 113-127. DOI: 10.1080/03461238.1987.10413823.'
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlpig())
  mlpig(x)

group: Poisson, binomial and negative binomial
construction:
- compounding
relations:
  mixture_of:
  - family: pois
    at:
      lambda: mix
    mixed: lambda
    mixing:
      family: invgauss
      at:
        mean: mean
        shape: shape
    when: shape > 0
fit:
  method: multistart
  certified: 'no'
  note: 'pig/README.md, Details; pig/mle.hpp (public fitter and held overloads); pig/test.cpp: Default and nontrivial
    conditional fits use multistart, finite grids or local profile refinement without a global certificate; all-held
    density evaluation is not counted as certification.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model insurance claim frequencies with inverse-Gaussian heterogeneity in intensity.
summary: Poisson-inverse Gaussian provides a model for insurance claim frequencies with inverse-Gaussian heterogeneity
  in intensity.
aliases:
- "PIG"
key_references:
- 2
---

The Poisson-inverse Gaussian distribution mixes a Poisson count with an inverse Gaussian mean. It models overdispersed nonnegative counts.

## Details

Either `mean` or `shape`, or both, can be held fixed. With both free the fitted mean is exactly the sample mean. The fitter searches all bracketed peaks on a broad log-parameter grid and compares the Poisson likelihood; it does not assume a unique peak. A search reaching its range boundary without a winning Poisson face reports failure.

With both parameters free and a positive sample mean, a population-form sample variance no greater than the mean gives the Poisson supremum, including equality. Strict overdispersion guarantees a finite maximum; a gain smaller than the numerical tie tolerance still selects Poisson.

Shape zero is the attained zero point mass. All-zero data with free shape return shape zero, preserving held mean or choosing mean one when unidentified. With positive shape held and mean free, all-zero data still have an unattained mean-zero supremum. With both parameters fixed the likelihood is evaluated directly. The infinite-mean Poisson-Levy face is strictly dominated for every finite sample when mean is free, including with shape fixed.
